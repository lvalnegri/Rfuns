#' Read MySQL configuration file for "group" access
#'
#' @param x the name of the group under which credentials are stored
#'
#' @return None
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
read_mysql_cnf <- function(x){
    y <- readLines('/etc/mysql/my.cnf')
    for(idx in 1:length(y)){
        if(y[idx] == paste0('[', x, ']')){
          idx <- idx + 1
          yx <- character(0)
          while(grepl('=', y[idx])){
              yx[gsub('=.*| ', '', y[idx])] <- gsub('.*=| ', '', y[idx])
              idx <- idx + 1
          }
          return(yx)
        }
    }
}


#' Allows to connect to a MySQL dtabase server using the "group" credentials stored in the configuration file
#'
#' @param x the name of the group under which credentials are stored
#' @param dbn the optional name of the database to connect to, if it is not already present in the group credentials
#'
#' @return None
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @export
#' 
dd_db_connect <- function(x, dbn = NA){
    y <- read_mysql_cnf(x)
    if(is.null(y)) stop('Group not found!')
    if(is.na(y['database'])){
        if(is.na(dbn)){
            DBI::dbConnect(RMySQL::MySQL(), host = y['host'], user = y['user'], password = y['password'])
        } else {
            DBI::dbConnect(RMySQL::MySQL(), host = y['host'], user = y['user'], password = y['password'], dbname = dbn)
        }
    } else {
        DBI::dbConnect(RMySQL::MySQL(), host = y['host'], user = y['user'], password = y['password'], dbname = y['database'])
    }
}


#' Creates a MySQL database
#'
#' \code{create_db} creates a database in the local MySQL Server
#'
#' @param dbn the name of the database to be created
#'
#' @return None
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import RMySQL
#'
#' @export
#'
dd_create_db <- function(dbn){
    dbc <- dd_db_connect('devs')
    dbSendQuery(dbc, paste('DROP DATABASE IF EXISTS', dbn))
    dbSendQuery(dbc, paste('CREATE DATABASE', dbn))
    dbDisconnect(dbc)
}


#' Creates a MySQL table
#'
#' \code{create_dbtable} creates a table in a specified database in the local MySQL Server
#'
#'
#' @param tname the name of the table to be created
#' @param dbn the name of the database in which create the table
#' @param tdef  the definitions for all the columns in the table
#' @param dts   the optional dataset to be uploaded
#'
#' @return None
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import RMySQL
#'
#' @export
#'
dd_create_dbtable <- function(tname, dbn, tdef, dts = NULL){
    dbc <- dd_db_connect('devs', dbn)
    dbSendQuery(dbc, paste('DROP TABLE IF EXISTS', tname))
    strSQL <- paste(
        "CREATE TABLE", tname, 
        "(", 
            tdef,
        ") ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci ROW_FORMAT=FIXED"
    )
    dbSendQuery(dbc, strSQL)
    if(!is.null(dts)) dbWriteTable(dbc, tname, dts, row.names = FALSE, append = TRUE)
    dbDisconnect(dbc)
}


#' Perform an action on a local MySQL database
#'
#' \code{dd_dbm_do} allows to:
#'   - (r)ead a table
#'   - (w)rite a table
#'   - (q)uery a table
#'   - (s)end a query
#'
#' @param dbn  the name of the database in which perform the action the table
#' @param action the action to be applied, among r (default), w, q, and s
#' @param tname  the name of the table to be involved
#' @param dts    the dataset to be uploaded if table has to be (over)written
#' @param strSQL the sql statement to get or send
#' @param trunc  describe if the table has to be truncated before data being written
#' @param drop   describe if the table has to be dropped before data being written
#'
#' @return A data.table, if action is 'r' or 'q'. None, otherwise
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import data.table DBI RMySQL
#'
#' @export
#'
dd_dbm_do <- function(dbn, action = 'r', tname = NA, dts = NULL, strSQL = NA, trunc = TRUE, drop = FALSE){
    db_check <- FALSE
    tryCatch({
            dbc <- dd_db_connect('devs', dbn)
            db_check <- TRUE
        }, error = function(err) {
    })
    if(db_check){
        if(action == 'r') tflag <- dbExistsTable(dbc, tname)
        dbDisconnect(dbc)
    } else {
        stop('Can not connect to the specified database!')
    }
    switch(action,
        'w' = {
            if(is.na(tname)) stop('The table name is missing!')
            if(is.null(dts)) stop('The dataset is missing!')
            dbc <- dd_db_connect('devs', dbn)
            if(trunc) dbSendQuery(dbc, paste("TRUNCATE TABLE", tname))
            if(drop) dbSendQuery(dbc, paste("DROP TABLE IF EXISTS", tname))
            dbWriteTable(dbc, tname, dts, row.names = FALSE, append = TRUE)
        },
        'r' = {
            if(is.na(tname)) stop('The table name is missing!')
            if(!tflag) stop('The specified table does not exists!')
            dbc <- dd_db_connect('devs', dbn)
            y <- data.table( dbReadTable(dbc, tname) )
        },
        'q' = {
            dbc <- dd_db_connect('devs', dbn)
            y <- data.table( dbGetQuery(dbc, strSQL) )
        },
        's' = {
            dbc <- dd_db_connect('devs', dbn)
            dbSendQuery(dbc, strSQL)
        },
        message('The required action is not currently implemented')
    )
    dbDisconnect(dbc)
    if(action %in% c('r', 'q')) return(y)
}


#' Update an existing table in a local MySQL database based on a specified dataset
#'
#' @param dbn         the name of the database that include the table to be updated
#' @param dts           the dataset with the new values
#' @param update_sql    the sql query that specifies the updating
#' @param alter_sql     the optional action to be applied to the temp table before the updating (like setting a primary key or indices)
#'
#' @return None
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import DBI RMySQL
#'
#' @export
#'
dbm_update <- function(dbn, dts, update_sql, alter_sql = NA){
    dd_dbm_do(dbn, 's', strSQL = "DROP TABLE IF EXISTS temp")
    dd_dbm_do(dbn, 'w', 'temp', dts, trunc = FALSE)
    if(!is.na(alter_sql)) dd_dbm_do(dbn, 's', strSQL = alter_sql)
    dd_dbm_do(dbn, 's', strSQL = update_sql)
    dd_dbm_do(dbn, 's', strSQL = "DROP TABLE temp")
}

#' Allows to rename or copy a local MySQL database
#'
#' @param old_db The name of the existing database
#' @param new_db The name of the database to be the new container
#' @param create_new IF TRUE any existing database called old_db will be dropped and created as new
#' @param drop_old If TRUE (the default) the old database will be dropped.
#'                 When FALSE, the database will be kept with all its tablesall
#'
#' @return None
#'
#' @author Luca Valnegri, \email{l.valnegri@datamaps.co.uk}
#'
#' @import DBI RMySQL
#'
#' @export
#'
dd_rename_db <- function(old_db, new_db, create_new = TRUE, drop_old = TRUE){
    if(create_new) dd_create_db(new_db)
    tnames <- dd_dbm_do(old_db, 'q', strSQL = 'SHOW TABLES')
    for(idx in 1:nrow(tnames)){
        strSQL <-
            if(drop_old){
                paste0('RENAME TABLE ', old_db, '.', tnames[idx], ' TO ', new_db, '.', tnames[idx])
            } else {
                paste0('CREATE TABLE ', new_db, '.', tnames[idx], ' SELECT * FROM ', old_db, '.', tnames[idx])
            }
        dd_dbm_do('old_db', 's', strSQL = strSQL)
    }
    if(drop_old) dd_dbm_do(old_db, 's', strSQL = paste('DROP DATABASE', old_db))
}
