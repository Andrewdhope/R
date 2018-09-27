# ---------- ---------- ----------
#  Establishing ODBC Connections
# ---------- ---------- ----------
# 
# This script features several functions
# to establish and configure ODBC connecitons,
# and to execute SQL queries.
# 
# The packages require an existing Data Source Name (DSN) for the OBDC connection.
# The DSN is established on the client machine.
# Setup and maintain the DSN here: %windir%\system32\odbcad32.exe
# (Control Panel > Administrative Tools > ODBC Data Sources).
# -- Bonus: you can and should set up the dsn with a default table to connect to, so the function below has the right starting point.
# 
# Package dependenies:
# - RODBC
# 
# Reference material:
# - RShowDoc("RODBC", package="RODBC") 
# 
# 
# /// --- RODBC functions --- ///
#
#
# NAME: demonstrateRODBC
# DESC: Prints out a demo of the core functions of the RODBC package.
#       This demo uses a dsn connected to an Epic dev environment.
# KEYS: odbc, sql, connection, query
# TODO: Convert the demo into encapsulated functions... but the encapsulated functions are just the RODBC package...
#       Generalize it away from Epic-specific databases. The demo can use Epic content but the vars should be passed in with a wrapper.
#       ...again, anything more general is just a call to RODBC functions. A demo is the most useful thing. Show the functions in action.
#
demonstrateRODBC <- function(conn = "", dsn = "2018-DEV-SQL") {
    library(RODBC)
    if (is.null(conn)) { # ISSUE. a closed connection will return false (its not null).
        # esatblish a connection
        # dsn defaults to 2018-DEV-SQL
        conn <- odbcConnect(dsn)
        closeWhenDone <- TRUE # only close the connection if it is created within the funciton.
    }
    else {closeWhenDone <- FALSE} # If the connection is provided as an input to demonstrateRODBC, leave it open.
    
    
    # summarize the table names in this database
    tables <- sqlTables(conn)
    
    cat(paste("\n","Get a list of tables in a database.", sep = "\n"))
    cat(paste("\n","sqlTables(conn)", sep = ""))
    cat(paste("\n","HEAD:", "\n", sep = ""))
    print(head(tables))
    cat(paste("\n","Number of tables (nrow): ", nrow(tables), "\n", sep = ""))
    
    
    # filter on table names using wildcards
    pat.filter <- sqlTables(conn, tableName = "PAT%")
    
    cat(paste("\n","Filter tables by table name.", "Use _ as a wildcard for a single character and  %  for more than one character.", sep = "\n"))
    cat(paste("\n","sqlTables(conn, tableName = \"PAT%\")", sep = ""))
    cat(paste("\n","HEAD:", "\n", sep = ""))
    print(head(pat.filter))
    
    
    # fetch a table
    fetch.specialties <- sqlFetch(conn, "ZC_SPECIALTY")
    
    cat(paste("\n","Retrieve a single table with the sqlFetch function. We'll get the list of specialities as an example.", sep = "\n"))
    cat(paste("\n","sqlFetch(conn, \"ZC_SPECIALTY\")", sep = ""))
    cat(paste("\n","HEAD:", "\n", sep = ""))
    print(head(fetch.specialties))
    
    
    # fetch and then fetch more
    fetch.more <- sqlFetch(conn, "ZC_SPECIALTY", max = 3)
    fetch.more<- sqlFetchMore(conn, max = 3)
    
    cat(paste("\n","If it has many rows it can be retrieved in sections.", sep = "\n"))
    cat(paste("\n","sqlFetchMore can automatically get the next m rows of the table.", sep = ""))
    cat(paste("\n","fetch <- sqlFetch(conn, \"ZC_SPECIALTY\", max = 3)", sep = ""))
    cat(paste("\n","fetch <- sqlFetchMore(conn, max = 3)", sep = ""))
    
    cat(paste("\n","HEAD:", "\n", sep = ""))
    print(fetch.more)
    
    
    # get columns and run a simple query
    table.columns <- sqlColumns(conn, "ZC_SPECIALTY")[3:4]
    simple.query <- sqlQuery(conn, "SELECT SPECIALTY_C, NAME FROM ZC_SPECIALTY WHERE SPECIALTY_C < 10 ORDER BY SPECIALTY_C") 
    
    cat(paste("\n","Get the column names for a table with sqlColumns.", sep = "\n"))
    cat(paste("\n","sqlColumns(conn, ZC_SPECIALTY)[3:4]", "\n", sep = ""))
    print(table.columns)
    
    cat(paste("\n","Run a standard query with sqlQuery.", sep = "\n"))
    cat(paste("\n","sqlQuery(conn, \"SELECT SPECIALTY_C, NAME FROM ZC_SPECIALTY WHERE SPECIALTY_C < 10 ORDER BY SPECIALTY_C\")", "\n", sep = ""))
    print(simple.query)
    
    
    # clean up connection
    if (closeWhenDone) {
        odbcClose(conn)
    }
}
