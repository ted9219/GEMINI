#' connect_db
#'
#' This function for DB server connecting
#' @keywords gemini
#' @export

# install.packages("DatabaseConnector")
# library(DatabaseConnector)

# Set User Info to check authority
connect_DB <- function(previous = "server_info.cfg"){
    serverInfoFile <- file.path(previous)

    #Set server information to connecting
    cdmDatabaseSchema <<- infoFromFile(serverInfoFile,"schemaName")
    connectionDetails <- DatabaseConnector::createConnectionDetails(
        dbms = infoFromFile(serverInfoFile,"dbName"),
        server = infoFromFile(serverInfoFile,"server"),
        schema = cdmDatabaseSchema,
        user = infoFromFile(serverInfoFile,"user"),
        password = infoFromFile(serverInfoFile,"password")
    )

    #Server connect
    tryCatch({
        connection <<- DatabaseConnector::connect(connectionDetails)
        DatabaseConnector::dbIsValid(connection)
        },error = function(e){
            file.rename(previous,"server_info_failed.cfg")
            print(e)
            stop("Connection failed. Check server_info_failed.cfg file or create new file.")
    })

}

#Read information from cfg file
infoFromFile <- function(file, pattern){
    gsub(paste0(pattern,"="),"",grep(paste0("^",pattern,"="), scan(file,what="",quiet=T,sep = "\n"),value=T))
}
