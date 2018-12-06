#' connect_db
#'
#' This function for DB server connecting
#' @keywords gemini
#' @export
#' @example
#' connect_DB()

# install.packages("DatabaseConnector")
# library(DatabaseConnector)

# Set User Info to check authority
connect_DB <- function(){
    serverInfoFile <- file.path("server_info.cfg")

    infoFromFile <- function(file, pattern){
        gsub(paste0(pattern,"="),"",grep(paste0("^",pattern,"="), scan(file,what="",quiet=T,sep = "\n"),value=T))
    }
    cdmDatabaseSchema <<- infoFromFile(serverInfoFile,"schemaName")
    connectionDetails <- DatabaseConnector::createConnectionDetails(
        dbms = infoFromFile(serverInfoFile,"dbName"),
        server = infoFromFile(serverInfoFile,"server"),
        schema = cdmDatabaseSchema,
        user = infoFromFile(serverInfoFile,"user"),
        password = infoFromFile(serverInfoFile,"password")
    )

    tryCatch({
        connection <<- DatabaseConnector::connect(connectionDetails)
        message("Connection success!")},
        error = function(e){
            file.rename("server_info.cfg","server_info_failed.cfg")
            stop("Connection failed. Check server_info_failed.cfg file or create new file.")
    })
    gemini::save_data()
}
