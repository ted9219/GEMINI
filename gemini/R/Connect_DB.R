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
    serverInfoFile <- file.path("../server_info.cfg")

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

    connection <<- DatabaseConnector::connect(connectionDetails)
    gemini::save_data()
}
