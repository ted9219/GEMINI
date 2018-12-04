# DB server connecting

# install.packages("DatabaseConnector")
# library(DatabaseConnector)
infoFromFile <- function(file, pattern){
    gsub(paste0(pattern,"="),"",grep(paste0("^",pattern,"="), scan(file,what="",quiet=T,sep = "\n"),value=T))
}

if(file.exists("../server_info.cfg")){
    message("Try to connecting DB server...")
}else{
    db_name<-paste0("dbName=",readline("Set db name : "))
    server_ip<-paste0("server=",readline("Set server ip : "))
    schema_name<-paste0("schemaName=",readline("Set schema name : "))
    user_id<-paste0("user=",readline("Set db user id : "))
    pwd<-paste0("password=",readline("Set password : "))
    write(c(db_name,server_ip,schema_name,user_id,pwd),"../server_info.cfg")
    message("Try to connecting DB server...")
}

# Set User Info to check authority
serverInfoFile <- file.path("../server_info.cfg")
cdmDatabaseSchema <- infoFromFile(serverInfoFile,"schemaName")

connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = infoFromFile(serverInfoFile,"dbName"),
    server = infoFromFile(serverInfoFile,"server"),
    schema = cdmDatabaseSchema,
    user = infoFromFile(serverInfoFile,"user"),
    password = infoFromFile(serverInfoFile,"password")
)

connection <- DatabaseConnector::connect(connectionDetails)