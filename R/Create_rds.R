#' Create rds file
#'
#' This function for creating server info files and trigger of create rds files
#' @keywords gemini
#' @export
#'
create_rds<- function(){
    gemini::path_set()
    check.packages("DatabaseConnector")
    create_server_info <-function(){
        db_name<-paste0("dbName=",readline("Set db server name : "))
        server_ip<-paste0("server=",readline("Set server ip : "))
        schema_name<-paste0("schemaName=",readline("Set schema name : "))
        user_id<-paste0("user=",readline("Set db user id : "))
        pwd<-paste0("password=",readline("Set password : "))
        if(!grepl(x = schema_name, pattern = "*.dbo$")){
            schema_name <- paste0(schema_name,".dbo")
        }
        return(c(db_name,server_ip,schema_name,user_id,pwd))
    }

    infofile_rename <- function(){
        if(file.exists("server_info_old*.cfg$")){
            temp <- tail(list.files(pattern="server_info_old*.cfg$"))
            temp <- paste0("server_info_old",as.character(as.integer(gsub("*.cfg$","",gsub("server_info_old*","",temp)))+1),".cfg")
            return(temp)
        }
        else{
            return("server_info_old0.cfg")
        }
    }

    if(file.exists("server_info.cfg"))
        {
        recon <- readline("Do you want to connect previous server?(y/n) : ")
        if(recon == 'y'||recon == 'Y'){
            message("Try to connecting DB server...")
            gemini::connect_DB()
            }
        else if(recon == 'n'||recon == 'N'){
            file.rename("server_info.cfg",infofile_rename())
            write(create_server_info(),"server_info.cfg")
            message("Try to connecting DB server...")
            gemini::connect_DB()
            }

    }else{
        write(create_server_info(),"server_info.cfg")
        message("Try to connecting DB server...")
        gemini::connect_DB()
    }
}
