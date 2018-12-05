#' Create rds file
#'
#' This function for creating server info files
#' @keywords gemini
#' @export
#' @example
#' create_rds()
create_rds<- function(){
    if(file.exists("../server_info.cfg")){
        message("Try to connecting DB server...")
        gemini::connect_DB()
    }else{
        db_name<-paste0("dbName=",readline("Set db name : "))
        server_ip<-paste0("server=",readline("Set server ip : "))
        schema_name<-paste0("schemaName=",readline("Set schema name : "))
        user_id<-paste0("user=",readline("Set db user id : "))
        pwd<-paste0("password=",readline("Set password : "))
        write(c(db_name,server_ip,schema_name,user_id,pwd),"../server_info.cfg")
        message("Try to connecting DB server...")
        gemini::connect_DB()
    }
}
