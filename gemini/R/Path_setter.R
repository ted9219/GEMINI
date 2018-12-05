#' GEMINI path setting
#'
#' Path setting
#' @keywords gemini
#' @export
#' @example
#' path_set()

#Using rstudio api. Unless use rstudio, it may set wrong path.
path_set<- function(){
    if(rstudioapi::isAvailable()){
        message("Automatically path setting...")
        tryCatch({
        setwd(dirname(rstudioapi::getSourceEditorContext()$path))
        },error=function(e){
            path_setter()
        })
        print(getwd())
        if(file.exists(c("Connect_DB.R")) == F){
            path_setter()
        }
        message("Path set.")
    }else{
        path_setter()
    }
}

#Find Connect_DB.R, SaveData.R file for extract data from CDM
path_setter <- function(){
    if(file.exists(c("Connect_DB.R")) == T){
        check_savedata()
        print(getwd())
        message("Path set.")
    }else{
        path_set <- readline('Wrong path, set R folder path :')
        setwd(path_set)
        if(file.exists(c("Connect_DB.R")) == T){
            check_savedata()
            print(getwd())
            message("Path set.")
        }else{
            path_setter()
        }
    }
}
