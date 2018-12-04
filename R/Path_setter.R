# GEMINI path setting

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

#Check SavaData.R file exist
check_savedata <- function(){
    if(file.exists("SaveData.R")==F){
        stop("SavaData.R file doesn't exist. Please check R files.")
    }
}

#Using rstudio api. Unless use rstudio, it may set wrong path.
if(rstudioapi::isAvailable()){
    message("Automatically path setting...")
    setwd(dirname(rstudioapi::getSourceEditorContext()$path))
    print(getwd())
    if(file.exists(c("Connect_DB.R")) == F){
        path_setter()
    }
    message("Path set.")
}else{
    path_setter()
}
