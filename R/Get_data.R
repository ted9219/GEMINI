# Read rds data

# assign value function
set_val <<- function(name_list) {
    lapply(name_list, function(x) assign(gsub(lapply(x, function(y) paste0("std_",y)), pattern = "*.rds$", replacement = ""), readRDS(paste0("../Standard RDS/", x)), envir = .GlobalEnv))
    lapply(name_list, function(x) assign(gsub(lapply(x, function(y) paste0("tar_",y)), pattern = "*.rds$", replacement = ""), readRDS(paste0("../Target RDS/", x)), envir = .GlobalEnv))
}

if(length(no_files) != 0&&length(name_list)!=0){
    set_val(name_list)
    lapply(no_files, function(x) assign(paste0("std_",gsub(x,pattern ="*.rds$",replacement = "")),value = readRDS(paste0("../Standard RDS/", x)),envir=.GlobalEnv))
    lapply(no_files, function(x) assign(paste0("tar_",gsub(x,pattern ="*.rds$",replacement = "")),value = NULL,envir=.GlobalEnv))
}else if(length(no_files) != 0&&length(name_list)==0){
    pms <- readline('No RDS file in Target RDS folder. Do you want to proceed(y / n)? ')
    if(pms == "y" || pms == "Y"){
        lapply(no_files, function(x) assign(paste0("std_",gsub(x,pattern ="*.rds$",replacement = "")),value = readRDS(paste0("../Standard RDS/", x)),envir=.GlobalEnv))
        lapply(no_files, function(x) assign(paste0("tar_",gsub(x,pattern ="*.rds$",replacement = "")),value = readRDS(paste0("../Standard RDS/", x)),envir=.GlobalEnv))
    }else if (pms == "n" || pms == "N"){
        message("Proceed is stopped.")
    }else{
        message("Wrong input. proceed is stopped.")
    }
}else if(length(no_files) == 0 && length(name_list)==0){
    message("No data exist. proceed is stopped.")
}else{
    set_val(name_list)
    print("All data exist.")
}
