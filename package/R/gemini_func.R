infoFromFile <- function(file, pattern){
    gsub(paste0(pattern,"="),"",grep(paste0("^",pattern,"="), scan(file,what="",quiet=T,sep = "\n"),value=T))
}

set_val <<- function(name_list) {
    temp <- lapply(name_list, function(x) paste0("std_",x))
    lapply(name_list, function(x) assign(gsub(temp, pattern = ".rds", replacement = ""), readRDS(paste0("../Standard RDS/", x)), envir = .GlobalEnv))
    temp <- gsub(temp, pattern = "std_", replacement = "tar_")
    lapply(name_list, function(x) assign(gsub(temp, pattern = ".rds", replacement = ""), readRDS(paste0("../Target RDS/", x)), envir = .GlobalEnv))
}