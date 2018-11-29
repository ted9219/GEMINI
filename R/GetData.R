################################################################################
# READ DATA RESOURCE
# In RDS Folder, there shouldn't exist another rds files.
################################################################################
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# assign value function
set_val <<- function(name_list) {
  for (i in 1:length(name_list)) {
    temp <- paste0("std_", name_list[i])
    assign(gsub(temp, pattern = ".rds", replacement = ""), readRDS(paste0("../Standard RDS/", name_list[i])), envir = .GlobalEnv)
    temp <- gsub(temp, pattern = "std_", replacement = "tar_")
    assign(gsub(temp, pattern = ".rds", replacement = ""), readRDS(paste0("../Target RDS/", name_list[i])), envir = .GlobalEnv)
  }
}
################################################################################
# CDM valuable name list
################################################################################
# Read data list
# if Save changed, this lit should change
str_name_list <- list.files(paste0(dirname(getwd()), "/Standard RDS/"), pattern = "*.rds")
tar_name_list <- list.files(paste0(dirname(getwd()), "/Target RDS/"), pattern = "*.rds")
################################################################################
# Read Data
################################################################################
set_val(str_name_list)
