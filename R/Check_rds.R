# Check rds data files

str_name_list <- list.files(paste0(dirname(getwd()), "/Standard RDS/"), pattern = "*.rds$")
tar_name_list <- list.files(paste0(dirname(getwd()), "/Target RDS/"), pattern = "*.rds$")

name_list <- intersect(str_name_list,tar_name_list)
no_files <- setdiff(str_name_list,tar_name_list)
