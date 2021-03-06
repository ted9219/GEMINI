#' Save data
#'
#' This function for saving data by RDS file
#' @keywords gemini
#' @export
#'
################################################################################
# SAVE DATA RESOURCE
# Set location when project path is change
################################################################################

save_data <- function(){
    dir.create(file.path(getwd(), "Standard RDS"), showWarnings = FALSE)
    dir.create(file.path(getwd(), "Target RDS"), showWarnings = FALSE)
    gemini::Query_func()
################################################################################
# PERSON DATA SAVING FUNCTION
################################################################################
    cat("Person data extracting...\n")
    tm1 <- as.numeric(round(system.time(gemini::person_data())[3], digit = 1))
    rds_maker("person")
################################################################################
# DEATH DATA SAVING FUNCTION
################################################################################
    cat("Death data extracting...\n")
    tm2 <- as.numeric(round(system.time(gemini::death_data())[3], digit = 1))
    rds_maker("death")
################################################################################
# VISIT_OCCURRENCE DATA SAVING FUNCTION
################################################################################
    cat("Visit occurrence data extracting...\n")
    tm3 <- as.numeric(round(system.time(gemini::visit_occurrence_data())[3], digit = 1))
    rds_maker("visit")
################################################################################
# CONDITION_OCCURRENCE DATA SAVING FUNCTION
################################################################################
    cat("Condition occurrence data extracting...\n")
    tm4 <- as.numeric(round(system.time(gemini::condition_occurrence_data())[3], digit = 1))
    rds_maker("condition")
################################################################################
# DRUG_EXPOSURE DATA SAVING FUNCTION
################################################################################
    cat("Drug exposure data extracting...\n")
    tm5 <- as.numeric(round(system.time(gemini::drug_exposure_data())[3], digit = 1))
    rds_maker("drug_exposure")
################################################################################
# DRUG_ERA DATA SAVING FUNCTION
################################################################################
    cat("Drug era data extracting...\n")
    tm6 <- as.numeric(round(system.time(gemini::drug_era_data())[3], digit = 1))
    rds_maker("drug_era")

    tryCatch({
        zip(zipfile = paste0("Standard RDS/",cdmDatabaseSchema,".zip"),
            files = paste0("Standard RDS/",list.files("~/Standard RDS",pattern = "*.rds$")), flag= c("-j", "-r"))
    },
    error = function(e){
        cat("Files zip is failed. \n")
    },
    warning = function(w){
        message(w)
    })
    if(length(list.files(paste0(getwd(), "/Standard RDS/"), pattern = "*.zip$"))>0){
        file.remove(paste0("Standard RDS/",list.files(path = "Standard RDS/", pattern = ".rds$")))
    }
    # time check
    cat(paste0("RDS files created.\nThis process takes ", sum(tm1, tm2, tm3, tm4, tm5, tm6), "s.\n"))
################################################################################
# Disconnect DB
################################################################################
    DatabaseConnector::disconnect(connection)
}
rds_maker <- function(table_name){
    switch (table_name,
            "person" =  {
                persontbl_list <- list(
                    persontbl_record, persontbl_person_ratio, persontbl_gender, persontbl_min_age, persontbl_max_age, persontbl_race
                    , persontbl_ethnicity, persontbl_location, persontbl_provider, persontbl_care_site
                )
                persontbl_name_list <- c(
                    "persontbl_record", "persontbl_person_ratio", "persontbl_gender", "persontbl_min_age", "persontbl_max_age",
                    "persontbl_race", "persontbl_ethnicity", "persontbl_location", "persontbl_provider", "persontbl_care_site"
                )

                data_list <- persontbl_list
                rds_path <- sapply(FUN = paste0, "Standard RDS/", persontbl_name_list, ".rds")
            },
            "death" = {
                deathtbl_list <- list(deathtbl_check, deathtbl_type)
                deathtbl_name_list <- c("deathtbl_check", "deathtbl_type")

                data_list <- deathtbl_list
                rds_path <- sapply(FUN = paste0, "Standard RDS/", deathtbl_name_list, ".rds")
            },
            "visit" = {
                visittbl_list <- list(
                    visittbl_record, visittbl_person_ratio, visittbl_visit_concept, visittbl_start, visittbl_end, visittbl_diff_date
                    , visittbl_count, visittbl_type_concept, visittbl_care_site, visittbl_source_concept, visittbl_admitting_source,
                    visittbl_discharge, visittbl_preceding
                )
                visittbl_name_list <- list(
                    "visittbl_record", "visittbl_person_ratio", "visittbl_visit_concept", "visittbl_start", "visittbl_end",
                    "visittbl_diff_date", "visittbl_count", "visittbl_type_concept", "visittbl_care_site", "visittbl_source_concept",
                    "visittbl_admitting_source", "visittbl_discharge", "visittbl_preceding"
                )

                data_list <- visittbl_list
                rds_path <- sapply(FUN = paste0, "Standard RDS/", visittbl_name_list, ".rds")
            },
            "condition" = {
                conditiontbl_list <- list(
                    conditiontbl_record, conditiontbl_person_ratio, conditiontbl_diff_date, conditiontbl_start, conditiontbl_end,
                    conditiontbl_type_concept, conditiontbl_stop, conditiontbl_visit_occurrence, conditiontbl_visit_detail
                )
                conditiontbl_name_list <- list(
                    "conditiontbl_record", "conditiontbl_person_ratio", "conditiontbl_diff_date", "conditiontbl_start",
                    "conditiontbl_end", "conditiontbl_type_concept", "conditiontbl_stop", "conditiontbl_visit_occurrence",
                    "conditiontbl_visit_detail"
                )

                data_list <- conditiontbl_list
                rds_path <- sapply(FUN = paste0, "Standard RDS/", conditiontbl_name_list, ".rds")
            },
            "drug_exposure" = {
                drug_exptbl_list <- list(
                    drug_exptbl_record, drug_exptbl_person_ratio, drug_exptbl_diff_date, drug_exptbl_start, drug_exptbl_end,
                    drug_exptbl_type_concept, drug_exptbl_stop, drug_exptbl_route, drug_exptbl_visit_occurrence
                )
                drug_exptbl_name_list <- list(
                    "drug_exptbl_record", "drug_exptbl_person_ratio", "drug_exptbl_diff_date", "drug_exptbl_start", "drug_exptbl_end",
                    "drug_exptbl_type_concept", "drug_exptbl_stop", "drug_exptbl_route", "drug_exptbl_visit_occurrence"
                )

                data_list <- drug_exptbl_list
                rds_path <- sapply(FUN = paste0, "Standard RDS/", drug_exptbl_name_list, ".rds")
            },
            "drug_era" = {
                drug_eratbl_list <- list(
                    drug_eratbl_record, drug_eratbl_person_ratio, drug_eratbl_diff_date, drug_eratbl_start, drug_eratbl_end,
                    drug_eratbl_exp_count, drug_eratbl_gap_days
                )
                drug_eratbl_name_list <- list(
                    "drug_eratbl_record", "drug_eratbl_person_ratio", "drug_eratbl_diff_date", "drug_eratbl_start", "drug_eratbl_end",
                    "drug_eratbl_exp_count", "drug_eratbl_gap_days"
                )

                data_list <- drug_eratbl_list
                rds_path <- sapply(FUN = paste0, "Standard RDS/", drug_eratbl_name_list, ".rds")
            }
    )
    mapply(saveRDS, object = data_list, file = rds_path)
}
