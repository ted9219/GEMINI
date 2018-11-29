################################################################################
# Loading Function
################################################################################
source("gemini.r")
################################################################################
# Get data from drug_exposure_id
################################################################################
tryCatch(drug_exptbl_record <- get_total_records("drug_exposure")
  ,
  error = function(e) {
    drug_exptbl_record <<- NULL
  }
)
################################################################################
# Get data from person_id
################################################################################
tryCatch(drug_exptbl_person_ratio <- get_person_ratio("drug_exposure")
  ,
  error = function(e) {
    drug_exptbl_person_ratio <<- NULL
  }
)
################################################################################
# Extract drug_exposure_start_date
################################################################################
tryCatch(drug_exptbl_start <- get_record_per_year("drug_exposure", "drug_exposure_start_date")
  ,
  error = function(e) {
    drug_exptbl_start <<- NULL
  }
)
################################################################################
# Extract drug_exposure_end_date
################################################################################
tryCatch(drug_exptbl_end <- get_record_per_year("drug_exposure", "drug_exposure_end_date")
  ,
  error = function(e) {
    drug_exptbl_end <<- NULL
  }
)
################################################################################
# Get data from drug_exposure_diff_date
################################################################################
tryCatch({
  drug_exptbl_diff_date <- get_diff_year("drug_exposure", "drug_exposure_start_date", "drug_exposure_end_date")
}, error = function(e) {
  drug_exptbl_diff_date <<- NULL
})
################################################################################
# Get data from drug_type_concept_id
################################################################################
tryCatch({
  drug_exptbl_type_concept <- get_ratio("drug_exposure", "drug_type_concept_id")
}, error = function(e) {
  drug_exptbl_type_concept <<- NULL
})
################################################################################
# Get data from stop_reason
################################################################################
tryCatch({
  drug_exptbl_stop <- get_reason_count("drug_exposure")
}, error = function(e) {
  drug_exptbl_stop <<- NULL
})
################################################################################
# Get data from route_concept_id
################################################################################
tryCatch({
  drug_exptbl_route <- get_ratio("drug_exposure", "route_concept_id")
}, error = function(e) {
  drug_exptbl_route <<- NULL
})
################################################################################
# Get data from visit_occurrence_id
################################################################################
tryCatch({
  drug_exptbl_visit_occurrence <- get_compared_ratio("drug_exposure", "visit_occurrence_id", "drug_exposure_id")
}, error = function(e) {
  drug_exptbl_visit_occurrence <<- NULL
})
################################################################################
# Get data from visit_detail_id
# No 'visit_detail_id' data in drug_exposure table
# drug_exposure_visit_detail <- get_compared_ratio("drug_exposure","visit_detail_id","drug_exposure_id")
################################################################################
tryCatch({
  drug_exptbl_visit_detail <- get_compared_ratio("drug_exposure", "visit_detail_id", "drug_exposure_id")
}, error = function(e) {
  drug_exptbl_visit_detail <<- NULL
})
