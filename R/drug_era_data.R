#' Drug era data
#'
#' This function extract data from drug era table
#' @keywords gemini
#' @export
#'
drug_era_data <- function(){
################################################################################
# Get data from drug_era_id
################################################################################
tryCatch(drug_eratbl_record <<- get_total_records("drug_era")
  ,
  error = function(e) {
    drug_eratbl_record <<- NULL
  }
)
################################################################################
# Get data from person_id
################################################################################
tryCatch(drug_eratbl_person_ratio <<- get_person_ratio("drug_era")
  ,
  error = function(e) {
    drug_eratbl_person_ratio <<- NULL
  }
)
################################################################################
# Extract drug_era_start_date
################################################################################
tryCatch(drug_eratbl_start <<- get_record_per_year("drug_era", "drug_era_start_date")
  ,
  error = function(e) {
    drug_eratbl_start <<- NULL
  }
)
################################################################################
# Extract drug_era_end_date
################################################################################
tryCatch(drug_eratbl_end <<- get_record_per_year("drug_era", "drug_era_end_date")
  ,
  error = function(e) {
    drug_eratbl_end <<- NULL
  }
)
################################################################################
# Get data from drug_era_diff_date
################################################################################
tryCatch({
  drug_eratbl_diff_date <<- get_diff_year("drug_era", "drug_era_start_date", "drug_era_end_date")
}, error = function(e) {
  drug_eratbl_diff_date <<- NULL
})
################################################################################
# Get data from drug_exposure_count
#
################################################################################
tryCatch({
  drug_eratbl_exp_count <<- get_ratio("drug_era", "drug_exposure_count")
}, error = function(e) {
  drug_eratbl_exp_count <<- NULL
})
################################################################################
# Get data from gap_day
# hist
################################################################################
tryCatch({
  sql <<- "WITH T1 AS(
  SELECT SUM(person) as person_count, gap_day_range FROM(
    SELECT count(person_id) as person ,CAST((gap_days/5)*5 as VARCHAR)+ '~' + CAST((gap_days/5)*5+4 as VARCHAR) as gap_day_range FROM @cdm_database_schema.drug_era
    GROUP BY gap_days)AS TEMP
  GROUP BY gap_day_range
)
SELECT gap_day_range, ROUND((person_count/CONVERT(float,(SELECT SUM(person_count) FROM T1)))*100,1) as person_ratio FROM T1
ORDER BY CAST(LEFT(gap_day_range,CHARINDEX('~',gap_day_range)-1)AS INT) + CAST(RIGHT(gap_day_range,CHARINDEX('~',gap_day_range)-1)AS INT) ASC"
drug_eratbl_gap_days <<- queryRender(sql)
# sql <- SqlRender::renderSql(sql, cdm_database_schema = cdmDatabaseSchema)$sql
  # sql <- SqlRender::translateSql(sql, targetDialect = attr(connection, "dbms"))$sql
  # drug_eratbl_gap_days <- DatabaseConnector::querySql(connection, sql)
  # colnames(drug_eratbl_gap_days) <- SqlRender::snakeCaseToCamelCase(colnames(drug_eratbl_gap_days))
}, error = function(e) {
  drug_eratbl_gap_days <<- NULL
})
}
