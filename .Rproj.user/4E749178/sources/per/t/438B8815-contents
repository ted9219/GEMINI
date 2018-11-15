################################################################################
#Loading Function
################################################################################
source("gemini.r")
################################################################################
#Get data from condition_occurrence_id
################################################################################
tryCatch(conditiontbl_record <- get_total_records("condition_occurrence")
         ,error=function(e){
           conditiontbl_record <<- NULL
         })
################################################################################
#Get data from person_id
################################################################################
tryCatch(conditiontbl_person_ratio <- get_person_ratio("condition_occurrence")
         ,error=function(e){
           conditiontbl_person_ratio <<- NULL
         })
################################################################################
#Extract condition_start_date
################################################################################
tryCatch(conditiontbl_start <- get_record_per_year("condition_occurrence","condition_start_date")
         ,error=function(e){
           conditiontbl_start <<- NULL
         })
################################################################################
#Extract condition_end_date
################################################################################
tryCatch(conditiontbl_end <- get_record_per_year("condition_occurrence","condition_end_date")
         ,error=function(e){
           conditiontbl_end <<- NULL
         })
################################################################################
#Extract condition_start_date, condition_end_date for histogram
################################################################################
tryCatch(conditiontbl_diff_date <- get_diff_year("condition_occurrence","condition_start_date","condition_end_date")
         ,error=function(e){
           conditiontbl_diff_date <<- NULL
         })
################################################################################
#Get data from condition_type_concept_id
################################################################################
tryCatch(conditiontbl_type_concept <- get_ratio("condition_occurrence","condition_type_concept_id")
         ,error=function(e){
           conditiontbl_type_concept <<- NULL
         })
################################################################################
#Get data from stop_reason
################################################################################
tryCatch({sql<- paste0("SELECT 'stop reason' as attribute_name ,count(distinct stop_reason) as attribute_count
             FROM @cdm_database_schema.condition_occurrence")
sql <- SqlRender::renderSql(sql,cdm_database_schema = cdmDatabaseSchema)$sql
sql <- SqlRender::translateSql(sql, targetDialect = attr(connection, "dbms"))$sql
conditiontbl_stop<-DatabaseConnector::querySql(connection,sql)
colnames(conditiontbl_stop)<-SqlRender::snakeCaseToCamelCase(colnames(conditiontbl_stop))
},error=function(e){
  conditiontbl_stop <<- NULL
})
################################################################################
#Get data from visit_occurrence_id
################################################################################
tryCatch({conditiontbl_visit_occurrence <- get_compared_ratio("condition_occurrence","visit_occurrence_id","condition_occurrence_id")
},error=function(e){
  conditiontbl_visit_occurrence <<- NULL
})
################################################################################
#Get data from visit_detail_id
#NO data in NHIS
################################################################################
tryCatch({conditiontbl_visit_detail <- get_compared_ratio("condition_occurrence","visit_detail_id","condition_occurrence_id")
},error=function(e){
  conditiontbl_visit_detail <<- NULL
})
