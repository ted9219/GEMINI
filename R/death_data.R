################################################################################
# Loading Function
################################################################################
source("gemini.r")
################################################################################
# Get data from death_date to check person who got 0 or many death_date
# If not value = 1, It should be wrong data
################################################################################
tryCatch({
  sql <- paste0("select attribute_name, ROUND(count(attribute_name)*100/convert(float,(select count(*) from @cdm_database_schema.death)),1) as ratio 
        from (select count(death_date) as attribute_name from @cdm_database_schema.death group by person_id)
              AS death_date_temp group by attribute_name")
  deathtbl_check <<- queryRender(sql)
  # sql <- SqlRender::renderSql(sql, cdm_database_schema = cdmDatabaseSchema)$sql
  # sql <- SqlRender::translateSql(sql, targetDialect = attr(connection, "dbms"))$sql
  # deathtbl_check <- DatabaseConnector::querySql(connection, sql)
  # colnames(deathtbl_check) <- SqlRender::snakeCaseToCamelCase(colnames(deathtbl_check))
}, error = function(e) {
  deathtbl_check <<- NULL
})
################################################################################
# Get data from death_type_concept_id to check death type.
################################################################################
tryCatch({
  sql <- paste0("SELECT (SELECT CONCEPT_NAME FROM @cdm_database_schema.concept where concept_id = death_type_concept_id) as attribute_name,
              round(100*count(distinct person_id)/convert(float,(SELECT count(distinct person_id) FROM @cdm_database_schema.death)),1) as ratio
              FROM @cdm_database_schema.death
              GROUP BY death_type_concept_id")
  deathtbl_type <<- queryRender(sql)
  # sql <- SqlRender::renderSql(sql, cdm_database_schema = cdmDatabaseSchema)$sql
  # sql <- SqlRender::translateSql(sql, targetDialect = attr(connection, "dbms"))$sql
  # deathtbl_type <- DatabaseConnector::querySql(connection, sql)
  # colnames(deathtbl_type) <- SqlRender::snakeCaseToCamelCase(colnames(deathtbl_type))
}, error = function(e) {
  deathtbl_type <<- NULL
})
