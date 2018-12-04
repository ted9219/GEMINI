# Gemini Data extract function

#' This function allows you to render query 
#' @param sqlquery Query what you want to get data in DB.
#' @param tblName Table name in schema.
#' @param AttName Attribute name in schema table.
#' @param comparedAttName Attribute name that compared with AttName.
#' @param startName Time that you need to calculate duration.
#' @param endName Time that you need to calculate duration.
#' @keywords sql
#' @export
queryRender <- function(sqlquery, tblName = "", AttName = "", comparedAttName = "", startName = "", endName = "") {
  # Unset parameter call warning message
  options(warn = -1)
  sql <- SqlRender::renderSql(sql,
    cdm_database_schema = cdmDatabaseSchema, tbl_name = tblName, att_name = AttName,
    compared_att_name = comparedAttName, start_name = startName, end_name = endName
  )$sql
  options(warn = 1)
  sql <- SqlRender::translateSql(sql, targetDialect = attr(connection, "dbms"))$sql

  temp <- DatabaseConnector::querySql(connection, sql)
  # Colname change
  colnames(temp) <- SqlRender::snakeCaseToCamelCase(colnames(temp))

  return(temp)
}

#' This function allows query to get total records fraction
#' @param tblName Table name in schema.
#' @keywords sql
#' @export
get_total_records <- function(tblName) {
  sql <<- "SELECT '@tbl_name' as attribute_name , round(count(*)/CONVERT(float,(
            SELECT SUM(TEMP.rows) FROM(
              SELECT rows FROM(
                SELECT o.name, i.rows FROM sysindexes i
                INNER JOIN 
                sysobjects o 
                ON i.id = o.id
                WHERE i.indid < 2 AND  o.xtype = 'U' AND (o.name='person' OR o.name='death' OR o.name='visit_occurrence'
                                        OR o.name='condition_occurrence' OR o.name='drug_exposure' OR o.name='drug_era')
                )AS T
              )AS TEMP))*100,1
            )as ratio
          FROM @cdm_database_schema.@tbl_name"

  return(queryRender(sql, tblName))
}

#' This function allows query to get person fraction
#' @param tblName Table name in schema.
#' @keywords sql
#' @export
get_person_ratio <- function(tblName) {
  sql <<- "SELECT 'table person' as attribute_name,
          ROUND(count(distinct person_id)/CONVERT(float,(SELECT count(distinct person_id) FROM @cdm_database_schema.person))*100,1) as ratio
          FROM @cdm_database_schema.@tbl_name"

  return(queryRender(sql, tblName))
}

#' This function allows query to get gender fraction
#' @param tblName Table name in schema.
#' @param AttName Attribute name in schema table.
#' @keywords sql
#' @export
get_gender_ratio <- function(tblName, attName) {
  sql <<- "SELECT (SELECT CONCEPT_NAME FROM @cdm_database_schema.concept where concept_id = @att_name) as attribute_name,
          round(count(distinct person_id)/convert(float,(SELECT count(distinct person_id)
          FROM @cdm_database_schema.@tbl_name))*100,1) as ratio
          FROM @cdm_database_schema.@tbl_name
          GROUP BY @att_name"

  return(queryRender(sql, tblName, attName))
}

#' This function allows query to get fraction about attribute
#' @param tblName Table name in schema.
#' @param AttName Attribute name in schema table.
#' @keywords sql
#' @export
get_ratio <- function(tblName, attName) {
  sql <<- "SELECT (SELECT CONCEPT_NAME FROM @cdm_database_schema.concept where concept_id = @att_name) as attribute_name,
          round(count(@att_name)/convert(float,(SELECT count(*) FROM @cdm_database_schema.@tbl_name))*100,1) as ratio
          FROM @cdm_database_schema.@tbl_name 
          GROUP BY @att_name"

  return(queryRender(sql, tblName, attName))
}

#' This function allows query to get count about attribute and get fraction about null.
#' @param tblName Table name in schema.
#' @param AttName Attribute name in schema table.
#' @keywords sql
#' @export
get_null_ratio <- function(tblName, attName) {
  sql <<- "select '@att_name' as attribute_name ,count(distinct @att_name) as attribute_count,
          round(SUM(CASE WHEN @att_name IS NULL THEN 1 ELSE 0 END)/convert(float,SUM(CASE WHEN @att_name IS NULL THEN 1 ELSE 1 END))*100,1) as null_ratio
          from @cdm_database_schema.@tbl_name"

  return(queryRender(sql, tblName, attName))
}

#' This function allows query to get fraction of person per year data
#' @param tblName Table name in schema.
#' @param AttName Attribute name in schema table.
#' @keywords sql
#' @export
get_record_per_year <- function(tblName, attName) {
  sql <<- "WITH T1 AS(
          select	LEFT(@att_name,4) as visit_year, count(person_id) as person_count  FROM @cdm_database_schema.@tbl_name
          GROUP BY LEFT(@att_name,4)
        )
          SELECT visit_year, ROUND(person_count/CONVERT(float,(SELECT SUM(person_count) FROM T1))*100,1) as person_ratio FROM T1
          ORDER BY visit_year ASC"

  return(queryRender(sql, tblName, attName))
}

#' This function allows query to get fraction of duration
#' @param tblName Table name in schema.
#' @param AttName Attribute name in schema table.
#' @keywords sql
#' @export
get_diff_year <- function(tblName, startName, endName) {
  sql <<- "SELECT day_diff, SUM(person_temp) as person_count FROM(SELECT DATEDIFF(day, @start_name, @end_name) as day_diff,
          count(person_id) as person_temp FROM @cdm_database_schema.@tbl_name
          GROUP BY DATEDIFF(day, @start_name, @end_name)) AS T1
          GROUP BY day_diff"

  return(queryRender(sql, tblName, startName = startName, endName = endName))
}

#' This function allows query to extract data and compare other column
#' @param tblName Table name in schema.
#' @param AttName Attribute name in schema table.
#' @param comparedAttName Attribute name that compared with AttName.
#' @keywords sql
#' @export
get_compared_ratio <- function(tblName, attName, comparedAttName) {
  sql <<- "select 'Associate' as attribute_name, ROUND((count(@att_name)/CONVERT(float,count(@compared_att_name)))*100,1) as ratio
          from @cdm_database_schema.@tbl_name"

  return(queryRender(sql, tblName, attName, comparedAttName))
}

#' This function allows query to count stop reason.
#' @param tblName Table name in schema.
#' @keywords sql
#' @export
get_reason_count <- function(tblName) {
  sql <<- "select 'stop reason' as attribute_name ,count(distinct stop_reason) as attribute_count
          from @cdm_database_schema.@tbl_name"
  return(queryRender(sql, tblName))
}