################################################################################
#Loading Function
################################################################################
source("gemini.r")
################################################################################
#Calculate records ratio
#Query to find all table records count info, target table record and calculate with it
################################################################################
tryCatch(persontbl_record <- get_total_records("PERSON")
         ,error=function(e){
           persontbl_record <<- NULL
         })
################################################################################
#Calculate person ratio
#In person Table, It will be 100%. Unless it should be error
################################################################################
tryCatch(persontbl_person_ratio <- get_person_ratio("PERSON")
         ,error=function(e){
           persontbl_person_ratio <<- NULL
         })
################################################################################
#Using gender_concept_id, make plot
#In bar chart, focus on Male, Female to easy compare
################################################################################
tryCatch(persontbl_gender <- get_gender_ratio("PERSON","gender_concept_id")
         ,error=function(e){
           persontbl_gender <<- NULL
         })
################################################################################
#Extract data from person, observation_period to calculate ratio
#In query, Get person_ratio, age_range, gender_concept_id
#In R, Null append, 0.0 to 0.1 converted
#Query Year band with person
################################################################################
tryCatch({sql<-"WITH T1 AS(
SELECT SUM(person_count) AS person_count, AGE_RANGE, gender_concept_id FROM(
SELECT	count(A.person_ID) as person_count
		, CAST(((MIN(YEAR(OBSERVATION_PERIOD_START_DATE)-year_of_birth))/5)*5 as VARCHAR)+ '~' + CAST(((MIN(YEAR(OBSERVATION_PERIOD_START_DATE)-year_of_birth))/5)*5+4 as VARCHAR) as AGE_RANGE
		, gender_concept_id FROM @cdm_database_schema.observation_period AS A
INNER JOIN @cdm_database_schema.person AS B ON A.PERSON_ID = B.person_id
GROUP BY A.person_id, YEAR(OBSERVATION_PERIOD_START_DATE)-year_of_birth, gender_concept_id
) AS TEMP
GROUP BY AGE_RANGE, gender_concept_id
)
SELECT AGE_RANGE, gender_concept_id, ROUND(person_count*100/CONVERT(float,(SELECT SUM(person_count) FROM T1)),1) as ratio FROM T1
ORDER BY CAST(LEFT(AGE_RANGE,CHARINDEX('~',AGE_RANGE)-1)AS INT) + CAST(RIGHT(AGE_RANGE,CHARINDEX('~',AGE_RANGE)-1)AS INT) ASC ,gender_concept_id"
sql <- SqlRender::renderSql(sql, cdm_database_schema = cdmDatabaseSchema)$sql
sql <- SqlRender::translateSql(sql, targetDialect = attr(connection, "dbms"))$sql
persontbl_min_age<-DatabaseConnector::querySql(connection,sql)
colnames(persontbl_min_age)<-SqlRender::snakeCaseToCamelCase(colnames(persontbl_min_age))
#value 0.0 to 0.01 that is not no data. just too small to ceiling 
options(scipen = 9999)
i <- 1
while(i!= nrow(persontbl_min_age)+1){
  if(persontbl_min_age$ratio[i] == 0.0 || persontbl_min_age$ratio[i] == 0){
    persontbl_min_age$ratio[i] = 0.01
  }
  if(persontbl_min_age$genderConceptId[i] == '8507' && persontbl_min_age$genderConceptId[i+1] != '8532'){
    temp <- c(persontbl_min_age$ageRange[i],'8532',0)
    persontbl_min_age <- rbind(persontbl_min_age[c(1:i),],temp,persontbl_min_age[c(i+1:nrow(persontbl_min_age)),])
    
  }
  else if(persontbl_min_age$genderConceptId[i] == '8532' && persontbl_min_age$genderConceptId[i-1] != '8507'){
    temp <- c(persontbl_min_age$ageRange[i],'8507',0)
    persontbl_min_age <- rbind(persontbl_min_age[c(1:i-1),],temp,persontbl_min_age[c(i:nrow(persontbl_min_age)),])
    
  }
  i<-i+1
}
},error=function(e){
  persontbl_min_age <<- NULL
})
################################################################################
#OBSERVATION_PERIOD end_date
#Same work
################################################################################
tryCatch({sql<-"WITH T1 AS(
SELECT SUM(person_count) AS person_count, AGE_RANGE, gender_concept_id FROM(
SELECT	count(A.person_ID) as person_count
, CAST(((MAX(YEAR(OBSERVATION_PERIOD_START_DATE)-year_of_birth))/5)*5 as VARCHAR)+ '~' + CAST(((MAX(YEAR(OBSERVATION_PERIOD_START_DATE)-year_of_birth))/5)*5+4 as VARCHAR) as AGE_RANGE
, gender_concept_id FROM @cdm_database_schema.observation_period AS A
INNER JOIN @cdm_database_schema.person AS B ON A.PERSON_ID = B.person_id
GROUP BY A.person_id, YEAR(OBSERVATION_PERIOD_START_DATE)-year_of_birth, gender_concept_id
) AS TEMP
GROUP BY AGE_RANGE, gender_concept_id
)
SELECT AGE_RANGE, gender_concept_id, ROUND(person_count*100/CONVERT(float,(SELECT SUM(person_count) FROM T1)),1) as ratio FROM T1
ORDER BY CAST(LEFT(AGE_RANGE,CHARINDEX('~',AGE_RANGE)-1)AS INT) + CAST(RIGHT(AGE_RANGE,CHARINDEX('~',AGE_RANGE)-1)AS INT) ASC ,gender_concept_id"
sql <- SqlRender::renderSql(sql,
                            cdm_database_schema = cdmDatabaseSchema)$sql
sql <- SqlRender::translateSql(sql, targetDialect = attr(connection, "dbms"))$sql
persontbl_max_age<-DatabaseConnector::querySql(connection,sql)
colnames(persontbl_max_age)<-SqlRender::snakeCaseToCamelCase(colnames(persontbl_max_age))
#value 0.0 to 0.01 that is not no data. just too small to ceiling 
#options(scipen = 9999)
i <- 1
while(i!= nrow(persontbl_max_age)+1){
  if(persontbl_max_age$ratio[i] == 0.0 || persontbl_max_age$ratio[i] == 0){
    persontbl_max_age$ratio[i] = 0.01
  }
  if(persontbl_max_age$genderConceptId[i] == '8507' && persontbl_max_age$genderConceptId[i+1] != '8532'){
    temp <- c(persontbl_max_age$ageRange[i],'8532',0)
    persontbl_max_age <- rbind(persontbl_max_age[c(1:i),],temp,persontbl_max_age[c(i+1:nrow(persontbl_max_age)),])
    
  }
  else if(persontbl_max_age$genderConceptId[i] == '8532' && persontbl_max_age$genderConceptId[i-1] != '8507'){
    temp <- c(persontbl_max_age$ageRange[i],'8507',0)
    persontbl_max_age <- rbind(persontbl_max_age[c(1:i-1),],temp,persontbl_max_age[c(i:nrow(persontbl_max_age)),])
    
  }
  i<-i+1
}
male_max_ratio <- persontbl_max_age$ratio[persontbl_max_age$genderConceptId == '8507']
female_max_ratio <- persontbl_max_age$ratio[persontbl_max_age$genderConceptId == '8532']
x_max_lbl <- persontbl_max_age$ageRange[persontbl_max_age$genderConceptId == '8507']
},error=function(e){
  persontbl_max_age <<- NULL
})
################################################################################
#Get data from race_concept_id
################################################################################
tryCatch(persontbl_race <- get_ratio("person","race_concept_id")
         ,error=function(e){
           persontbl_race <<- NULL
})
################################################################################
#Get data from ethnicity_concept_id
################################################################################
tryCatch(persontbl_ethnicity<- get_ratio("person","ethnicity_concept_id")
         ,error=function(e){
           persontbl_ethnicity <<- NULL
         })
################################################################################
#Get data from location_id
#NULL ratio
################################################################################
tryCatch(persontbl_location <- get_null_ratio("person","location_id")
         ,error=function(e){
           persontbl_location <<- NULL
         })
################################################################################
#Get data from provider_id
################################################################################
tryCatch(persontbl_provider <- get_null_ratio("person","provider_id")
         ,error=function(e){
           persontbl_provider <<- NULL
         })
################################################################################
#Get data from care_site_id
################################################################################
tryCatch({sql <- paste0("select 'care_site_id' as attribute_name ,count(care_site_id) as attribute_count,
round(SUM(CASE WHEN care_site_id IS NULL THEN 1 ELSE 0 END)/convert(float,SUM(CASE WHEN care_site_id IS NULL THEN 1 ELSE 1 END))*100,1) as null_ratio
              from @cdm_database_schema.person")
sql <- SqlRender::renderSql(sql, cdm_database_schema = cdmDatabaseSchema)$sql
sql <- SqlRender::translateSql(sql, targetDialect = attr(connection, "dbms"))$sql
persontbl_care_site <- DatabaseConnector::querySql(connection,sql)
colnames(persontbl_care_site)<-SqlRender::snakeCaseToCamelCase(colnames(persontbl_care_site))
},error=function(e){
  persontbl_care_site <<- NULL
})
