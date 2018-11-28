#######################################################################
#Data extraction
#######################################################################
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#Sys.setlocale('LC_ALL','C')

#sql query Render
queryRender <- function(sqlquery,tblName="",AttName="",comparedAttName="", startName="", endName=""){
  #Unset parameter call warning message
  options(warn=-1)
  sql <- SqlRender::renderSql(sql,cdm_database_schema = cdmDatabaseSchema,tbl_name = tblName, att_name = AttName,
                              compared_att_name = comparedAttName,start_name = startName, end_name=endName)$sql
  options(warn=1)
  sql <- SqlRender::translateSql(sql, targetDialect = attr(connection, "dbms"))$sql
  
  temp <- DatabaseConnector::querySql(connection,sql)
  #Colname change
  colnames(temp) <- SqlRender::snakeCaseToCamelCase(colnames(temp))
  
  return(temp)
}

#Get total records
#Query to find all table records count info, target table record and calculate with it
get_total_records <- function(tblName){
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
  
  return(queryRender(sql,tblName))
}

#Get person ratio with Query
get_person_ratio <- function(tblName){
  sql <<- "SELECT 'table person' as attribute_name,
          ROUND(count(distinct person_id)/CONVERT(float,(SELECT count(distinct person_id) FROM @cdm_database_schema.person))*100,1) as ratio
          FROM @cdm_database_schema.@tbl_name"
  
  return(queryRender(sql,tblName))
}

#Get Gender ratio
get_gender_ratio <- function(tblName,attName){
  sql <<- "SELECT (SELECT CONCEPT_NAME FROM @cdm_database_schema.concept where concept_id = @att_name) as attribute_name,
          round(count(distinct person_id)/convert(float,(SELECT count(distinct person_id)
          FROM @cdm_database_schema.@tbl_name))*100,1) as ratio
          FROM @cdm_database_schema.@tbl_name
          GROUP BY @att_name"
  
  return(queryRender(sql,tblName, attName))
}

#Get regular type ratio
get_ratio <- function(tblName,attName){
  sql <<- paste0("SELECT (SELECT CONCEPT_NAME FROM @cdm_database_schema.concept where concept_id = @att_name) as attribute_name,
                round(count(@att_name)/convert(float,(SELECT count(*) FROM @cdm_database_schema.@tbl_name))*100,1) as ratio
                FROM @cdm_database_schema.@tbl_name 
                GROUP BY @att_name")
  
  return(queryRender(sql,tblName, attName))
}

#Get regular type ratio with NULL ratio
get_null_ratio <- function(tblName,attName){
  sql <<- paste0("select '@att_name' as attribute_name ,count(distinct @att_name) as attribute_count,
round(SUM(CASE WHEN @att_name IS NULL THEN 1 ELSE 0 END)/convert(float,SUM(CASE WHEN @att_name IS NULL THEN 1 ELSE 1 END))*100,1) as null_ratio
                from @cdm_database_schema.@tbl_name")
  
  return(queryRender(sql,tblName, attName))
}

#Get Person per Year data
get_record_per_year <- function(tblName,attName){
  sql <<- paste0("WITH T1 AS(
                select	LEFT(@att_name,4) as visit_year, count(person_id) as person_count  FROM @cdm_database_schema.@tbl_name
                GROUP BY LEFT(@att_name,4)
  )
                SELECT visit_year, ROUND(person_count/CONVERT(float,(SELECT SUM(person_count) FROM T1))*100,1) as person_ratio FROM T1
                ORDER BY visit_year ASC")
  
  return(queryRender(sql,tblName, attName))
}

#End date - Start date, to get Duration
get_diff_year <- function(tblName,startName,endName){
  sql <<- paste0("SELECT day_diff, SUM(person_temp) as person_count FROM(SELECT DATEDIFF(day, @start_name, @end_name) as day_diff,
                count(person_id) as person_temp FROM @cdm_database_schema.@tbl_name
                GROUP BY DATEDIFF(day, @start_name, @end_name)) AS T1
                GROUP BY day_diff ORDER BY day_diff")
  
  return(queryRender(sql,tblName, startName = startName, endName = endName))
}

#Extract data and Compare other column
get_compared_ratio <- function(tblName,attName,comparedAttName){
  sql <<- paste0("select 'Associate' as attribute_name, ROUND((count(@att_name)/CONVERT(float,count(@compared_att_name)))*100,1) as ratio
                 from @cdm_database_schema.@tbl_name")
  
  return(queryRender(sql,tblName, attName, comparedAttName))
}

#######################################################################
#Data Visualization
#######################################################################

#label set
labeling <- function(value){
  lbl <- value$attributeName
  lbl <- paste0(lbl,"\n",value$ratio)
  lbl <- paste0(lbl,"%",seq="")
  return(lbl)
}
#NA data 
naTostring <- function(value){
  napos <- sapply(value$attributeName,FUN=is.na)
  temp_sum <-sum(value$ratio)
  value[napos,2] <- round(100-temp_sum,1)
  value$attributeName[napos] <- "NA"
  return(value)
}

#After no data error, print "No data" to show user
afterError<-function(){
  plot(0,0,axes = F,ann = F, type="n")
  text(0,0,"No data", cex=2.0)
}

#Set x_axis by get min, max
setXAxis <- function(std_value,tar_value){
  if(min(std_value$visitYear)<min(tar_value$visitYear)){
    temp_min <-min(std_value$visitYear)
    temp_diff <- as.numeric(min(tar_value$visitYear))-as.numeric(min(std_value$visitYear))
    temp_vec <- tar_value
  }else{
    temp_min <-min(tar_value$visitYear)
    temp_diff <- as.numeric(min(std_value$visitYear))-as.numeric(min(tar_value$visitYear))
  }
  if(max(std_value$visitYear)>max(tar_value$visitYear)){
    temp_max <-max(std_value$visitYear)
  }else{
    temp_max <-max(tar_value$visitYear)
  }
  lbl <- c(temp_min:temp_max)
  return(lbl)
}

setYAxis <- function(std_value,tar_value){
  #Set y_axis which got more big value
  if(max(std_value$personRatio) > max(tar_value$personRatio)){
    axis <- as.numeric(ceiling(max(std_value$personRatio)))
  }else{
    axis <- as.numeric(ceiling(max(tar_value$personRatio)))
  }
  return(axis)
}

#low value label position setting
label_sort <- function(std_att,tar_att,unit="%", dp = 70){
  if(dp<=std_att){
    std_lbl <- paste0("\n",std_att,unit)
  }else{
    std_lbl <- paste0(std_att,unit,"\n")
  }
  if(dp<=tar_att){
    tar_lbl <- paste0("\n",tar_att,unit)
  }else{
    tar_lbl <- paste0(tar_att,unit,"\n")
  }
  return(c(std_lbl,tar_lbl))
}

gridline <- function(std_value,tar_value){
  if(nrow(std_value)>nrow(tar_value)){
    abline(h=c(0:ceiling(as.numeric(max(std_value$personRatio,tar_value$personRatio)))),
           v=std_value$visitYear, lty=3)
  }else{
    abline(h=c(0:ceiling(as.numeric(max(std_value$personRatio,tar_value$personRatio)))),
           v=tar_value$visitYear, lty=3)
  }
}

#Create Pie chart which use attribute_name
draw_ratio_pie<- function(std_value, tar_value,path){
  jpeg(filename = paste0("../images/",path), width = 720, height = 720, quality = 75, bg = "white")
  # par(mfrow = c(1,2))
  par(mfrow = c(1,2), xpd=T)
  #standard CDM
  tryCatch({
    #If NA, value must get 0. So this NA value get error range 0.1
    std_value <- naTostring(std_value)
    #Label Setting
    #Label name set , append ratio num, percentage mark
    std_lbl <- labeling(std_value)
    std_slices <- as.numeric(std_value$ratio)
    #pie3d doesn't work that some value is 0.0
    std_slices <- sapply(std_slices, function(x) if(x==0.0){ x <- 0.01}else(x))
    #Draw Pie
    pie3D(std_slices,labels = paste0(std_value$ratio,"%") ,explode = 0.1, main="Standard CDM",
          radius= 1.0, labelcex = 1.5, theta=0.8,start=pi/2, cex.main=2.0, col=rainbow(nrow(std_value)+1, s = 0.7))
    legend(-1.5,-1.5, std_value$attributeName, cex = 1.5, fill=rainbow(nrow(std_value)+1, s = 0.7),xpd=T)
  },#If data isn't exist...
  error = function(error_message){
    print(error_message)
    afterError()
  })
  #target CDM
  
  tryCatch({
    #If NA, value must get 0. And this NA value get error range 0.1
    tar_value <- naTostring(tar_value)
    #Label Setting
    tar_lbl <- labeling(tar_value)
    tar_slices <- as.numeric(tar_value$ratio)
    #pie3d doesn't work that some value is 0.0
    tar_slices <- sapply(tar_slices, function(x) if(x==0.0){ x <- 0.01}else(x))
    #Draw Pie
    pie3D(tar_slices,labels = paste0(tar_value$ratio,"%") ,explode = 0.03, main="Target CDM",
          radius= 1.0, labelcex = 1.5, theta=0.8,start=pi/2,cex.main=2.0, col= rainbow(nrow(tar_value)+1, s = 0.7) )
    legend(-1.5,-1.5, tar_value$attributeName, cex = 1.5, fill=rainbow(nrow(tar_value)+1, s = 0.7), xpd=T)
  },#If data isn't exist...
  error = function(error_message){
    print(error_message)
    afterError()
  })
}

draw_compare_pie<- function(std_value, tar_value,path){
  jpeg(filename = paste0("../images/",path), width = 720, height = 720, quality = 75, bg = "white")
  par(mfrow = c(1,2))
  #standard CDM
  tryCatch({
    #If NA, value must get 0. And this NA value get error range 0.1
    std_value <- naTostring(std_value)
    #Label Setting
    #Label name set , append ratio num, percentage mark
    std_lbl <- labeling(std_value)
    std_slices <- c(as.numeric(std_value$ratio),as.numeric(100-std_value$ratio))
    #pie3d doesn't work that some value is 0.0
    std_slices <- sapply(std_slices, function(x) if(x==0.0||x==0){ x <- 0.01}else(x))
    #Draw pie
    pie3D(std_slices,labels = c("",std_lbl),explode = 0.03, main="Standard CDM",
          radius= 1.0, labelcex = 1.5, theta=0.8,start=pi/2, cex.main=2.0, col=rainbow(nrow(std_value)+1, s = 0.7))
  },#If data isn't exist...
  error = function(error_message){
    print(error_message)
    browser()
    afterError()
  })
  #target CDM
  
  tryCatch({
    #If NA, value must get 0. And this NA value get error range 0.1
    tar_value <- naTostring(tar_value)
    #Label Setting
    tar_lbl <- labeling(tar_value)
    tar_slices <- c(as.numeric(tar_value$ratio),as.numeric(100-tar_value$ratio))
    #pie3d doesn't work that some value is 0.0
    tar_slices <- sapply(tar_slices, function(x) if(x==0.0){ x <- 0.01}else(x))
    #Draw pie
    pie3D(tar_slices,labels = c("",tar_lbl),explode = 0.03, main="Target CDM",
          radius= 1.0, labelcex = 1.5, theta=0.8, start=pi/2, cex.main=2.0, col=rainbow(nrow(tar_value)+1, s = 0.7))
  },#If data isn't exist...
  error = function(error_message){
    print(error_message)
    afterError()
  })
  
}

draw_table_pie <- function(std_value,tar_value,tblname,path){
  jpeg(filename = paste0("../images/",path), width = 720, height = 720, quality = 75, bg = "white")
  par(mfrow = c(1,2))
  #standard CDM
  tryCatch({
    #Label Setting 
    #Label name set , append ratio num, percentage mark
    std_recordlbl <- paste(tblname,"\n",std_value$ratio)
    std_recordlbl <- paste(std_recordlbl,"%",seq="")
    #Set pie slice
    #just 1 kind of attribute
    std_recordslices <- c(as.numeric(std_value$ratio),as.numeric(100-std_value$ratio))
    #pie3d doesn't work which value is 0.0
    sapply(std_recordslices, function(x) if(x==0.0){ x <- 0.01}else(x))
    pie3D(std_recordslices,labels = c(std_recordlbl,""),explode = 0.03, main="Standard CDM", 
          col=rainbow(nrow(std_value)+1, s = 0.7), radius= 1.0, labelcex = 1.5, theta=0.8,start=pi/2, cex.main=2.0)
  },#If data isn't exist...
  error = function(error_message){
    print(error_message)
    afterError()
  })
  #target CDM
  tryCatch({
    #Label Setting
    tar_recordlbl <- paste(tblname,"\n",tar_value$ratio)
    tar_recordlbl <- paste(tar_recordlbl,"%",seq="")
    #Set pie slice
    tar_recordslices <- c(as.numeric(tar_value$ratio),as.numeric(100-tar_value$ratio))
    #pie3d doesn't work which value is 0.0
    sapply(tar_recordslices, function(x) if(x==0.0){ x <- 0.01}else(x))
    pie3D(tar_recordslices,labels = c(tar_recordlbl,""),explode = 0.03, main="Target CDM",
          col=rainbow(nrow(tar_value)+1, s = 0.7), radius= 1.0, labelcex = 1.5, theta=0.8,start=pi/2, cex.main=2.0)
  },#If data isn't exist...
  error = function(error_message){
    print(error_message)
    afterError()
  })
}

#Draw line graph for start date
draw_line_start<- function(std_value,tar_value,text="",path){
  jpeg(filename = paste0("../images/",path), width = 720, height = 720, quality = 75, bg = "white")
  par(mfrow = c(1,1))
  #Draw line Graph
  tryCatch({
    x_lbl <- setXAxis(std_value,tar_value)
    y_axis <- setYAxis(std_value,tar_value)
    #drawing line
    plot(std_value, type='o', col=4,lwd=2, xlab="YEAR", ylab="Person ratio(%)",axes=T, xlim=c(min(x_lbl),max(x_lbl)), ylim=c(0,y_axis),
         main=paste0(text," Start Date"),cex.main=2.0, cex.lab= 1.5)
    lines(tar_value,type='o', col=2,lwd=2)
    axis(1,at = c(1:length(x_lbl)),labels = x_lbl)
    axis(2,at = c(0:y_axis), cex.axis =2.0)
    box()
    gridline(std_value,tar_value)
    legend("topleft",c("Standard", "Target"),lwd=2,lty=1,cex = 1.5,col=c("blue","red"))
  },#If data isn't exist...
  error = function(error_message){
    print(error_message)
    afterError()
  })
}

#Draw line graph for end date
draw_line_end <- function(std_value,tar_value,na_value="No data",over_value="No data",title="",path){
  jpeg(filename = paste0("../images/",path),width = 720, height = 720, quality = 75, bg = "white")
  par(mfrow = c(1,1))
  #Draw line Graph
  tryCatch({
    #Set x_axis by get min, max
    x_lbl <- setXAxis(std_value,tar_value)
    y_axis <- setYAxis(std_value,tar_value)
  
    #drawing line
    plot(std_value, type='o', col=4, xlab="YEAR", ylab="Person_ratio(%)",axes=T, xlim=c(min(x_lbl),max(x_lbl)), ylim=c(0,y_axis),
         main=paste0(title," End Date"), lwd=2,cex.main=2.0, cex.lab= 1.5)
    lines(tar_value,type='o', col=2, lwd=2)
    axis(1,at = c(1:length(x_lbl)),labels = x_lbl)
    axis(2,at = c(0:y_axis), cex.axis =2.0)
    box()
    gridline(std_value,tar_value)
    legend("topleft",c("Standard", "Target"),lwd=2,lty=1 ,cex = 1.5,col=c("blue","red"))
    #Need to append NA, 2999 person data.
    if(is.null(na_value)&&is.null(over_value)){}
    else{
      #as.numeric(min(c(std_value$visitYear,tar_value$visitYear)))+10,y=as.numeric(max(c(std_value$personRatio,tar_value$personRatio)))
      mtext(side = 3, line=-5, adj = 1 , cex = 1.2,font=2, outer=T,
            text = paste0("Standard CDM NA : ",na_value[1]," / Target CDM NA : ",na_value[2],"\nStandard CDM 2999 : ",over_value[1]," / Target CDM 2999 : ",over_value[2])
           )
    }
  },#If data isn't exist...
  error = function(error_message){
    print(error_message)
    afterError()
  })
}

#Draw bar chart with count and NULL
draw_null_bar <- function(std_value, tar_value, text ="",path){
  jpeg(filename = paste0("../images/",path),width = 720, height = 720, quality = 75, bg = "white")
  par(mfrow = c(1,2))
  tryCatch({
  #count drawing
    bar_data <- barplot(c(std_value$attributeCount, tar_value$attributeCount), ylim = c(0,max(std_value$attributeCount, tar_value$attributeCount)), cex.names = 2.0,
                        beside = F, names=c("Standard","Target"), col=c("Green","Yellow"),main = paste("Number of",text,"type"),cex.main=2.0,cex.axis = 2.0,
                        ylab = "Counts (s)", cex.lab= 1.5)
    text(x=bar_data,y= c(std_value$attributeCount,tar_value$attributeCount),
         labels=label_sort(std_value$attributeCount,tar_value$attributeCount,unit="s",dp = max(c(std_value$attributeCount,tar_value$attributeCount))*0.5)
         ,col="black",cex = 1.5)
    #NULL data drawing
    null_bar <- barplot(c(std_value$nullRatio, tar_value$nullRatio), ylim = c(0,max(std_value$nullRatio, tar_value$nullRatio)),
                        beside = F, names=c("Standard","Target"), col=c("Gray40","Gray55"),main = "NULL Ratio", cex.main=2.0, cex.axis = 2.0,
                        ylab = "Percentage (%)",cex.names = 2.0, cex.lab= 1.5)
    text(x=null_bar,y= c(std_value$nullRatio,tar_value$nullRatio),
         labels=label_sort(std_value$nullRatio,tar_value$nullRatio,dp = max(c(std_value$nullRatio,tar_value$nullRatio))*0.5)
         ,col="black",cex = 1.5)
  },#If data isn't exist...
  error = function(error_message){
    print(error_message)
    afterError()
  })
}

#Draw bar chart
draw_ratio_bar<- function(std_value,tar_value,path){
  jpeg(filename = paste0("../images/",path),width = 720, height = 720, quality = 75, bg = "white")
  par(mfrow = c(1,1))
  tryCatch({
    #count drawing
    bar_data <- barplot(c(std_value$ratio, tar_value$ratio),
                        beside = T, names=c("Standard","Target"), col=c("Green","Yellow"),main = "Count by hospital",
                        xlab = "Hospital", ylab = "Percentage (%)",cex.names = 2.5,cex.main=2.0, cex.lab= 1.5, cex.axis =2.0)
    #low value label position setting
    
    # if(max(c(std_value$ratio,tar_value$ratio))*0.5<=std_value$ratio){
    #   std_lbl <- paste("\n",std_value$ratio,"%")
    # }else{
    #   std_lbl <- paste(std_value$ratio,"%\n")
    # }
    # if(max(c(std_value$ratio,tar_value$ratio))*0.5<=tar_value$ratio){
    #   tar_lbl <- paste("\n",tar_value$ratio,"%")
    # }else{
    #   tar_lbl <- paste(tar_value$ratio,"%\n")
    # }
    text(x=bar_data,y= c(std_value$ratio,tar_value$ratio),
         labels=label_sort(std_value$ratio,tar_value$ratio,dp = max(c(std_value$ratio,tar_value$ratio))*0.5),col="black",cex = 1.5)
  },#If data isn't exist...
  error = function(error_message){
    print(error_message)
    afterError()
  })
}
#Using count how many kinds
draw_count_bar <- function(std_value, tar_value,text="",path){
  jpeg(filename = paste0("../images/",path),width = 720, height = 720, quality = 75, bg = "white")
  par(mfrow = c(1,1))
  tryCatch({
    #count drawing
    bar_data <- barplot(c(std_value$attributeCount, tar_value$attributeCount),
                        beside = F, names=c("Standard","Target"), col=c("Green","Yellow"),main = paste0(text," Count by hospital"),
                        xlab = "Hospital", ylab = "Counts (s)",cex.names = 1.5, cex.main=2.0, cex.lab= 1.5, cex.axis =2.0)
    #low value label position setting
    # if(max(c(std_value$attributeCount,tar_value$attributeCount))*0.5<=std_value$attributeCount){
    #   std_lbl <- paste("\n",std_value$attributeCount,"s")
    # }else{
    #   std_lbl <- paste(std_value$attributeCount,"s\n")
    # }
    # if(max(c(std_value$attributeCount,tar_value$attributeCount))*0.5<=tar_value$attributeCount){
    #   tar_lbl <- paste("\n",tar_value$attributeCount,"s")
    # }else{
    #   tar_lbl <- paste(tar_value$attributeCount,"s\n")
    # }
    text(x=bar_data,y= c(std_value$attributeCount,tar_value$attributeCount),
         labels=label_sort(std_value$attributeCount,tar_value$attributeCount,unit="s",dp=max(c(std_value$attributeCount,tar_value$attributeCount))*0.5)
         ,col="black",cex = 1.5)
  },#If data isn't exist...
  error = function(error_message){
    print(error_message)
    afterError()
  })
}
#Compare two attributes
draw_compare_bar<- function(std_value,tar_value,text ="",path){
  jpeg(filename = paste0("../images/",path),width = 720, height = 720, quality = 75, bg = "white")
  par(mfrow = c(1,1))
  tryCatch({
    #count drawing
    bar_data <- barplot(c(std_value$ratio, tar_value$ratio),
                        beside = F, names=c("Standard","Target"), col=c("Green","Yellow"),main = paste0(text, " by hospital"),
                        xlab = "Hospital", ylab = "Percentage (%)",cex.names = 2.5, cex.main=2.0, cex.lab= 1.5, cex.axis =2.0)
    #low value label position setting
    
    # 
    # if(max(c(std_value$ratio,tar_value$ratio))*0.5<=std_value$ratio){
    #   std_lbl <- paste("\n",std_value$ratio,"%")
    # }else{
    #   std_lbl <- paste(std_value$ratio,"%\n")
    # }
    # if(max(c(std_value$ratio,tar_value$ratio))*0.5<=tar_value$ratio){
    #   tar_lbl <- paste("\n",tar_value$ratio,"%")
    # }else{
    #   tar_lbl <- paste(tar_value$ratio,"%\n")
    # }
    text(x=bar_data,y= c(std_value$ratio,tar_value$ratio),
         labels=label_sort(std_value$ratio,tar_value$ratio,dp = max(c(std_value$ratio,tar_value$ratio))*0.5),col="black",cex = 1.5)
  },#If data isn't exist...
  error = function(error_message){
    print(error_message)
    afterError()
  })
}

