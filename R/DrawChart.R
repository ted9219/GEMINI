# install.packages('plotrix')
library(plotrix)
library(ggplot2)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
dir.create(file.path(getwd(), "../images"), showWarnings = FALSE)
source("gemini.R")
#Library for Pie 3D function
options(scipen = 999999)
#Draw graph in one bar chart
################################################################################
#WHOLE TABLE info VISUALLIZATION
################################################################################
################################################################################
#WHOLE TABLE Record info
################################################################################
par(mfrow = c(1,1),xpd=T)
jpeg(filename = "../images/Whole/00.Record.jpg",
     width = 720, height = 720, quality = 75, bg = "white")
record_bar <- barplot(c(std_persontbl_record$ratio[1],tar_persontbl_record$ratio[1],std_visittbl_record$ratio[1],tar_visittbl_record$ratio[1],
                        std_conditiontbl_record$ratio[1],tar_conditiontbl_record$ratio[1],std_drug_exptbl_record$ratio[1],tar_drug_exptbl_record$ratio[1],
                        std_drug_eratbl_record$ratio[1],tar_drug_eratbl_record$ratio[1]),
                      beside = F, names=c("Person",NA,"Visit",NA,"Condition",NA,
                                          "Drug exp",NA,"Drug era",NA),
                      ylim = c(0,100),col=c("Green","Yellow"),main = "Record by hospital",xlab = "Table name", ylab = "Percentage (%)", cex.axis = 1.5, cex.names =1.5,
                      cex.main=2.0, cex.lab= 1.5)
text(x=record_bar,y= c(std_persontbl_record$ratio[1],tar_persontbl_record$ratio[1],std_visittbl_record$ratio[1],tar_visittbl_record$ratio[1],
                       std_conditiontbl_record$ratio[1],tar_conditiontbl_record$ratio[1],std_drug_exptbl_record$ratio[1],tar_drug_exptbl_record$ratio[1],
                       std_drug_eratbl_record$ratio[1],tar_drug_eratbl_record$ratio[1]),
     labels=c(label_sort(std_persontbl_record$ratio[1],tar_persontbl_record$ratio[1]),
              label_sort(std_visittbl_record$ratio[1],tar_visittbl_record$ratio[1]),
              label_sort(std_conditiontbl_record$ratio[1],tar_conditiontbl_record$ratio[1]),
              label_sort(std_drug_exptbl_record$ratio[1],tar_drug_exptbl_record$ratio[1]),
              label_sort(std_drug_eratbl_record$ratio[1],tar_drug_eratbl_record$ratio[1])
     ),col="black",cex=2.0)
legend("topleft",c("Standard CDM","Target CDM"),pch=15,cex = 1.5,col=c("green","yellow"))
# dev.copy(device = jpeg , filename=paste0())
dev.off()

################################################################################
#WHOLE TABLE Person info
################################################################################
par(mfrow = c(1,1))
jpeg(filename = "../images/Whole/01.Person.jpg",
     width = 720, height = 720, quality = 75, bg = "white")
#Draw graph in one bar chart
person_bar <- barplot(c(std_persontbl_person_ratio$ratio,tar_persontbl_person_ratio$ratio,std_visittbl_person_ratio$ratio,tar_visittbl_person_ratio$ratio,
                        std_conditiontbl_person_ratio$ratio,tar_conditiontbl_person_ratio$ratio,std_drug_exptbl_person_ratio$ratio,tar_drug_exptbl_person_ratio$ratio,
                        std_drug_eratbl_person_ratio$ratio,tar_drug_eratbl_person_ratio$ratio),
                      beside = F, names=c("Person",NA,"Visit",NA,"Condition",NA,
                                          "Drug exp",NA,"Drug era",NA),
                      ylim = c(0,100),col=c("Green","Yellow"),main = "Person by Hospital",xlab = "Table name", ylab = "Percentage (%)",cex.axis = 1.5, cex.names = 1.5,
                      cex.main=2.0, cex.lab= 1.5)
text(x=person_bar,y= c(std_persontbl_person_ratio$ratio,tar_persontbl_person_ratio$ratio,std_visittbl_person_ratio$ratio,tar_visittbl_person_ratio$ratio,
                       std_conditiontbl_person_ratio$ratio,tar_conditiontbl_person_ratio$ratio,std_drug_exptbl_person_ratio$ratio,tar_drug_exptbl_person_ratio$ratio,
                       std_drug_eratbl_person_ratio$ratio,tar_drug_eratbl_person_ratio$ratio),
     labels=c(label_sort(std_persontbl_person_ratio$ratio,tar_persontbl_person_ratio$ratio),
              label_sort(std_visittbl_person_ratio$ratio,tar_visittbl_person_ratio$ratio),
              label_sort(std_conditiontbl_person_ratio$ratio,tar_conditiontbl_person_ratio$ratio),
              label_sort(std_drug_exptbl_person_ratio$ratio,tar_drug_exptbl_person_ratio$ratio),
              label_sort(std_drug_eratbl_person_ratio$ratio,tar_drug_eratbl_person_ratio$ratio)
     ),col="black",cex=2.0)
legend("bottomleft",c("Standard CDM","Target CDM"),pch=15,cex = 1.5,col=c("green","yellow"))

# dev.copy(device = jpeg ,filename=paste0("images/Whole/01.Person.jpg"))
dev.off()
################################################################################
#PERSON TABLE VISUALLIZATION
################################################################################
################################################################################
# person_record
################################################################################
draw_table_pie(std_persontbl_record,tar_persontbl_record,"PERSON TABLE","Person/00.Person_record.jpg")
mtext("Person Table Ratio",side=3, line= -5, outer=T, cex = 2.5, font =2)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# gender_concept_id
################################################################################
jpeg(filename = "../images/Person/01.Person_gender.jpg",
     width = 720, height = 720, quality = 75, bg = "white")
par(mfrow = c(1,1),xpd=T)
#Draw graph in one bar chart
gender_bar <- barplot(c(std_persontbl_gender$ratio[1], tar_persontbl_gender$ratio[1], std_persontbl_gender$ratio[2], tar_persontbl_gender$ratio[2]),
                      beside = F, ylim =c(0,100),names=c(std_persontbl_gender$attributeName[1], tar_persontbl_gender$attributeName[1],
                                                         std_persontbl_gender$attributeName[2], tar_persontbl_gender$attributeName[2]),
                      col=c("Green","Yellow"),main = "Gender by hospital", xlab = "Gender", ylab = "Percentage (%)",cex.main=2.0, cex.lab= 1.5,
                      cex.names = 1.5,cex.axis = 1.5)
text(x=gender_bar,y= c(std_persontbl_gender$ratio[1],tar_persontbl_gender$ratio[1],std_persontbl_gender$ratio[2],tar_persontbl_gender$ratio[2]),
     labels=c(label_sort(std_persontbl_gender$ratio[1],tar_persontbl_gender$ratio[1]),
              label_sort(std_persontbl_gender$ratio[2],tar_persontbl_gender$ratio[2])),col="black",cex=2.0)
legend("topleft",c("Standard CDM","Target CDM"),pch=15,cex = 2.0,col=c("green","yellow"))
#Graph Save
dev.off() 
################################################################################
# year_of_birth (min)
################################################################################
jpeg(filename = "../images/Person/02.Person_min_year.jpg",
     width = 720, height = 720, quality = 75, bg = "white")
par(mfrow = c(1,1),xpd=F)
#Divide ratio by Gender
male_min_ratio <- std_persontbl_min_age$ratio[std_persontbl_min_age$genderConceptId == '8507']
female_min_ratio <- std_persontbl_min_age$ratio[std_persontbl_min_age$genderConceptId == '8532']
compared_male_min_ratio <- tar_persontbl_min_age$ratio[tar_persontbl_min_age$genderConceptId == '8507']
compared_female_min_ratio <- tar_persontbl_min_age$ratio[tar_persontbl_min_age$genderConceptId == '8532']
  #Set label which got more long length
if(length(std_persontbl_min_age$ageRange[std_persontbl_min_age$genderConceptId == '8507'])
   >length(tar_persontbl_min_age$ageRange[tar_persontbl_min_age$genderConceptId == '8507'])){
  x_min_lbl <- std_persontbl_min_age$ageRange[std_persontbl_min_age$genderConceptId == '8507']
}else{
  x_min_lbl <- tar_persontbl_min_age$ageRange[tar_persontbl_min_age$genderConceptId == '8507']
}
#Draw line Graph
plot(male_min_ratio, type='o',lwd = 2, col=4, xlab="AGE_RANGE", ylab="GENDER_RATIO(%)",axes=F,
     ylim=c(0,as.numeric(max(c(male_min_ratio,female_min_ratio,compared_male_min_ratio,compared_female_min_ratio)))),
     xlim = c(0,length(x_min_lbl)), main="First Visit Person per Year",cex.main=2.0,cex.lab = 1.5)
axis(1,at = c(1:length(x_min_lbl)),labels = x_min_lbl,cex.axis = 1.5)
  #Set y_axis which got more big value
if(max(std_persontbl_min_age$ratio) > max(tar_persontbl_min_age$ratio)){
  y_axis <- as.numeric(max(std_persontbl_min_age$ratio))
}else{
  y_axis <- as.numeric(max(tar_persontbl_min_age$ratio))
}
axis(2,at = c(0:ceiling(y_axis)),cex.axis = 1.5)
     
#add another line graph
lines(female_min_ratio, type='o',lwd = 2, col=2)+
lines(compared_male_min_ratio, type='o', lwd = 2,lty=5, col="cyan4")+
lines(compared_female_min_ratio, type='o',lwd = 2, lty=5, col="brown")
box()
abline(h=c(0:ceiling(as.numeric(max(std_persontbl_min_age$ratio,tar_persontbl_min_age$ratio)))),
       v=c(1:(max(nrow(std_persontbl_min_age),nrow(tar_persontbl_min_age))/2)), lty=3) 
legend("topright",c("Std.Male", "Std.Female",
              "Tar.Male","Tar.Female"),lwd=c(2,2,2,2),lty=c(1,1,5,5),cex = 2.0,col=c("blue","red","cyan4","brown"))

#Graph Save
dev.off()
################################################################################
# year_of_birth (max)
################################################################################
jpeg(filename = "../images/Person/03.Person_max_year.jpg",
     width = 720, height = 720, quality = 75, bg = "white")
par(mfrow = c(1,1),xpd=F)
#Divide ratio by Gender
male_max_ratio <- std_persontbl_max_age$ratio[std_persontbl_max_age$genderConceptId == '8507']
female_max_ratio <- std_persontbl_max_age$ratio[std_persontbl_max_age$genderConceptId == '8532']
compared_male_max_ratio <- tar_persontbl_max_age$ratio[tar_persontbl_max_age$genderConceptId == '8507']
compared_female_max_ratio <- tar_persontbl_max_age$ratio[tar_persontbl_max_age$genderConceptId == '8532']
#Set label which got more long length
if(length(std_persontbl_max_age$ageRange[std_persontbl_max_age$genderConceptId == '8507'])
   >length(tar_persontbl_max_age$ageRange[tar_persontbl_max_age$genderConceptId == '8507'])){
  x_max_lbl <- std_persontbl_max_age$ageRange[std_persontbl_max_age$genderConceptId == '8507']
}else{
  x_max_lbl <- tar_persontbl_max_age$ageRange[tar_persontbl_max_age$genderConceptId == '8507']
}
#Draw line Graph
plot(male_max_ratio, type='o',lwd = 2, col=4, xlab="AGE_RANGE", ylab="GENDER_RATIO(%)",axes=F,
     ylim=c(0,as.numeric(max(c(male_max_ratio,female_max_ratio,compared_male_max_ratio,compared_female_max_ratio)))),
     xlim = c(0,length(x_max_lbl)), main="Last visit Person per Year",cex.main=2.0,cex.lab = 1.5)
axis(1,at = c(1:length(x_max_lbl)),labels = x_max_lbl,cex.axis = 1.5)
#Set y_axis which got more big value
if(max(std_persontbl_max_age$ratio) > max(tar_persontbl_max_age$ratio)){
  y_axis <- as.numeric(max(std_persontbl_max_age$ratio))
}else{
  y_axis <- as.numeric(max(tar_persontbl_max_age$ratio))
}
axis(2,at = c(0:ceiling(y_axis)),cex.axis = 1.5)

#add another line graph
lines(female_max_ratio, type='o',lwd = 2, col=2)+
lines(compared_male_max_ratio, type='o', lwd = 2,lty=5, col="cyan4")+
lines(compared_female_max_ratio, type='o',lwd = 2, lty=5, col="brown")
box()
abline(h=c(0:ceiling(as.numeric(max(std_persontbl_max_age$ratio,tar_persontbl_max_age$ratio)))),
       v=c(1:(max(nrow(std_persontbl_max_age),nrow(tar_persontbl_max_age))/2)), lty=3) 
legend("topright",c("Std.Male", "Std.Female",
              "Tar.Male","Tar.Female"),lwd=c(2,2,2,2),lty=c(1,1,5,5),cex = 2.0,col=c("blue","red","cyan4","brown"))

#Graph Save
dev.off()
################################################################################
#race_concept_id
################################################################################
draw_ratio_pie(std_persontbl_race,tar_persontbl_race,"Person/04.Person_race.jpg")
mtext("Compare Race by hospital", font = 2,side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
#ethnicity_concept_id
################################################################################
draw_ratio_pie(std_persontbl_ethnicity,tar_persontbl_ethnicity,"Person/05.Person_ethnicity.jpg")
mtext("Compare Ethnicity by hospital",font=2, side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
#location_concept_id
################################################################################
draw_null_bar(std_persontbl_location,tar_persontbl_location,"Location","Person/06.Person_location.jpg")
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
#provider_concept_id
################################################################################
draw_null_bar(std_persontbl_provider,tar_persontbl_provider,"Provider","Person/07.Person_provider.jpg")
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
#care_site_id
################################################################################
draw_null_bar(std_persontbl_care_site,tar_persontbl_care_site,"Care Site","Person/08.Person_care_site.jpg")
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
#DEATH TABLE VISUALLIZATION
################################################################################
################################################################################
#death_date
################################################################################
if(std_deathtbl_check$attributeName == 1 || std_deathtbl_check$attributeName == "Integrity Data"){
  std_deathtbl_check$attributeName <- "Integrity Data"
}else if(std_deathtbl_check$attributeName > 1 || std_deathtbl_check$attributeName < 1 || std_deathtbl_check$attributeName == "Duplicate time Data"){
  std_deathtbl_check$attributeName <- "Duplicata time Data"
}
if(tar_deathtbl_check$attributeName == 1 || tar_deathtbl_check$attributeName == "Integrity Data"){
  tar_deathtbl_check$attributeName <- "Integrity Data"
}else if(tar_deathtbl_check$attributeName > 1 || tar_deathtbl_check$attributeName < 1 || tar_deathtbl_check$attributeName == "Duplicate time Data"){
  tar_deathtbl_check$attributeName <- "Duplicata time Data"
}
#For legend, Name should be change
draw_ratio_pie(std_deathtbl_check,tar_deathtbl_check,"Death/00.Death_deathcheck.jpg")
mtext("Check Multiple Death time by hospital",font =2, side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
#death_type_concept_id
################################################################################
draw_ratio_pie(std_deathtbl_type,tar_deathtbl_type,"Death/01.Death_type.jpg")
mtext("View Death type by hospital",font= 2, side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
#VISIT OCCURRENCE TABLE VISUALLIZATION
################################################################################
################################################################################
# visit_occurrence_record
################################################################################
draw_table_pie(std_visittbl_record,tar_visittbl_record,"VISIT TABLE","Visit/00.Visit_record.jpg")
mtext("Compare Visit Table Ratio",font=2,side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence_person_id
################################################################################
draw_table_pie(std_visittbl_person_ratio,tar_visittbl_person_ratio,"VISIT PERSON","Visit/01.Visit_person.jpg")
mtext("Compare Visit Person Ratio",font =2,side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence visit_concept_id
# too low value, each label cross
#uncomplete
################################################################################
draw_ratio_pie(std_visittbl_visit_concept,tar_visittbl_visit_concept,"Visit/02.Visit_concept.jpg")
mtext("Compare Visit Concept by hospital",font =2,side=3, line= -5,outer=T, cex = 2.5)
# par(mfrow=c(1,2))
# barplot(matrix(std_visittbl_visit_concept$ratio),legend=std_visittbl_visit_concept$attributeName, col = rainbow(nrow(std_visittbl_visit_concept)))
# barplot(matrix(tar_visittbl_visit_concept$ratio),legend=tar_visittbl_visit_concept$attributeName, col = rainbow(nrow(std_visittbl_visit_concept)))
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
#visit_occurrence_diff_date
################################################################################
#Image file open
jpeg(filename = "../images/Visit/03.Visit_Duration.jpg",
     width = 720, height = 720, quality = 75, bg = "white")

par(mfrow=c(1,2),oma = c(0,0,2,0))
hist(std_visittbl_diff_date$dayDiff, breaks = 25, xlab ="Visit Duration", main = "Standard CDM",cex.main=2.0,cex.axis = 1.5,cex.lab = 1.5)
hist(tar_visittbl_diff_date$dayDiff, breaks = 25, xlab ="Visit Duration", main = "Target CDM",cex.main=2.0,cex.axis = 1.5,cex.lab = 1.5)
title("Visit Duration by Hospital", line = -1, outer = TRUE , cex = 2.0)
#file close
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
#visit_occurrence_start_date
#Using sd
#sd uncomplete
################################################################################
draw_line_start(std_visittbl_start,tar_visittbl_start,"Visit Occurrence","Visit/04.Visit_start.jpg")
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
#visit_occurrence_end_date
#std_visittbl_end error. [1,][2,] no difference
################################################################################
#2999, NA Problem issue
if(length(std_visittbl_end[is.na(std_visittbl_end$visitYear)])!=0){
  std_visittbl_na_end <- std_visittbl_end[is.na(std_visittbl_end$visitYear),2]
  temp_std_s <- 2
}else{
  std_visittbl_na_end <- NA
  temp_std_s <-1
}
if(length(std_visittbl_end[std_visittbl_end$visitYear==2999,]$visitYear)!=0){
  std_visittbl_over_end <- std_visittbl_end[na.omit(std_visittbl_end$visitYear==2999),2]
  temp_std_e <- nrow(std_visittbl_end)-1
}else{
  std_visittbl_over_end <- NA
  temp_std_e <- nrow(std_visittbl_end)
}
if(length(tar_visittbl_end[is.na(tar_visittbl_end$visitYear)])!=0){
  tar_visittbl_na_end <- tar_visittbl_end[is.na(tar_visittbl_end$visitYear),2]
  temp_tar_s <- 2
}else{
  tar_visittbl_na_end <- NA
  temp_tar_s <- 1
}
if(length(tar_visittbl_end[tar_visittbl_end$visitYear==2999,]$visitYear)!=0){
  tar_visittbl_over_end <- tar_visittbl_end[na.omit(tar_visittbl_end$visitYear==2999),2]
  temp_tar_e <- nrow(tar_visittbl_end)-1
}else{
  tar_visittbl_over_end <- NA
  temp_tar_e <- nrow(tar_visittbl_end)
}
visit_na_end<- c(std_visittbl_na_end,tar_visittbl_na_end)
visit_over_end <- c(std_visittbl_over_end,tar_visittbl_over_end)
#Grid line sketch
draw_line_end(std_visittbl_end[temp_std_s:temp_std_e,],tar_visittbl_end[temp_tar_s:temp_tar_e,],visit_na_end,visit_over_end,"Visit Occurrence","Visit/05.Visit_End.jpg")
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
#visit_occurrence_type_concept
################################################################################
draw_ratio_pie(std_visittbl_type_concept,tar_visittbl_type_concept,"Visit/06.Visit_type.jpg")
mtext("Compare Visit type by hospital",font=2,side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
#visit_occurrence care_site
#NHIS data kind problem
#uncomplete
################################################################################
# par(mfrow = c(1,2))
# 
# std_visit_carelbl <- std_visittbl_care_site$attributeName
# std_visit_carelbl <- paste(std_visit_carelbl,"\n",std_visittbl_care_site$ratio)
# std_visit_carelbl <- paste(std_visit_carelbl,"%",seq="")
# 
# std_visit_careslices <- as.numeric(std_visittbl_care_site$ratio)
# #pie3d doesn't work which value is 0.0
# for(i in 1:length(std_visit_careslices)){
#   if(std_visit_careslices[i]==0.0){
#     std_visit_careslices[i] = 0.01
#   }
# }
# pie3D(std_visit_careslices,labels = std_visit_carelbl,explode = 0.03, main="Standard CDM",
#       radius= 0.85, labelcex=0.9, theta=0.8)
# # draw_ratio_pie(std_visittbl_care_site,tar_visittbl_care_site)
# #target CDM
# #Label Setting
# barplot(tar_visittbl_care_site$ratio, main="Target CDM")
# tar_visit_carelbl <- tar_visittbl_care_site$attributeName
# tar_visit_carelbl <- paste(tar_visit_carelbl,"\n",tar_visittbl_care_site$ratio)
# tar_visit_carelbl <- paste(tar_visit_carelbl,"%",seq="")
# 
# tar_visit_careslices <- as.numeric(tar_visittbl_care_site$ratio)
# #pie3d doesn't work which value is 0.0
# # for(i in 1:length(tar_visit_careslices)){
# #   if(tar_visit_careslices[i]==0.0){
# #     tar_visit_careslices[i] = 0.01
# #   }
# # }
# # pie3D(tar_visit_careslices,labels = tar_visit_carelbl,explode = 0.03, main="Target CDM\nCare Site",
# #       radius= 0.85, labelcex=0.9, theta=0.8)
# 
draw_null_bar(std_visittbl_care_site,tar_visittbl_care_site,"Care site","Visit/07.Visit_care_site.jpg")
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
#visit_occurrence visit_source_concept_id
################################################################################
draw_ratio_pie(std_visittbl_source_concept,tar_visittbl_source_concept,"Visit/08.Visit_source.jpg")
mtext("Compare Visit source by hospital",font=2,side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
#visit_occurrence admitting_source_concept_id
#No data in NHIS
################################################################################
draw_ratio_pie(std_visittbl_admitting_source,tar_visittbl_admitting_source,"Visit/09.Visit_admitting.jpg")
mtext("Compare Visit admitting by hospital",font=2,side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
#visit_occurrence discharge_to_concept_id
#No data in NHIS
################################################################################
draw_ratio_pie(std_visittbl_discharge,tar_visittbl_discharge,"Visit/10.Visit_discharge.jpg")
mtext("Compare Visit discharge by hospital",font=2,side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
#visit_occurrence preceding_visit_occurrence_id
#No data in NHIS
################################################################################
draw_compare_pie(std_visittbl_preceding,tar_visittbl_preceding,"Visit/11.Visit_preceding.jpg")
mtext("Compare Visit preceding by hospital",font=2,side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
#CONDITION OCCURRENCE TABLE VISUALLIZATION
################################################################################
################################################################################
# condition_occurrence_record
################################################################################
draw_table_pie(std_conditiontbl_record,tar_conditiontbl_record,"CONDITION\nTABLE","Condition/00.Condition_record.jpg")
mtext("Condition Record Ratio",font=2,side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# condition_occurrence_person_id
################################################################################
draw_table_pie(std_conditiontbl_person_ratio,tar_conditiontbl_person_ratio,"CONDITION\nPERSON","Condition/01.Condition_person.jpg")
mtext("Condition Person Ratio",font=2,side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# condition_occurrence start_date
################################################################################
draw_line_start(std_conditiontbl_start,tar_conditiontbl_start,"Condition","Condition/02.Condition_start.jpg")
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# condition_occurrence end_date
################################################################################
#2999, NA Problem issue
if(length(std_conditiontbl_end[is.na(std_conditiontbl_end$visitYear)])!=0){
  std_conditiontbl_na_end <- std_conditiontbl_end[is.na(std_conditiontbl_end$visitYear),2]
  temp_std_s <- 2
}else{
  std_conditiontbl_na_end <- NA
  temp_std_s <- 1
}
if(length(std_conditiontbl_end[std_conditiontbl_end$visitYear==2999,]$visitYear)!=0){
  std_conditiontbl_over_end <- std_conditiontbl_end[na.omit(std_conditiontbl_end$visitYear==2999),2]
  temp_std_e <- nrow(std_conditiontbl_end)-1
  
}else{
  std_conditiontbl_over_end <- NA
  temp_std_e <- nrow(std_conditiontbl_end)
}
if(length(tar_conditiontbl_end[is.na(tar_conditiontbl_end$visitYear)])!=0){
  tar_conditiontbl_na_end <- tar_conditiontbl_end[is.na(tar_conditiontbl_end$visitYear),2]
  temp_tar_s <- 2
}else{
  tar_conditiontbl_na_end <- NA
  temp_tar_s <- 1
}
if(length(tar_conditiontbl_end[tar_conditiontbl_end$visitYear==2999,]$visitYear)!=0){
  tar_conditiontbl_over_end <- tar_conditiontbl_end[tar_conditiontbl_end$visitYear==2999,2]
  temp_tar_e <- nrow(tar_conditiontbl_end)-1
}else{
  tar_conditiontbl_over_end <- NA
  temp_tar_e <- nrow(tar_conditiontbl_end)-1
}

condition_na_end<- c(std_conditiontbl_na_end,tar_conditiontbl_na_end)
condition_over_end <- c(std_conditiontbl_over_end,tar_conditiontbl_over_end)
draw_line_end(std_conditiontbl_end[temp_std_s:temp_std_e,],tar_conditiontbl_end[temp_tar_s:temp_tar_e,],condition_na_end,condition_over_end,
              "Condition","Condition/03.Condition_End.jpg")
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# condition_occurrence diff_date
################################################################################
jpeg(filename = "../images/Condition/04.Condition_Duration.jpg",
     width = 720, height = 720, quality = 75, bg = "white")
par(mfrow = c(1,2),oma = c(0,0,2,0))
hist(std_conditiontbl_diff_date$dayDiff, breaks = 25, xlab ="Condition Duration", main = "Standard CDM",cex.main=2.0,cex.axis = 1.5,cex.lab = 1.5)
hist(tar_conditiontbl_diff_date$dayDiff, breaks = 25, xlab ="Condition Duration", main = "Target CDM",cex.main=2.0,cex.axis = 1.5,cex.lab = 1.5)
title("Condition Duration by Hospital", outer = T, cex.main=2.0)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# condition_occurrence type_concept_id
################################################################################
draw_ratio_pie(std_conditiontbl_type_concept,tar_conditiontbl_type_concept,"Condition/05.Condition_type.jpg")
mtext("Compare Condition type by hospital",font=2,side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# condition_occurrence stop_reason
################################################################################
draw_count_bar(std_conditiontbl_stop,tar_conditiontbl_stop,"Condition stop reason","Condition/06.Condition Stop.jpg")
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# condition_occurrence visit_occurrence_id
################################################################################
draw_compare_pie(std_conditiontbl_visit_occurrence,tar_conditiontbl_visit_occurrence,"Condition/07.Condition_visit_occurrence.jpg")
mtext("Condition / Visit occurrence ratio by hospital",font=2,side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() 
################################################################################
# condition_occurrence visit_detail_id
################################################################################
draw_compare_pie(std_conditiontbl_visit_detail,tar_conditiontbl_visit_detail,"Condition/08.Condition_visit_detail.jpg")
mtext("Condition/Visit detail ratio by hospital",font=2,side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() 
################################################################################
#DRUG EXPOSURE TABLE VISUALLIZATION
################################################################################
################################################################################
# drug_exposure_record
################################################################################
draw_table_pie(std_drug_exptbl_record,tar_drug_exptbl_record,"DRUG EXP\nTABLE","Drug exposure/00.Drug_exp_record.jpg")
mtext("Drug Exposure Table Ratio",font=2,side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# drug_exposure person_id
################################################################################
draw_table_pie(std_drug_exptbl_person_ratio,tar_drug_exptbl_person_ratio,"DRUG EXP\nPERSON","Drug exposure/01.Drug_exp_person.jpg")
mtext("Drug Exposure Person Ratio",font=2,side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# drug_exposure start_date
################################################################################
draw_line_start(std_drug_exptbl_start,tar_drug_exptbl_start,"Drug Exposure ","Drug Exposure/02.Drug_exp_start.jpg")
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# drug_exposure end_date
################################################################################
#2999, NA Problem issue
#2999, NA Problem issue
if(length(std_drug_exptbl_end[is.na(std_drug_exptbl_end$drug_expYear)])!=0){
  std_drug_exptbl_na_end <- std_drug_exptbl_end[is.na(std_drug_exptbl_end$drug_expYear),2]
  temp_std_s <- 2
}else{
  std_drug_exptbl_na_end <- NA
  temp_std_s <- 1
}
if(length(std_drug_exptbl_end[std_drug_exptbl_end$drug_expYear==2999,]$drug_expYear)!=0){
  std_drug_exptbl_over_end <- std_drug_exptbl_end[std_drug_exptbl_end$drug_expYear==2999,2]
  temp_std_e <- nrow(std_drug_exptbl_end)-1
}else{
  std_drug_exptbl_over_end <- NA
  temp_std_e <- nrow(std_drug_exptbl_end)
}
if(length(tar_drug_exptbl_end[is.na(tar_drug_exptbl_end$drug_expYear)])!=0){
  tar_drug_exptbl_na_end <- tar_drug_exptbl_end[is.na(tar_drug_exptbl_end$drug_expYear),2]
  temp_tar_s <- 2
}else{
  tar_drug_exptbl_na_end <- NA
  temp_tar_s <- 1
}
if(length(tar_drug_exptbl_end[tar_drug_exptbl_end$drug_expYear==2999,]$drug_expYear)!=0){
  tar_drug_exptbl_over_end <- tar_drug_exptbl_end[tar_drug_exptbl_end$drug_expYear==2999,2]
  temp_tar_e <- nrow(tar_drug_exptbl_end)-1
}else{
  tar_drug_exptbl_over_end <- NA
  temp_tar_e <- nrow(tar_drug_exptbl_end)
}
drug_exp_na_end<- c(std_drug_exptbl_na_end,tar_drug_exptbl_na_end)
drug_exp_over_end <- c(std_drug_exptbl_over_end,tar_drug_exptbl_over_end)
draw_line_end(std_drug_exptbl_end[temp_std_s:temp_std_e,],tar_drug_exptbl_end[temp_tar_s:temp_tar_e,],drug_exp_na_end, drug_exp_over_end,
              "Drug Exposure","Drug Exposure/03.Drug_exp_end.jpg")
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# drug_exposure diff_date
################################################################################
jpeg(filename = "../images/Drug Exposure/04.Drug_exp_duration.jpg",
     width = 720, height = 720, quality = 75, bg = "white")
par(mfrow = c(1,2),oma = c(0,0,2,0))
hist(std_conditiontbl_diff_date$dayDiff, breaks = 25, xlab ="Drug exp Duration", main = "Standard CDM",cex.main=2.0,cex.axis = 1.5,cex.lab = 1.5)
hist(tar_conditiontbl_diff_date$dayDiff, breaks = 25, xlab ="Drug exp Duration", main = "Target CDM",cex.main=2.0,cex.axis = 1.5,cex.lab = 1.5)
title("Drug Exposure Duration by Hospital", outer = T, cex.main=2.0)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# drug_exposure type_concept_id
################################################################################
draw_ratio_pie(std_drug_exptbl_type_concept,tar_drug_exptbl_type_concept,"Drug Exposure/05.Drug_exp_type.jpg")
mtext("Drug Exposure Type Concept",font=2,side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# drug_exposure stop_reason
################################################################################
draw_count_bar(std_drug_exptbl_stop,tar_drug_exptbl_stop, "Drug Exposure stop reason","Drug Exposure/06.Drug Stop.jpg")
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# drug_exposure route_concept_id
################################################################################
draw_ratio_pie(std_drug_exptbl_route,tar_drug_exptbl_route,"Drug Exposure/07.Drug_exp_route.jpg")
mtext("Drug Exposure Route Concept",font=2,side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# drug_exposure visit_occurrence_id
################################################################################
draw_compare_pie(std_drug_exptbl_visit_occurrence,tar_drug_exptbl_visit_occurrence,"Drug exposure/08.Drug_exp_visit_occurrence.jpg")
mtext("Drug Exposure/Visit Occurrence Ratio",font=2,side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
#DRUG ERA TABLE VISUALLIZATION
################################################################################
################################################################################
# drug_era_record
################################################################################
draw_table_pie(std_drug_eratbl_record,tar_drug_eratbl_record,"DRUG ERA\nTABLE","Drug era/00.Drug_era_record.jpg")
mtext("Drug Era Table Ratio",font=2,side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# drug_era person_id
################################################################################
draw_table_pie(std_drug_eratbl_person_ratio,tar_drug_eratbl_person_ratio,"DRUG ERA\nPERSON","Drug era/01.Drug_era_person.jpg")
mtext("Drug Era Person Ratio",font=2,side=3, line= -5,outer=T, cex = 2.5)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# drug_era start_date
################################################################################
draw_line_start(std_drug_eratbl_start,tar_drug_eratbl_start, "Drug Era","Drug Era/02.Drug_era_start.jpg")
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# drug_era end_date
################################################################################
#2999, NA Problem issue
if(length(std_drug_eratbl_end[is.na(std_drug_eratbl_end$drug_eraYear)])!=0){
  std_drug_eratbl_na_end <- std_drug_eratbl_end[is.na(std_drug_eratbl_end$drug_eraYear),2]
  temp_std_s <- 2
}else{
  std_drug_eratbl_na_end <- NA
  temp_std_s <- 1
}
if(length(std_drug_eratbl_end[std_drug_eratbl_end$drug_eraYear==2999,]$drug_eraYear)!=0){
  std_drug_eratbl_over_end <- std_drug_eratbl_end[na.omit(std_drug_eratbl_end$drug_eraYear==2999),2]
  temp_std_e <- nrow(std_drug_eratbl_end)-1
}else{
  std_drug_eratbl_over_end <- NA
  temp_std_e <- nrow(std_drug_eratbl_end)
}
if(length(tar_drug_eratbl_end[is.na(tar_drug_eratbl_end$drug_eraYear)])!=0){
  tar_drug_eratbl_na_end <- tar_drug_eratbl_end[is.na(tar_drug_eratbl_end$drug_eraYear),2]
  temp_tar_s <- 2
}else{
  tar_drug_eratbl_na_end <- NA
  temp_tar_s <- 1
}
if(length(tar_drug_eratbl_end[tar_drug_eratbl_end$drug_eraYear==2999,]$drug_eraYear)!=0){
  tar_drug_eratbl_over_end <- tar_drug_eratbl_end[tar_drug_eratbl_end$drug_eraYear==2999,2]
  temp_tar_e <- nrow(tar_drug_eratbl_end)-1
}else{
  tar_drug_eratbl_over_end <- NA
  temp_tar_e <- nrow(tar_drug_eratbl_end)
}#Sometimes there sjlt

drug_era_na_end<- c(std_drug_eratbl_na_end,tar_drug_eratbl_na_end)
drug_era_over_end <- c(std_drug_eratbl_over_end,tar_drug_eratbl_over_end)

draw_line_end(std_drug_eratbl_end[temp_std_s:temp_std_e,],tar_drug_eratbl_end[temp_tar_s:temp_tar_e,],drug_era_na_end, drug_era_over_end,
              "Drug Era","Drug Era/03.Drug_era_end.jpg")

#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# drug_era diff_date
################################################################################
jpeg(filename = "../images/Drug era/04.Drug_era_Duration.jpg",
     width = 720, height = 720, quality = 75, bg = "white")
par(mfrow = c(1,2), oma = c(0,0,2,0))
hist(std_conditiontbl_diff_date$dayDiff, breaks = 25, xlab ="Drug Era Duration", main = "Standard CDM",cex.main=2.0,cex.axis = 1.5,cex.lab = 1.5)
hist(tar_conditiontbl_diff_date$dayDiff, breaks = 25, xlab ="Drug Era Duration", main = "Target CDM",cex.main=2.0,cex.axis = 1.5,cex.lab = 1.5)
title("Drug Era Duration by Hospital", outer =T, cex.main=2.0)
#Graph Save
dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# drug_era exp_count
################################################################################
# draw_ratio_pie(std_drug_eratbl_exp_count,tar_drug_eratbl_exp_count)
# mtext("Drug Exposure Count",font=2,side=3, line= -5,outer=T, cex = 1.5)
# #Graph Save
# dev.copy(device = jpeg ,filename=paste0("images/Drug era/"))
# dev.off() #It protect previous jpg file to not change current jpg image.
################################################################################
# drug_era gap_days
################################################################################
jpeg(filename = "../images/Drug era/05.Drug_era_Gap.jpg",
     width = 720, height = 720, quality = 75, bg = "white")
par(mfrow = c(1,2), oma = c(0,0,2,0))
hist(std_drug_eratbl_gap_days$personRatio, breaks = 25, xlab ="Gap Days Duration", main = "Standard CDM",cex.main=2.0,cex.axis = 1.5,cex.lab = 1.5)
hist(tar_drug_eratbl_gap_days$personRatio, breaks = 25, xlab ="Gap Days Duration", main = "Target CDM",cex.main=2.0,cex.axis = 1.5,cex.lab = 1.5)
title("Drug Era Gap Day by Hospital",outer=T,cex.main=2.0)
dev.off() #It protect previous jpg file to not change current jpg image.