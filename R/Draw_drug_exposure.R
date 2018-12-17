#' Draw drug exposure
#'
#' This function for draw graph from drug exposure RDS data
#' @keywords gemini
#' @export
#'
################################################################################
# DRUG EXPOSURE TABLE VISUALLIZATION
################################################################################
draw_drug_exposure <- function(){
    message("Drug exposure data visualizing...")
################################################################################
# drug_exposure_record
################################################################################
draw_table_pie(std_drug_exptbl_record, tar_drug_exptbl_record, "DRUG EXP\nTABLE", "Drug exposure/00.Drug_exp_record.jpg")
mtext("Comparison of records between institutions", font = 2, side = 3, line = -5, outer = T, cex = 2.0)
mtext(paste0("count : ",std_drug_exptbl_record$count), side = 1, line = -15, at=0.75, outer = T, cex = 1.5)
mtext(paste0("count : ",tar_drug_exptbl_record$count), side = 1, line = -15, at=0.25, outer = T, cex = 1.5)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# drug_exposure person_id
################################################################################
draw_table_pie(std_drug_exptbl_person_ratio, tar_drug_exptbl_person_ratio, "DRUG EXP\nPERSON", "Drug exposure/01.Drug_exp_person.jpg")
mtext("Comparison of person ratio between institutions", font = 2, side = 3, line = -5, outer = T, cex = 2.0)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# drug_exposure start_date
################################################################################
draw_line_start(std_drug_exptbl_start, tar_drug_exptbl_start, "Drug Exposure", "Drug exposure/02.Drug_exp_start.jpg")
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# drug_exposure end_date
################################################################################
# 2999, NA Problem issue
# 2999, NA Problem issue
if (length(std_drug_exptbl_end[is.na(std_drug_exptbl_end$visitYear)]) != 0) {
    std_drug_exptbl_na_end <<- std_drug_exptbl_end[is.na(std_drug_exptbl_end$visitYear), 2]
    temp_std_s <<- 2
} else {
    std_drug_exptbl_na_end <<- NA
    temp_std_s <<- 1
}
if (length(std_drug_exptbl_end[std_drug_exptbl_end$visitYear == 2999, ]$visitYear) != 0) {
    std_drug_exptbl_over_end <<- std_drug_exptbl_end[std_drug_exptbl_end$visitYear == 2999, 2]
    temp_std_e <<- nrow(std_drug_exptbl_end) - 1
} else {
    std_drug_exptbl_over_end <<- NA
    temp_std_e <<- nrow(std_drug_exptbl_end)
}
if (length(tar_drug_exptbl_end[is.na(tar_drug_exptbl_end$visitYear)]) != 0) {
    tar_drug_exptbl_na_end <<- tar_drug_exptbl_end[is.na(tar_drug_exptbl_end$visitYear), 2]
    temp_tar_s <<- 2
} else {
    tar_drug_exptbl_na_end <<- NA
    temp_tar_s <<- 1
}
if (length(tar_drug_exptbl_end[tar_drug_exptbl_end$visitYear == 2999, ]$visitYear) != 0) {
    tar_drug_exptbl_over_end <<- tar_drug_exptbl_end[tar_drug_exptbl_end$visitYear == 2999, 2]
    temp_tar_e <<- nrow(tar_drug_exptbl_end) - 1
} else {
    tar_drug_exptbl_over_end <<- NA
    temp_tar_e <<- nrow(tar_drug_exptbl_end)
}
drug_exp_na_end <<- c(std_drug_exptbl_na_end, tar_drug_exptbl_na_end)
drug_exp_over_end <<- c(std_drug_exptbl_over_end, tar_drug_exptbl_over_end)
draw_line_end(
    std_drug_exptbl_end[temp_std_s:temp_std_e, ], tar_drug_exptbl_end[temp_tar_s:temp_tar_e, ], drug_exp_na_end, drug_exp_over_end,
    "Drug Exposure", "Drug Exposure/03.Drug_exp_end.jpg"
)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# drug_exposure diff_date
################################################################################
jpeg(
    filename = "images/Drug exposure/04.Drug_exp_duration.jpg",
    width = 720, height = 720, quality = 75, bg = "white"
)
par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
tryCatch(
    hist(std_conditiontbl_diff_date$dayDiff, breaks = 25, xlab = "Drug exp Duration", main = "A CDM", cex.main = 2.0, cex.axis = 1.5, cex.lab = 1.5)
    , # If data isn't exist...
    error = function(error_message) {
        print(error_message)
        afterError()
    }
)
tryCatch(
    hist(tar_conditiontbl_diff_date$dayDiff, breaks = 25, xlab = "Drug exp Duration", main = "B CDM", cex.main = 2.0, cex.axis = 1.5, cex.lab = 1.5)
    , # If data isn't exist...
    error = function(error_message) {
        print(error_message)
        afterError()
    }
)
title("Comparison of duration between institutions", outer = T, cex.main = 2.0)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# drug_exposure type_concept_id
################################################################################
draw_ratio_pie(std_drug_exptbl_type_concept, tar_drug_exptbl_type_concept, "Drug exposure/05.Drug_exp_type.jpg")
mtext("Comparison of type concept between institutions", font = 2, side = 3, line = -5, outer = T, cex = 2.0)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# drug_exposure stop_reason
################################################################################
draw_count_bar(std_drug_exptbl_stop, tar_drug_exptbl_stop, "Comparison of stop reason between institutions", "Drug Exposure/06.Drug Stop.jpg")
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# drug_exposure route_concept_id
################################################################################
draw_ratio_pie(std_drug_exptbl_route, tar_drug_exptbl_route, "Drug exposure/07.Drug_exp_route.jpg")
mtext("Comparison of route concept between institutions", font = 2, side = 3, line = -5, outer = T, cex = 2.0)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# drug_exposure visit_occurrence_id
################################################################################
draw_compare_pie(std_drug_exptbl_visit_occurrence, tar_drug_exptbl_visit_occurrence, "Drug exposure/08.Drug_exp_visit_occurrence.jpg")
mtext("Comparison of drug exposure/visit occurrence\nbetween institutions", font = 2, side = 3, line = -5, outer = T, cex = 2.0)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
}
