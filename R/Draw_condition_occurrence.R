#' Draw condition occurrence
#'
#' This function for draw graph from condition occurrence RDS data
#' @keywords gemini
#' @export
#'
################################################################################
# CONDITION OCCURRENCE TABLE VISUALLIZATION
################################################################################
draw_condition_occurrence <- function(){
    message("Condition occurrence data visualizing...")
################################################################################
# condition_occurrence_record
################################################################################
draw_table_pie(std_conditiontbl_record, tar_conditiontbl_record, "CONDITION\nTABLE", "Condition/00.Condition_record.jpg")
mtext("Comparison of records ratio between institutions", font = 2, side = 3, line = -5, outer = T, cex = 2.0)
mtext(paste0("count : ",std_conditiontbl_record$count), side = 1, line = -15, at=0.75, outer = T, cex = 1.5)
mtext(paste0("count : ",tar_conditiontbl_record$count), side = 1, line = -15, at=0.25, outer = T, cex = 1.5)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# condition_occurrence_person_id
################################################################################
draw_table_pie(std_conditiontbl_person_ratio, tar_conditiontbl_person_ratio, "CONDITION\nPERSON", "Condition/01.Condition_person.jpg")
mtext("Comparison of person ratio between institutions", font = 2, side = 3, line = -5, outer = T, cex = 2.0)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# condition_occurrence start_date
################################################################################
draw_line_start(std_conditiontbl_start, tar_conditiontbl_start, "Condition", "Condition/02.Condition_start.jpg")
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# condition_occurrence end_date
################################################################################
# 2999, NA Problem issue
if (length(std_conditiontbl_end[is.na(std_conditiontbl_end$visitYear)]) != 0) {
    std_conditiontbl_na_end <<- std_conditiontbl_end[is.na(std_conditiontbl_end$visitYear), 2]
    temp_std_s <<- 2
} else {
    std_conditiontbl_na_end <<- NA
    temp_std_s <<- 1
}
if (length(std_conditiontbl_end[std_conditiontbl_end$visitYear == 2999, ]$visitYear) != 0) {
    std_conditiontbl_over_end <<- std_conditiontbl_end[na.omit(std_conditiontbl_end$visitYear == 2999), 2]
    temp_std_e <<- nrow(std_conditiontbl_end) - 1
} else {
    std_conditiontbl_over_end <<- NA
    temp_std_e <<- nrow(std_conditiontbl_end)
}
if (length(tar_conditiontbl_end[is.na(tar_conditiontbl_end$visitYear)]) != 0) {
    tar_conditiontbl_na_end <<- tar_conditiontbl_end[is.na(tar_conditiontbl_end$visitYear), 2]
    temp_tar_s <<- 2
} else {
    tar_conditiontbl_na_end <<- NA
    temp_tar_s <<- 1
}
if (length(tar_conditiontbl_end[tar_conditiontbl_end$visitYear == 2999, ]$visitYear) != 0) {
    tar_conditiontbl_over_end <<- tar_conditiontbl_end[tar_conditiontbl_end$visitYear == 2999, 2]
    temp_tar_e <<- nrow(tar_conditiontbl_end) - 1
} else {
    tar_conditiontbl_over_end <<- NA
    temp_tar_e <<- nrow(tar_conditiontbl_end) - 1
}

condition_na_end <<- c(std_conditiontbl_na_end, tar_conditiontbl_na_end)
condition_over_end <<- c(std_conditiontbl_over_end, tar_conditiontbl_over_end)
draw_line_end(
    std_conditiontbl_end[temp_std_s:temp_std_e, ], tar_conditiontbl_end[temp_tar_s:temp_tar_e, ], condition_na_end, condition_over_end,
    "Condition", "Condition/03.Condition_end.jpg"
)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# condition_occurrence diff_date
################################################################################
jpeg(
    filename = "images/Condition/04.Condition_duration.jpg",
    width = 720, height = 720, quality = 75, bg = "white"
)
par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
tryCatch(
    hist(std_conditiontbl_diff_date$dayDiff, breaks = 25, xlab = "Condition Duration", main = "A CDM", cex.main = 2.0, cex.axis = 1.5, cex.lab = 1.5)
    , # If data isn't exist...
    error = function(error_message) {
        print(error_message)
        afterError()
    }
)
tryCatch(
    hist(tar_conditiontbl_diff_date$dayDiff, breaks = 25, xlab = "Condition Duration", main = "B CDM", cex.main = 2.0, cex.axis = 1.5, cex.lab = 1.5)
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
# condition_occurrence type_concept_id
################################################################################
draw_ratio_pie(std_conditiontbl_type_concept, tar_conditiontbl_type_concept, "Condition/05.Condition_type.jpg")
mtext("Comparison of condition type between institutions", font = 2, side = 3, line = -5, outer = T, cex = 2.0)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# condition_occurrence stop_reason
################################################################################
draw_count_bar(std_conditiontbl_stop, tar_conditiontbl_stop, "Comparison of stop reason between institutions", "Condition/06.Condition_stop.jpg")
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# condition_occurrence visit_occurrence_id
################################################################################
draw_compare_pie(std_conditiontbl_visit_occurrence, tar_conditiontbl_visit_occurrence, "Condition/07.Condition_visit_occurrence.jpg")
mtext("Comparison of condition/visit occurrence between institutions", font = 2, side = 3, line = -5, outer = T, cex = 2.0)
# Graph Save
dev.off()
################################################################################
# condition_occurrence visit_detail_id
################################################################################
draw_compare_pie(std_conditiontbl_visit_detail, tar_conditiontbl_visit_detail, "Condition/08.Condition_visit_detail.jpg")
mtext("Comparison of condition/visit detail between institutions", font = 2, side = 3, line = -5, outer = T, cex = 2.0)
# Graph Save
dev.off()
}
