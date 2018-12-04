################################################################################
# VISIT OCCURRENCE TABLE VISUALLIZATION
################################################################################
################################################################################
# visit_occurrence_record
################################################################################
draw_table_pie(std_visittbl_record, tar_visittbl_record, "VISIT TABLE", "Visit/00.Visit_record.jpg")
mtext("Compare Visit Table Ratio", font = 2, side = 3, line = -5, outer = T, cex = 2.5)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence_person_id
################################################################################
draw_table_pie(std_visittbl_person_ratio, tar_visittbl_person_ratio, "VISIT PERSON", "Visit/01.Visit_person.jpg")
mtext("Compare Visit Person Ratio", font = 2, side = 3, line = -5, outer = T, cex = 2.5)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence visit_concept_id
# too low value, each label cross
# uncomplete
################################################################################
draw_ratio_pie(std_visittbl_visit_concept, tar_visittbl_visit_concept, "Visit/02.Visit_concept.jpg")
mtext("Compare Visit Concept by hospital", font = 2, side = 3, line = -5, outer = T, cex = 2.5)
# par(mfrow=c(1,2))
# barplot(matrix(std_visittbl_visit_concept$ratio),legend=std_visittbl_visit_concept$attributeName, col = rainbow(nrow(std_visittbl_visit_concept)))
# barplot(matrix(tar_visittbl_visit_concept$ratio),legend=tar_visittbl_visit_concept$attributeName, col = rainbow(nrow(std_visittbl_visit_concept)))
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence_diff_date
################################################################################
# Image file open
jpeg(
    filename = "../images/Visit/03.Visit_Duration.jpg",
    width = 720, height = 720, quality = 75, bg = "white"
)

par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
tryCatch(
    hist(std_visittbl_diff_date$dayDiff, breaks = 25, xlab = "Visit Duration", main = "A CDM", cex.main = 2.0, cex.axis = 1.5, cex.lab = 1.5)
    , # If data isn't exist...
    error = function(error_message) {
        print(error_message)
        afterError()
    }
)
tryCatch(
    hist(tar_visittbl_diff_date$dayDiff, breaks = 25, xlab = "Visit Duration", main = "B CDM", cex.main = 2.0, cex.axis = 1.5, cex.lab = 1.5)
    , # If data isn't exist...
    error = function(error_message) {
        print(error_message)
        afterError()
    }
)
title("Visit Duration by Hospital", line = -1, outer = TRUE, cex = 2.0)
# file close
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence_start_date
# Using sd
# sd uncomplete
################################################################################
draw_line_start(std_visittbl_start, tar_visittbl_start, "Visit Occurrence", "Visit/04.Visit_start.jpg")
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence_end_date
# std_visittbl_end error. [1,][2,] no difference
################################################################################
# 2999, NA Problem issue
if (length(std_visittbl_end[is.na(std_visittbl_end$visitYear)]) != 0) {
    std_visittbl_na_end <- std_visittbl_end[is.na(std_visittbl_end$visitYear), 2]
    temp_std_s <- 2
} else {
    std_visittbl_na_end <- NA
    temp_std_s <- 1
}
if (length(std_visittbl_end[std_visittbl_end$visitYear == 2999, ]$visitYear) != 0) {
    std_visittbl_over_end <- std_visittbl_end[na.omit(std_visittbl_end$visitYear == 2999), 2]
    temp_std_e <- nrow(std_visittbl_end) - 1
} else {
    std_visittbl_over_end <- NA
    temp_std_e <- nrow(std_visittbl_end)
}
if (length(tar_visittbl_end[is.na(tar_visittbl_end$visitYear)]) != 0) {
    tar_visittbl_na_end <- tar_visittbl_end[is.na(tar_visittbl_end$visitYear), 2]
    temp_tar_s <- 2
} else {
    tar_visittbl_na_end <- NA
    temp_tar_s <- 1
}
if (length(tar_visittbl_end[tar_visittbl_end$visitYear == 2999, ]$visitYear) != 0) {
    tar_visittbl_over_end <- tar_visittbl_end[na.omit(tar_visittbl_end$visitYear == 2999), 2]
    temp_tar_e <- nrow(tar_visittbl_end) - 1
} else {
    tar_visittbl_over_end <- NA
    temp_tar_e <- nrow(tar_visittbl_end)
}
visit_na_end <- c(std_visittbl_na_end, tar_visittbl_na_end)
visit_over_end <- c(std_visittbl_over_end, tar_visittbl_over_end)
# Grid line sketch
draw_line_end(std_visittbl_end[temp_std_s:temp_std_e, ], tar_visittbl_end[temp_tar_s:temp_tar_e, ], visit_na_end, visit_over_end, "Visit Occurrence", "Visit/05.Visit_End.jpg")
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence_type_concept
################################################################################
draw_ratio_pie(std_visittbl_type_concept, tar_visittbl_type_concept, "Visit/06.Visit_type.jpg")
mtext("Compare Visit type by hospital", font = 2, side = 3, line = -5, outer = T, cex = 2.5)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence care_site
################################################################################
draw_null_bar(std_visittbl_care_site, tar_visittbl_care_site, "Care site", "Visit/07.Visit_care_site.jpg")
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence visit_source_concept_id
################################################################################
draw_ratio_pie(std_visittbl_source_concept, tar_visittbl_source_concept, "Visit/08.Visit_source.jpg")
mtext("Compare Visit source by hospital", font = 2, side = 3, line = -5, outer = T, cex = 2.5)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence admitting_source_concept_id
# No data in NHIS
################################################################################
draw_ratio_pie(std_visittbl_admitting_source, tar_visittbl_admitting_source, "Visit/09.Visit_admitting.jpg")
mtext("Compare Visit admitting by hospital", font = 2, side = 3, line = -5, outer = T, cex = 2.5)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence discharge_to_concept_id
# No data in NHIS
################################################################################
draw_ratio_pie(std_visittbl_discharge, tar_visittbl_discharge, "Visit/10.Visit_discharge.jpg")
mtext("Compare Visit discharge by hospital", font = 2, side = 3, line = -5, outer = T, cex = 2.5)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# visit_occurrence preceding_visit_occurrence_id
# No data in NHIS
################################################################################
draw_compare_pie(std_visittbl_preceding, tar_visittbl_preceding, "Visit/11.Visit_preceding.jpg")
mtext("Compare Visit preceding by hospital", font = 2, side = 3, line = -5, outer = T, cex = 2.5)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.