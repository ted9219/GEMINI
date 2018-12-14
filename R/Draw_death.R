#' Draw death
#'
#' This function for draw graph from death RDS data
#' @keywords gemini
#' @export
#'
################################################################################
# DEATH TABLE VISUALLIZATION
################################################################################
draw_death <- function(){
    message("Death data visualizing...")
################################################################################
# death_date
################################################################################
if (std_deathtbl_check$attributeName == 1 || std_deathtbl_check$attributeName == "Integrity Data") {
    std_deathtbl_check$attributeName <<- "Integrity Data"
} else if (std_deathtbl_check$attributeName > 1 || std_deathtbl_check$attributeName < 1 || std_deathtbl_check$attributeName == "Duplicate time Data") {
    std_deathtbl_check$attributeName <<- "Duplicata time Data"
}
if (tar_deathtbl_check$attributeName == 1 || tar_deathtbl_check$attributeName == "Integrity Data") {
    tar_deathtbl_check$attributeName <<- "Integrity Data"
} else if (tar_deathtbl_check$attributeName > 1 || tar_deathtbl_check$attributeName < 1 || tar_deathtbl_check$attributeName == "Duplicate time Data") {
    tar_deathtbl_check$attributeName <<- "Duplicata time Data"
}
# For legend, Name should be change
draw_ratio_pie(std_deathtbl_check, tar_deathtbl_check, "Death/00.Death_deathcheck.jpg")
mtext("Check Multiple Death time by hospital", font = 2, side = 3, line = -5, outer = T, cex = 2.5)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
################################################################################
# death_type_concept_id
################################################################################
draw_ratio_pie(std_deathtbl_type, tar_deathtbl_type, "Death/01.Death_type.jpg")
mtext("View Death type by hospital", font = 2, side = 3, line = -5, outer = T, cex = 2.5)
# Graph Save
dev.off() # It protect previous jpg file to not change current jpg image.
}
