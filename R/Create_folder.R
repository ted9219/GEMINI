#' Create folder
#'
#' This function for creating folder
#' @keywords gemini
#' @export
#' @example
#' create_folder()
create_folder<-function(){
    dir.create(file.path(getwd(), "Standard RDS"), showWarnings = FALSE)
    dir.create(file.path(getwd(), "Target RDS"), showWarnings = FALSE)

    dir.create(file.path(getwd(), "images"), showWarnings = FALSE)
    dir.create(file.path(getwd(), "images/Whole"), showWarnings = FALSE)
    dir.create(file.path(getwd(), "images/Person"), showWarnings = FALSE)
    dir.create(file.path(getwd(), "images/Death"), showWarnings = FALSE)
    dir.create(file.path(getwd(), "images/Visit"), showWarnings = FALSE)
    dir.create(file.path(getwd(), "images/Condition"), showWarnings = FALSE)
    dir.create(file.path(getwd(), "images/Drug exposure"), showWarnings = FALSE)
    dir.create(file.path(getwd(), "images/Drug era"), showWarnings = FALSE)
}
