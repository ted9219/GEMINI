#' GEMINI path setting
#'
#' Path setting only window
#' @keywords gemini
#' @export
#' @example
#' path_set()

path_set <- function(){
    my_wd <- choose.dir()
    setwd(my_wd)
}
