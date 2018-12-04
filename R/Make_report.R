#' Make report
#'
#' This function render rmd file and execute html file
#' @keywords gemini
#' @export
#' @example
#' make_report()

# Execute R markdown file
make_report<-function(){
    # install.packages("knitr")
    # install.packages("rmarkdown")
    library(rmarkdown)
    library(knitr)
    file.copy(from = paste0(.libPaths()[1],"/gemini/data/Gemini_md.Rmd"), to = getwd())
    rmarkdown::render("Gemini_md.Rmd",encoding = "UTF-8")
    browseURL(url="Gemini_md.html")
}
