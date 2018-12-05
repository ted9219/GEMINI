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
    rmarkdown::render("gemini_md.Rmd",encoding = "UTF-8")
    browseURL(url="gemini_md.html")
}
