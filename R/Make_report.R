#' Make report
#'
#' This function render rmd file and execute html file
#' @keywords gemini
#' @export
#'

# Execute R markdown file
make_report<-function(){
    # install.packages("knitr")
    # install.packages("rmarkdown")
    library(rmarkdown)
    library(knitr)
    tryCatch({
        file.copy(from = paste0(.libPaths()[1],"/gemini/data/Gemini_md.Rmd"), to = getwd())
        rmarkdown::render(paste0(getwd(),"/Gemini_md.Rmd"),encoding = "UTF-8")
        browseURL(url=paste0(getwd(),"/Gemini_md.html"))
    },error = function(x){
        message("Need Rmd file.")
    })
}
