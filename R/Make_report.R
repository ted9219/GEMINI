#' Make report
#'
#' This function render rmd file and execute html file
#' @keywords gemini
#' @export
#'

# Execute R markdown file
make_report<-function(){
    tryCatch({
        file.copy(from = paste0(.libPaths()[1],"/gemini/data/Gemini_md_eng.Rmd"), to = getwd(),overwrite = T)
        rmarkdown::render(paste0(getwd(),"/Gemini_md_eng.Rmd"),encoding = "UTF-8")
        browseURL(url=paste0(getwd(),"/Gemini_md_eng.html"))
        file.copy(from = paste0(.libPaths()[1],"/gemini/data/Gemini_md_kor.Rmd"), to = getwd(),overwrite = T)
        rmarkdown::render(paste0(getwd(),"/Gemini_md_kor.Rmd"),encoding = "UTF-8")
        browseURL(url=paste0(getwd(),"/Gemini_md_kor.html"))
    },error = function(x){
        message("Need Rmd file.")
    })
}
