# Execute R markdown file

# install.packages("knitr")
# install.packages("rmarkdown")
library(rmarkdown)
library(knitr)
rmarkdown::render("gemini_md.Rmd",encoding = "UTF-8")
browseURL(url=paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/gemini_md.html"))