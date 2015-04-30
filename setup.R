library(slidify)
library(slidifyLibraries)

slidify("index.Rmd")
browseURL("index.html")
publish_github("Tuumorigeneetika","tpall")
