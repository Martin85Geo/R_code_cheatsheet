
# set work dir
setwd('C:')
path <- rstudioapi::getSourceEditorContext()$path
dir  <- dirname(rstudioapi::getSourceEditorContext()$path); dir
setwd(dir)


#### Extract R code from Rmd document

library(knitr)

purl("testcode.Rmd")

#### Extract R code and also include documentation
purl("testcode.Rmd", output = "testcode_t.R", documentation = 2)
