install.packages(c("tidyverse", "rvest", "xml2", "fuzzyjoin", 
                   "lpSolve", "devtools", "knitr", "ggrepel", 
                   "hrbrthemes", "palmerpenguins", "gt", "factoextra"))

library(devtools)

install_github("cran/dummies")

lapply(c("tidyverse", "rvest", "xml2", "fuzzyjoin", 
         "lpSolve", "dummies", "knitr", "ggrepel", "devtools", 
         "hrbrthemes", "palmerpenguins", "gt", "factoextra"), require, character.only = TRUE)
