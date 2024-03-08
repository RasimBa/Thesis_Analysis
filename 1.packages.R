pkg <- c("dplyr", "lubridate", "tidyr", "sf", "mice", "stringr", 
         "viridis", "RColorBrewer", "readxl", "knitr", "kableExtra", 
         "gt", "table1", "writexl", "ggplot2", "corrplot", "png", 
         "patchwork", "summarytools", "psych", "broom", "nngeo", 
         "jsonlite", "geojsonio", "units", "car", "stargazer", "rgdal", 
         "fixest", "lmtest", "foreign", "nlme", "spdep", "spatialreg","tibble",
         "flextable","modelsummary")

#load all packages
lapply(pkg, require, character.only = TRUE)