
# > sessionInfo()
# R version 4.0.2 (2020-06-22)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 7 x64 (build 7601) Service Pack 1
# 
# Matrix products: default
# 
# locale:
# [1] LC_COLLATE=Dutch_Netherlands.1252  LC_CTYPE=Dutch_Netherlands.1252   
# [3] LC_MONETARY=Dutch_Netherlands.1252 LC_NUMERIC=C                      
# [5] LC_TIME=Dutch_Netherlands.1252    
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base


rm(list = ls())

## Read in all separate cohort files

setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/BERNHOVEN/TOTAAL")
covas <- read.csv("COVAS_CORFU_data.csv", header = TRUE)
covas$cohort <- rep("covas", nrow(covas))

setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/ELVIS/Oude data/TOTAAL")
elvis <- read.csv("ELVIS_CORFU_FINAL.csv", header = TRUE)
elvis$cohort <- rep("elvis", nrow(elvis))

setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/MaastrICCht/TOTAAL")
maast <- read.csv("Maastricht_CORFU_data.csv", header = TRUE)
maast$cohort <- rep("maastriccht", nrow(maast))

## Complete file, all cohorts long
setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data")

capture.output(names(covas), file = "covas.txt")
capture.output(names(elvis), file = "elvis.txt")
capture.output(names(maast), file = "maast.txt")

all.data <- rbind.fill(covas, elvis, maast)

