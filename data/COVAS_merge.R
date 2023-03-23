
################################################################################
## Merge acute and follow-up data from the COVAS cohort for CORFU 
## Sander van Kuijk
##
## Start: 22/03/2022
################################################################################

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

library(haven)
library(dplyr)

setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/BERNHOVEN")

acute <- read_spss("BERNHOVEN_CORFU acute variables CORFU ID_22_2_24.sav",
                   user_na = TRUE)
corfu <- read_spss("220727_CORFU_24mdn_BERNHOVEN_22_2_23.sav",
                   user_na = TRUE)

## Make sure column name and presentation of ID number is equal
acute$cohort_ID <- tolower(acute$cohort_ID)
names(corfu)[1] <- names(acute)[2]
attr(corfu$cohort_ID, which = "label") <- "Cohort ID"
attr(corfu$cohort_ID, which = "format.spss") <- "A8"
attr(corfu$cohort_ID, which = "display_width") <- NULL

## Merge files
all.data <- left_join(acute, corfu, by = "cohort_ID")

## Export as SPSS .sav dataset
setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/BERNHOVEN")
write_sav(all.data, "COVAS_CORFU_data.sav")
rm(all.data)

## End of file.