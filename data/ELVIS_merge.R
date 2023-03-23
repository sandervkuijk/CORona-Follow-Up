
################################################################################
## Merge acute and follow-up data from the ELVIS cohort for CORFU 
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

#orginele data xlsx
library(openxlsx)
setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/ELVIS/dataset archief")
acute <- openxlsx::read.xlsx("ELVIS_acute en vragenlijst data extractie van 26.01.2023.xlsx",
                             sheet = "Export to MUMC")
acute <- acute[, colSums(is.na(acute)) < nrow(acute)]
acute <- acute[, 1:348] 

# names(acute) <- make.unique(names(acute), sep = ".")

#bewerkte data door ons, graag deze gebruiken
#in deze data moet variabel TrFU_AdmDtNW worden toegevoegd!
#dit gaat Gwynteh mogelijk doen. Hierop wachten.
library(haven)
setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/ELVIS")
acute <-  read_spss("ELVIS_acute en vragenlijst data extractie van 09.03.2023 MI.sav",
                    user_na = TRUE)
acute <- acute[, colSums(is.na(acute)) < nrow(acute)] #lege kolommen verwijderen
acute <- acute[, 1:310] #t/m CS1MEDIA 


## Check time intervals
acute$TrFU_AdmDtNW <- as.Date(acute$TrFU_AdmDtNW, origin = "1899-12-30")
acute$proven_infection_date <- as.Date(acute$proven_infection_date, origin = "1899-12-30")
acute$INGEVULD <- as.Date(acute$INGEVULD, origin = "1899-12-30")

acute$first_months <- as.numeric(round(difftime(acute$INGEVULD,
                                 acute$TrFU_AdmDtNW, units = "weeks")/(52/12)))
acute$moment <- ifelse(acute$first_months < 9, 6,
                ifelse(acute$first_months < 15, 12,
                ifelse(acute$first_months < 21, 18, 24)))
table(acute$moment)                      

# Sander: verder met inlezen ieder moment apart (dus totaal xlsx, dan patnr t/m
# einde vragenlijst). Plaats alles onder elkaar, dan naast elkaar. Haal kolomnamen ui
# andere sets. Dit gaat enkel over acuut/ baseline waarschijnlijk.