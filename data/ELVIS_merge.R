
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
library(openxlsx)
library(haven)
library(sjmisc)

# names(acute) <- make.unique(names(acute), sep = ".")

#bewerkte data door ons, graag deze gebruiken
#in deze data moet variabel TrFU_AdmDtNW worden toegevoegd!
#dit gaat Gwynteh mogelijk doen. Hierop wachten.
# 
# setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/ELVIS")
# acute <-  read_spss("ELVIS_acute en vragenlijst data extractie van 09.03.2023 MI.sav",
#                     user_na = TRUE)
# acute <- acute[, colSums(is.na(acute)) < nrow(acute)] #lege kolommen verwijderen
# acute <- acute[, 1:310] #t/m CS1MEDIA 

# Orginal data xlsx
setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/ELVIS/dataset archief")
full <- openxlsx::read.xlsx("ELVIS_acute en vragenlijst data extractie van 26.01.2023.xlsx",
                            sheet = "Export to MUMC")
full$TrFU_AdmDtNW <- as.Date(full$TrFU_AdmDtNW, origin = "1899-12-30")
full$proven_infection_date <- as.Date(full$proven_infection_date, origin = "1899-12-30")

full$INGEVULD   <- as.Date(full$INGEVULD, origin = "1899-12-30")
full$INGEVULD.1 <- as.Date(full$INGEVULD.1, origin = "1899-12-30")
full$INGEVULD.2 <- as.Date(full$INGEVULD.2, origin = "1899-12-30")

full <- full[, colSums(is.na(full)) < nrow(full)]

first <- full[, c(1:125, 135:348)] 
first$months <- as.numeric(round(difftime(first$INGEVULD,
                                 first$TrFU_AdmDtNW, units = "weeks")/(52/12)))
first$moment <- ifelse(first$months < 9, 6,
                       ifelse(first$months < 15, 12,
                              ifelse(first$months < 21, 18, 24)))


# Sander: verder met inlezen ieder moment apart (dus totaal xlsx, dan patnr t/m
# einde vragenlijst). Plaats alles onder elkaar, dan naast elkaar. Haal kolomnamen ui
# andere sets. Dit gaat enkel over acuut/ baseline waarschijnlijk.

## Second round of questionnaires
second <- full[, c(1:125, 351:524)]
second$months <- as.numeric(round(difftime(second$INGEVULD.1,
                            second$TrFU_AdmDtNW, units = "weeks")/(52/12)))
second$moment <- ifelse(second$months < 9, 6,
                       ifelse(second$months < 15, 12,
                              ifelse(second$months < 21, 18, 24)))

## Third round of questionnaires
third <- full[, c(1:125, 527:718)]
third$months <- as.numeric(round(difftime(third$INGEVULD.2,
                           third$TrFU_AdmDtNW, units = "weeks")/(52/12)))
third$moment <- ifelse(third$months < 9, 6,
                        ifelse(third$months < 15, 12,
                               ifelse(third$months < 21, 18, 24)))

## Merge to long format dataframe before going wide
# Make sure second and third questionnaires are of same length

ch_sec <- names(second[, grep(".1", names(second), fixed = TRUE, value = FALSE)])
names(second)[grep(".1", names(second), fixed = TRUE, value = FALSE)] <-
substr(ch_sec, 1, nchar(ch_sec)-2)

ch_thi1  <- names(third[, grep(".1", names(third), fixed = TRUE, value = FALSE)])
ch_thi2 <- names(third[, grep(".2", names(third), fixed = TRUE, value = FALSE)])
names(third)[grep(".1", names(third), fixed = TRUE, value = FALSE)] <-
  substr(ch_thi1, 1, nchar(ch_thi1)-2)
names(third)[grep(".2", names(third), fixed = TRUE, value = FALSE)] <-
  substr(ch_thi2, 1, nchar(ch_thi2)-2)

setdiff(names())
