
################################################################################
## Merge acute and follow-up data from the MaastrICCht cohort for CORFU 
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

library(foreign)
library(openxlsx)
library(haven)

## Read acute-phase data
setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/MaastrICCht/ACUUT")
acute <- read.spss("MaastrICCht_acute data_7_3_23.sav", to.data.frame = TRUE,
                   use.value.labels = TRUE, use.missings = FALSE)
names(acute)[2] <- "Participant.Id"

## Read CORFU follow-up data
setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/MaastrICCht/FU")
fub <- read.xlsx("COVID19_MUMC_baselineCORFU_14032023.xlsx",
                 sheet = "Study results")
fub <- subset(fub, select = - Participant.Status)

fuv <- read.xlsx("COVID19_MUMC_vaccinationCORFU_14032023.xlsx",
                 sheet = "Study results")
fuv <- subset(fuv, select = - Participant.Status)

fu3 <- read.xlsx("COVID19_MUMC_3monthsCORFU_14032023_clean.xlsx",
                 sheet = "Study results")
fu3$INGEVULD_3MONTHS <- as.Date(fu3$INGEVULD_3MONTHS, format = "%d-%m-%Y")

fu6 <- read.xlsx("COVID19_MUMC_6monthsCORFU_14032023_clean.xlsx",
                 sheet = "Study results")
fu6$INGEVULD_6MONTHS <- as.Date(fu6$INGEVULD_6MONTHS, format = "%d-%m-%Y")
fu6 <- subset(fu6, select = - Site.Abbreviation)

fu12 <- read.xlsx("COVID19_MUMC_12monthsCORFU_14032023_clean.xlsx",
                  sheet = "Study results")
fu12$INGEVULD_12MONTHS <- as.Date(fu12$INGEVULD_12MONTHS, format = "%d-%m-%Y")
fu12 <- subset(fu12, select = - Site.Abbreviation)

fu18 <- read.xlsx("COVID19_MUMC_18monthsCORFU_14032023_clean.xlsx",
                  sheet = "Study results")
fu18$INGEVULD_18MONTHS <- as.Date(fu18$INGEVULD_18MONTHS, format = "%d-%m-%Y")
fu18 <- subset(fu18, select = - Site.Abbreviation)

fu24 <- read.xlsx("COVID19_MUMC_24monthsCORFU_14032023_clean.xlsx",
                  sheet = "Study results")
fu24$INGEVULD_24MONTHS <- as.Date(fu24$INGEVULD_24MONTHS, format = "%d-%m-%Y")
fu24 <- subset(fu24, select = - Site.Abbreviation)

## Merge acute data with all follow-up data
TEMP <- merge(acute, fub, by = "Participant.Id", all = TRUE)
TEMP <- merge(TEMP, fuv, by = "Participant.Id", all = TRUE)
TEMP <- merge(TEMP, fu3, by = "Participant.Id", all = TRUE)
TEMP <- merge(TEMP, fu6, by = "Participant.Id", all = TRUE)
TEMP <- merge(TEMP, fu12, by = "Participant.Id", all = TRUE)
TEMP <- merge(TEMP, fu18, by = "Participant.Id", all = TRUE)
TEMP <- merge(TEMP, fu24, by = "Participant.Id", all = TRUE)
all.data <- TEMP

## Remove redundant dataframes
rm(TEMP, acute, fub, fuv, fu3, fu6, fu12, fu18, fu24)

## Remove empty columns
all.data <- all.data[, colSums(is.na(all.data)) < nrow(all.data)]

## Export as SPSS .sav dataset
setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/MaastrICCht/TOTAAL")
write_sav(all.data, "MaastrICCht_CORFU_data.sav")
rm(all.data)

## End of file.