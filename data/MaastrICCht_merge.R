
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

rm(list = ls())

library(foreign)
library(openxlsx)
library(haven)

## Read acute-phase data
setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/MaastrICCht/ACUUT")
acute <- read.spss("MaastrICCht_acute data_16_3_23.sav", to.data.frame = TRUE,
                   use.value.labels = TRUE, use.missings = FALSE)
names(acute)[2] <- "Participant.Id"

## Read CORFU follow-up data
setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/MaastrICCht/FU")
fub <- openxlsx::read.xlsx("COVID19_MUMC_baselineCORFU_14032023.xlsx",
                           sheet = "Study results")
fub <- subset(fub, select = - Participant.Status)
fub[, 4:33] = apply(fub[, 4:33], 2, function(x) as.numeric(as.character(x)))

fuv <- openxlsx::read.xlsx("COVID19_MUMC_vaccinationCORFU_14032023.xlsx",
                           sheet = "Study results")
fuv <- subset(fuv, select = - Participant.Status)
fuv[, 4:13] = apply(fuv[, 4:13], 2, function(x) as.numeric(as.character(x)))

fu3 <- openxlsx::read.xlsx("COVID19_MUMC_3monthsCORFU_14032023_clean.xlsx",
                           sheet = "Study results")
fu3$INGEVULD_3MONTHS <- as.Date(fu3$INGEVULD_3MONTHS, format = "%d-%m-%Y")
fu3[, 5:172] = apply(fu3[, 5:172], 2, function(x) as.numeric(as.character(x)))

fu6 <- openxlsx::read.xlsx("COVID19_MUMC_6monthsCORFU_14032023_clean.xlsx",
                           sheet = "Study results")
fu6$INGEVULD_6MONTHS <- as.Date(fu6$INGEVULD_6MONTHS, format = "%d-%m-%Y")
fu6 <- subset(fu6, select = - Site.Abbreviation)
fu6[, 4:153] = apply(fu6[, 4:153], 2, function(x) as.numeric(as.character(x)))

fu12 <- openxlsx::read.xlsx("COVID19_MUMC_12monthsCORFU_14032023_clean.xlsx",
                            sheet = "Study results")
fu12$INGEVULD_12MONTHS <- as.Date(fu12$INGEVULD_12MONTHS, format = "%d-%m-%Y")
fu12 <- subset(fu12, select = - Site.Abbreviation)
fu12[, 4:153] = apply(fu12[, 4:153], 2, function(x) as.numeric(as.character(x)))

fu18 <- openxlsx::read.xlsx("COVID19_MUMC_18monthsCORFU_14032023_clean.xlsx",
                            sheet = "Study results")
fu18$INGEVULD_18MONTHS <- as.Date(fu18$INGEVULD_18MONTHS, format = "%d-%m-%Y")
fu18 <- subset(fu18, select = - Site.Abbreviation)
fu18[, 4:153] = apply(fu18[, 4:153], 2, function(x) as.numeric(as.character(x)))

fu24 <- openxlsx::read.xlsx("COVID19_MUMC_24monthsCORFU_14032023_clean.xlsx",
                            sheet = "Study results")
fu24$INGEVULD_24MONTHS <- as.Date(fu24$INGEVULD_24MONTHS, format = "%d-%m-%Y")
fu24 <- subset(fu24, select = - Site.Abbreviation)
fu24[, 4:153] = apply(fu24[, 4:153], 2, function(x) as.numeric(as.character(x)))

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

## Remove redundant columns
all.data$Participant.Creation.Date.y <- NULL
all.data$Site.Abbreviation <- NULL
all.data$Site.Abbreviation.x <- NULL
all.data$Site.Abbreviation.y <- NULL

## Column names to CORFU column names
names(all.data)[1] <- "Cohort_ID"
names(all.data)[names(all.data) == "admission_ckmb"] <- "bio_ckmb"
names(all.data)[names(all.data) == "admission_bnp"] <- "bio_bnp"
names(all.data)[names(all.data) == "treat_reno"] <- "treat_renal"
names(all.data)[names(all.data) == "Participant.Creation.Date.x"] <- "admission_date" # Check with Marieke

names(all.data)[names(all.data) == "C1VACWHC_general#Janssen"] <- "C1VACWHC_1"
names(all.data)[names(all.data) == "C1VACWHC_general#Moderna"] <- "C1VACWHC_2"
names(all.data)[names(all.data) == "C1VACWHC_general#AstraZeneca"] <- "C1VACWHC_3"
names(all.data)[names(all.data) == "C1VACWHC_general#PfizerBioNTech"] <- "C1VACWHC_4"
names(all.data)[names(all.data) == "C1VACWHC_general#unknow"] <- "C1VACWHC_5"

names(all.data)[names(all.data) == "C1EQ5D_MO3"] <- "C1EQ5DPMO_3mdn"
names(all.data)[names(all.data) == "C1EQ5D_SC3"] <- "C1EQ5DPSC_3mdn"
names(all.data)[names(all.data) == "C1EQ5D_UA3"] <- "C1EQ5DPUA_3mdn"

names(all.data)[names(all.data) == "C1EQ5D_MO6"] <- "C1EQ5DPMO_6mdn"
names(all.data)[names(all.data) == "C1EQ5D_SC6"] <- "C1EQ5DPSC_6mdn"
names(all.data)[names(all.data) == "C1EQ5D_UA6"] <- "C1EQ5DPUA_6mdn"

names(all.data)[names(all.data) == "C1EQ5D_MO12"] <- "C1EQ5DPMO_12mdn"
names(all.data)[names(all.data) == "C1EQ5D_SC12"] <- "C1EQ5DPSC_12mdn"
names(all.data)[names(all.data) == "C1EQ5D_UA12"] <- "C1EQ5DPUA_21mdn"

names(all.data)[names(all.data) == "C1EQ5D_MO18"] <- "C1EQ5DPMO_18mdn"
names(all.data)[names(all.data) == "C1EQ5D_SC18"] <- "C1EQ5DPSC_18mdn"
names(all.data)[names(all.data) == "C1EQ5D_UA18"] <- "C1EQ5DPUA_18mdn"

names(all.data)[names(all.data) == "C1EQ5D_MO24"] <- "C1EQ5DPMO_24mdn"
names(all.data)[names(all.data) == "C1EQ5D_SC24"] <- "C1EQ5DPSC_24mdn"
names(all.data)[names(all.data) == "C1EQ5D_UA24"] <- "C1EQ5DPUA_24mdn"

all.data$C1EQ5DPPD_3mdn <- ifelse(all.data$'C1EQ5D_PD3#No_issue' == 1, 1,
                           ifelse(all.data$'C1EQ5D_PD3#Some_issue' == 1, 2,
                           ifelse(all.data$'C1EQ5D_PD3#Mild_issue' == 1, 3,
                           ifelse(all.data$'C1EQ5D_PD3#Severe_issue' == 1, 4,
                           ifelse(all.data$'C1EQ5D_PD3#Very_severe_issue' == 1, 5, NA)))))

all.data$C1EQ5DPPD_6mdn <- ifelse(all.data$'C1EQ5D_PD6#No_issue' == 1, 1,
                           ifelse(all.data$'C1EQ5D_PD6#Some_issue' == 1, 2,
                           ifelse(all.data$'C1EQ5D_PD6#Mild_issue' == 1, 3,
                           ifelse(all.data$'C1EQ5D_PD6#Severe_issue' == 1, 4,
                           ifelse(all.data$'C1EQ5D_PD6#Very_severe_issue' == 1, 5, NA)))))

all.data$C1EQ5DPPD_12mdn <- ifelse(all.data$'C1EQ5D_PD12#No_issue' == 1, 1,
                            ifelse(all.data$'C1EQ5D_PD12#Some_issue' == 1, 2,
                            ifelse(all.data$'C1EQ5D_PD12#Mild_issue' == 1, 3,
                            ifelse(all.data$'C1EQ5D_PD12#Severe_issue' == 1, 4,
                            ifelse(all.data$'C1EQ5D_PD12#Very_severe_issue' == 1, 5, NA)))))

all.data$C1EQ5DPPD_18mdn <- ifelse(all.data$'C1EQ5D_PD18#No_issue' == 1, 1,
                            ifelse(all.data$'C1EQ5D_PD18#Some_issue' == 1, 2,
                            ifelse(all.data$'C1EQ5D_PD18#Mild_issue' == 1, 3,
                            ifelse(all.data$'C1EQ5D_PD18#Severe_issue' == 1, 4,
                            ifelse(all.data$'C1EQ5D_PD18#Very_severe_issue' == 1, 5, NA)))))

all.data$C1EQ5DPPD_24mdn <- ifelse(all.data$'C1EQ5D_PD24#No_issue' == 1, 1,
                            ifelse(all.data$'C1EQ5D_PD24#Some_issue' == 1, 2,
                            ifelse(all.data$'C1EQ5D_PD24#Mild_issue' == 1, 3,
                            ifelse(all.data$'C1EQ5D_PD24#Severe_issue' == 1, 4,
                            ifelse(all.data$'C1EQ5D_PD24#Very_severe_issue' == 1, 5, NA)))))

all.data$C1EQ5DPAD_3mdn <- ifelse(all.data$'C1EQ5D_AD3#No_issue' == 1, 1,
                           ifelse(all.data$'C1EQ5D_AD3#Some_issue' == 1, 2,
                           ifelse(all.data$'C1EQ5D_AD3#Mild_issue' == 1, 3,
                           ifelse(all.data$'C1EQ5D_AD3#Severe_issue' == 1, 4,
                           ifelse(all.data$'C1EQ5D_AD3#Very_severe_issue' == 1, 5, NA)))))

all.data$C1EQ5DPAD_6mdn <- ifelse(all.data$'C1EQ5D_AD6#No_issue' == 1, 1,
                           ifelse(all.data$'C1EQ5D_AD6#Some_issue' == 1, 2,
                           ifelse(all.data$'C1EQ5D_AD6#Mild_issue' == 1, 3,
                           ifelse(all.data$'C1EQ5D_AD6#Severe_issue' == 1, 4,
                           ifelse(all.data$'C1EQ5D_AD6#Very_severe_issue' == 1, 5, NA)))))

all.data$C1EQ5DPAD_12mdn <- ifelse(all.data$'C1EQ5D_AD12#No_issue' == 1, 1,
                            ifelse(all.data$'C1EQ5D_AD12#Some_issue' == 1, 2,
                            ifelse(all.data$'C1EQ5D_AD12#Mild_issue' == 1, 3,
                            ifelse(all.data$'C1EQ5D_AD12#Severe_issue' == 1, 4,
                            ifelse(all.data$'C1EQ5D_AD12#Very_severe_issue' == 1, 5, NA)))))

all.data$C1EQ5DPAD_18mdn <- ifelse(all.data$'C1EQ5D_AD18#No_issue' == 1, 1,
                            ifelse(all.data$'C1EQ5D_AD18#Some_issue' == 1, 2,
                            ifelse(all.data$'C1EQ5D_AD18#Mild_issue' == 1, 3,
                            ifelse(all.data$'C1EQ5D_AD18#Severe_issue' == 1, 4,
                            ifelse(all.data$'C1EQ5D_AD18#Very_severe_issue' == 1, 5, NA)))))

all.data$C1EQ5DPAD_24mdn <- ifelse(all.data$'C1EQ5D_AD24#No_issue' == 1, 1,
                            ifelse(all.data$'C1EQ5D_AD24#Some_issue' == 1, 2,
                            ifelse(all.data$'C1EQ5D_AD24#Mild_issue' == 1, 3,
                            ifelse(all.data$'C1EQ5D_AD24#Severe_issue' == 1, 4,
                            ifelse(all.data$'C1EQ5D_AD24#Very_severe_issue' == 1, 5, NA)))))

names(all.data) <- gsub("HEADACHE", "HEA", names(all.data), fixed = TRUE)

## Export as SPSS .sav dataset
setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/MaastrICCht/TOTAAL")
write_sav(all.data, "MaastrICCht_CORFU_data.sav")
rm(all.data)

## End of file.