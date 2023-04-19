
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

# ## Function to convert SPSS date to R date
# spss2date <- function(x) as.Date(x/86400, origin = "1582-10-14")

## Read acute-phase data
# setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/MaastrICCht/ACUUT")
# acute <- read.spss("MaastrICCht_acute data_16_3_23.sav", to.data.frame = TRUE,
#                    use.value.labels = TRUE, use.missings = FALSE)

# Problemen met spss-compatibiliteit tussen bronnen
setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/ORGINELE DATA van cohorten/MaastrICCht/ACUUT")
acute <- read.csv("CORFU_MUMC.csv", header = TRUE)
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
names(all.data)[1] <- "cohort_ID"
names(all.data)[names(all.data) == "admission_ckmb"] <- "bio_ckmb"
names(all.data)[names(all.data) == "admission_bnp"] <- "bio_bnp"
names(all.data)[names(all.data) == "treat_reno"] <- "treat_renal"
names(all.data)[names(all.data) == "Participant.Creation.Date.x"] <- "admission_date" # Check with Marieke

names(all.data)[names(all.data) == "C1VACWHC_general#Janssen"] <- "C1VACWHC_1"
names(all.data)[names(all.data) == "C1VACWHC_general#Moderna"] <- "C1VACWHC_2"
names(all.data)[names(all.data) == "C1VACWHC_general#AstraZeneca"] <- "C1VACWHC_3"
names(all.data)[names(all.data) == "C1VACWHC_general#PfizerBioNTech"] <- "C1VACWHC_4"
names(all.data)[names(all.data) == "C1VACWHC_general#Unknow"] <- "C1VACWHC_5"

names(all.data)[names(all.data) == "C1EQ5D_MO3"] <- "C1EQ5DPMO_3mdn"
names(all.data)[names(all.data) == "C1EQ5D_SC3"] <- "C1EQ5DPSC_3mdn"
names(all.data)[names(all.data) == "C1EQ5D_UA3"] <- "C1EQ5DPUA_3mdn"

names(all.data)[names(all.data) == "C1EQ5D_MO6"] <- "C1EQ5DPMO_6mdn"
names(all.data)[names(all.data) == "C1EQ5D_SC6"] <- "C1EQ5DPSC_6mdn"
names(all.data)[names(all.data) == "C1EQ5D_UA6"] <- "C1EQ5DPUA_6mdn"

names(all.data)[names(all.data) == "C1EQ5D_MO12"] <- "C1EQ5DPMO_12mdn"
names(all.data)[names(all.data) == "C1EQ5D_SC12"] <- "C1EQ5DPSC_12mdn"
names(all.data)[names(all.data) == "C1EQ5D_UA12"] <- "C1EQ5DPUA_12mdn"

names(all.data)[names(all.data) == "C1EQ5D_MO18"] <- "C1EQ5DPMO_18mdn"
names(all.data)[names(all.data) == "C1EQ5D_SC18"] <- "C1EQ5DPSC_18mdn"
names(all.data)[names(all.data) == "C1EQ5D_UA18"] <- "C1EQ5DPUA_18mdn"

names(all.data)[names(all.data) == "C1EQ5D_MO24"] <- "C1EQ5DPMO_24mdn"
names(all.data)[names(all.data) == "C1EQ5D_SC24"] <- "C1EQ5DPSC_24mdn"
names(all.data)[names(all.data) == "C1EQ5D_UA24"] <- "C1EQ5DPUA_24mdn"

names(all.data)[names(all.data) == "C1EQ_VAS3"] <- "C1EQ_PVAS_3mdn"
names(all.data)[names(all.data) == "C1EQ_VAS6"] <- "C1EQ_PVAS_6mdn"
names(all.data)[names(all.data) == "C1EQ_VAS12"] <- "C1EQ_PVAS_12mdn"
names(all.data)[names(all.data) == "C1EQ_VAS18"] <- "C1EQ_PVAS_18mdn"
names(all.data)[names(all.data) == "C1EQ_VAS24"] <- "C1EQ_PVAS_24mdn"

all.data$C1EQ5DPPD_3mdn <- ifelse(all.data$'C1EQ5D_PD3#No_issue' == 1, 1,
                           ifelse(all.data$'C1EQ5D_PD3#Some_issue' == 1, 2,
                           ifelse(all.data$'C1EQ5D_PD3#Mild_issue' == 1, 3,
                           ifelse(all.data$'C1EQ5D_PD3#Severe_issue' == 1, 4,
                           ifelse(all.data$'C1EQ5D_PD3#Very_severe_issue' == 1, 5, NA))))) 
all.data$'C1EQ5D_PD3#No_issue'          <- NULL
all.data$'C1EQ5D_PD3#Some_issue'        <- NULL
all.data$'C1EQ5D_PD3#Mild_issue'        <- NULL
all.data$'C1EQ5D_PD3#Severe_issue'      <- NULL
all.data$'C1EQ5D_PD3#Very_severe_issue' <- NULL

all.data$C1EQ5DPPD_6mdn <- ifelse(all.data$'C1EQ5D_PD6#No_issue' == 1, 1,
                           ifelse(all.data$'C1EQ5D_PD6#Some_issue' == 1, 2,
                           ifelse(all.data$'C1EQ5D_PD6#Mild_issue' == 1, 3,
                           ifelse(all.data$'C1EQ5D_PD6#Severe_issue' == 1, 4,
                           ifelse(all.data$'C1EQ5D_PD6#Very_severe_issue' == 1, 5, NA)))))
all.data$'C1EQ5D_PD6#No_issue'          <- NULL
all.data$'C1EQ5D_PD6#Some_issue'        <- NULL
all.data$'C1EQ5D_PD6#Mild_issue'        <- NULL
all.data$'C1EQ5D_PD6#Severe_issue'      <- NULL
all.data$'C1EQ5D_PD6#Very_severe_issue' <- NULL

all.data$C1EQ5DPPD_12mdn <- ifelse(all.data$'C1EQ5D_PD12#No_issue' == 1, 1,
                            ifelse(all.data$'C1EQ5D_PD12#Some_issue' == 1, 2,
                            ifelse(all.data$'C1EQ5D_PD12#Mild_issue' == 1, 3,
                            ifelse(all.data$'C1EQ5D_PD12#Severe_issue' == 1, 4,
                            ifelse(all.data$'C1EQ5D_PD12#Very_severe_issue' == 1, 5, NA)))))
all.data$'C1EQ5D_PD12#No_issue'          <- NULL
all.data$'C1EQ5D_PD12#Some_issue'        <- NULL
all.data$'C1EQ5D_PD12#Mild_issue'        <- NULL
all.data$'C1EQ5D_PD12#Severe_issue'      <- NULL
all.data$'C1EQ5D_PD12#Very_severe_issue' <- NULL

all.data$C1EQ5DPPD_18mdn <- ifelse(all.data$'C1EQ5D_PD18#No_issue' == 1, 1,
                            ifelse(all.data$'C1EQ5D_PD18#Some_issue' == 1, 2,
                            ifelse(all.data$'C1EQ5D_PD18#Mild_issue' == 1, 3,
                            ifelse(all.data$'C1EQ5D_PD18#Severe_issue' == 1, 4,
                            ifelse(all.data$'C1EQ5D_PD18#Very_severe_issue' == 1, 5, NA)))))
all.data$'C1EQ5D_PD18#No_issue'          <- NULL
all.data$'C1EQ5D_PD18#Some_issue'        <- NULL
all.data$'C1EQ5D_PD18#Mild_issue'        <- NULL
all.data$'C1EQ5D_PD18#Severe_issue'      <- NULL
all.data$'C1EQ5D_PD18#Very_severe_issue' <- NULL

all.data$C1EQ5DPPD_24mdn <- ifelse(all.data$'C1EQ5D_PD24#No_issue' == 1, 1,
                            ifelse(all.data$'C1EQ5D_PD24#Some_issue' == 1, 2,
                            ifelse(all.data$'C1EQ5D_PD24#Mild_issue' == 1, 3,
                            ifelse(all.data$'C1EQ5D_PD24#Severe_issue' == 1, 4,
                            ifelse(all.data$'C1EQ5D_PD24#Very_severe_issue' == 1, 5, NA)))))
all.data$'C1EQ5D_PD24#No_issue'          <- NULL
all.data$'C1EQ5D_PD24#Some_issue'        <- NULL
all.data$'C1EQ5D_PD24#Mild_issue'        <- NULL
all.data$'C1EQ5D_PD24#Severe_issue'      <- NULL
all.data$'C1EQ5D_PD24#Very_severe_issue' <- NULL

all.data$C1EQ5DPAD_3mdn <- ifelse(all.data$'C1EQ5D_AD3#No_issue' == 1, 1,
                           ifelse(all.data$'C1EQ5D_AD3#Some_issue' == 1, 2,
                           ifelse(all.data$'C1EQ5D_AD3#Mild_issue' == 1, 3,
                           ifelse(all.data$'C1EQ5D_AD3#Severe_issue' == 1, 4,
                           ifelse(all.data$'C1EQ5D_AD3#Very_severe_issue' == 1, 5, NA)))))
all.data$'C1EQ5D_AD3#No_issue'          <- NULL
all.data$'C1EQ5D_AD3#Some_issue'        <- NULL
all.data$'C1EQ5D_AD3#Mild_issue'        <- NULL
all.data$'C1EQ5D_AD3#Severe_issue'      <- NULL
all.data$'C1EQ5D_AD3#Very_severe_issue' <- NULL

all.data$C1EQ5DPAD_6mdn <- ifelse(all.data$'C1EQ5D_AD6#No_issue' == 1, 1,
                           ifelse(all.data$'C1EQ5D_AD6#Some_issue' == 1, 2,
                           ifelse(all.data$'C1EQ5D_AD6#Mild_issue' == 1, 3,
                           ifelse(all.data$'C1EQ5D_AD6#Severe_issue' == 1, 4,
                           ifelse(all.data$'C1EQ5D_AD6#Very_severe_issue' == 1, 5, NA)))))
all.data$'C1EQ5D_AD6#No_issue'          <- NULL
all.data$'C1EQ5D_AD6#Some_issue'        <- NULL
all.data$'C1EQ5D_AD6#Mild_issue'        <- NULL
all.data$'C1EQ5D_AD6#Severe_issue'      <- NULL
all.data$'C1EQ5D_AD6#Very_severe_issue' <- NULL

all.data$C1EQ5DPAD_12mdn <- ifelse(all.data$'C1EQ5D_AD12#No_issue' == 1, 1,
                            ifelse(all.data$'C1EQ5D_AD12#Some_issue' == 1, 2,
                            ifelse(all.data$'C1EQ5D_AD12#Mild_issue' == 1, 3,
                            ifelse(all.data$'C1EQ5D_AD12#Severe_issue' == 1, 4,
                            ifelse(all.data$'C1EQ5D_AD12#Very_severe_issue' == 1, 5, NA)))))
all.data$'C1EQ5D_AD12#No_issue'          <- NULL
all.data$'C1EQ5D_AD12#Some_issue'        <- NULL
all.data$'C1EQ5D_AD12#Mild_issue'        <- NULL
all.data$'C1EQ5D_AD12#Severe_issue'      <- NULL
all.data$'C1EQ5D_AD12#Very_severe_issue' <- NULL

all.data$C1EQ5DPAD_18mdn <- ifelse(all.data$'C1EQ5D_AD18#No_issue' == 1, 1,
                            ifelse(all.data$'C1EQ5D_AD18#Some_issue' == 1, 2,
                            ifelse(all.data$'C1EQ5D_AD18#Mild_issue' == 1, 3,
                            ifelse(all.data$'C1EQ5D_AD18#Severe_issue' == 1, 4,
                            ifelse(all.data$'C1EQ5D_AD18#Very_severe_issue' == 1, 5, NA)))))
all.data$'C1EQ5D_AD18#No_issue'          <- NULL
all.data$'C1EQ5D_AD18#Some_issue'        <- NULL
all.data$'C1EQ5D_AD18#Mild_issue'        <- NULL
all.data$'C1EQ5D_AD18#Severe_issue'      <- NULL
all.data$'C1EQ5D_AD18#Very_severe_issue' <- NULL

all.data$C1EQ5DPAD_24mdn <- ifelse(all.data$'C1EQ5D_AD24#No_issue' == 1, 1,
                            ifelse(all.data$'C1EQ5D_AD24#Some_issue' == 1, 2,
                            ifelse(all.data$'C1EQ5D_AD24#Mild_issue' == 1, 3,
                            ifelse(all.data$'C1EQ5D_AD24#Severe_issue' == 1, 4,
                            ifelse(all.data$'C1EQ5D_AD24#Very_severe_issue' == 1, 5, NA)))))
all.data$'C1EQ5D_AD24#No_issue'          <- NULL
all.data$'C1EQ5D_AD24#Some_issue'        <- NULL
all.data$'C1EQ5D_AD24#Mild_issue'        <- NULL
all.data$'C1EQ5D_AD24#Severe_issue'      <- NULL
all.data$'C1EQ5D_AD24#Very_severe_issue' <- NULL

all.data$C1SDEMSEX_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1SDEMSEX, NA)
all.data$C1SDEMSEX_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1SDEMSEX, NA)
all.data$C1SDEMSEX_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1SDEMSEX, NA)
all.data$C1SDEMSEX_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1SDEMSEX, NA)
all.data$C1SDEMSEX_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1SDEMSEX, NA)
all.data$C1SDEMSEX <- NULL

all.data$C1SDEMEDU_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1SDEMEDU, NA)
all.data$C1SDEMEDU_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1SDEMEDU, NA)
all.data$C1SDEMEDU_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1SDEMEDU, NA)
all.data$C1SDEMEDU_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1SDEMEDU, NA)
all.data$C1SDEMEDU_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1SDEMEDU, NA)
all.data$C1SDEMEDU <- NULL

all.data$C1SDEMCOU_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1SDEMCOU, NA)
all.data$C1SDEMCOU_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1SDEMCOU, NA)
all.data$C1SDEMCOU_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1SDEMCOU, NA)
all.data$C1SDEMCOU_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1SDEMCOU, NA)
all.data$C1SDEMCOU_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1SDEMCOU, NA)
all.data$C1SDEMCOU <- NULL

all.data$C1SDEMETH_1_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1SDEMETH_1, NA)
all.data$C1SDEMETH_1_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1SDEMETH_1, NA)
all.data$C1SDEMETH_1_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1SDEMETH_1, NA)
all.data$C1SDEMETH_1_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1SDEMETH_1, NA)
all.data$C1SDEMETH_1_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1SDEMETH_1, NA)
all.data$C1SDEMETH_1 <- NULL

all.data$C1SDEMLIV_1_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1SDEMLIV_1, NA)
all.data$C1SDEMLIV_1_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1SDEMLIV_1, NA)
all.data$C1SDEMLIV_1_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1SDEMLIV_1, NA)
all.data$C1SDEMLIV_1_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1SDEMLIV_1, NA)
all.data$C1SDEMLIV_1_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1SDEMLIV_1, NA)
all.data$C1SDEMLIV_1 <- NULL

all.data$C1MORLIST_1_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1MORLIST_1, NA)
all.data$C1MORLIST_1_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1MORLIST_1, NA)
all.data$C1MORLIST_1_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1MORLIST_1, NA)
all.data$C1MORLIST_1_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1MORLIST_1, NA)
all.data$C1MORLIST_1_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1MORLIST_1, NA)
all.data$C1MORLIST_1 <- NULL

all.data$C1MORLIST_2_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1MORLIST_2, NA)
all.data$C1MORLIST_2_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1MORLIST_2, NA)
all.data$C1MORLIST_2_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1MORLIST_2, NA)
all.data$C1MORLIST_2_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1MORLIST_2, NA)
all.data$C1MORLIST_2_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1MORLIST_2, NA)
all.data$C1MORLIST_2 <- NULL

all.data$C1MORLIST_3_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1MORLIST_3, NA)
all.data$C1MORLIST_3_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1MORLIST_3, NA)
all.data$C1MORLIST_3_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1MORLIST_3, NA)
all.data$C1MORLIST_3_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1MORLIST_3, NA)
all.data$C1MORLIST_3_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1MORLIST_3, NA)
all.data$C1MORLIST_3 <- NULL

all.data$C1MORLIST_4_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1MORLIST_4, NA)
all.data$C1MORLIST_4_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1MORLIST_4, NA)
all.data$C1MORLIST_4_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1MORLIST_4, NA)
all.data$C1MORLIST_4_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1MORLIST_4, NA)
all.data$C1MORLIST_4_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1MORLIST_4, NA)
all.data$C1MORLIST_4 <- NULL

all.data$C1MORLIST_5_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1MORLIST_5, NA)
all.data$C1MORLIST_5_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1MORLIST_5, NA)
all.data$C1MORLIST_5_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1MORLIST_5, NA)
all.data$C1MORLIST_5_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1MORLIST_5, NA)
all.data$C1MORLIST_5_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1MORLIST_5, NA)
all.data$C1MORLIST_5 <- NULL

all.data$C1MORLIST_6_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1MORLIST_6, NA)
all.data$C1MORLIST_6_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1MORLIST_6, NA)
all.data$C1MORLIST_6_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1MORLIST_6, NA)
all.data$C1MORLIST_6_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1MORLIST_6, NA)
all.data$C1MORLIST_6_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1MORLIST_6, NA)
all.data$C1MORLIST_6 <- NULL

all.data$C1MORLIST_7_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1MORLIST_7, NA)
all.data$C1MORLIST_7_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1MORLIST_7, NA)
all.data$C1MORLIST_7_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1MORLIST_7, NA)
all.data$C1MORLIST_7_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1MORLIST_7, NA)
all.data$C1MORLIST_7_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1MORLIST_7, NA)
all.data$C1MORLIST_7 <- NULL

all.data$C1MORLIST_8_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1MORLIST_8, NA)
all.data$C1MORLIST_8_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1MORLIST_8, NA)
all.data$C1MORLIST_8_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1MORLIST_8, NA)
all.data$C1MORLIST_8_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1MORLIST_8, NA)
all.data$C1MORLIST_8_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1MORLIST_8, NA)
all.data$C1MORLIST_8 <- NULL

all.data$C1MORLIST_9_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1MORLIST_9, NA)
all.data$C1MORLIST_9_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1MORLIST_9, NA)
all.data$C1MORLIST_9_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1MORLIST_9, NA)
all.data$C1MORLIST_9_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1MORLIST_9, NA)
all.data$C1MORLIST_9_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1MORLIST_9, NA)
all.data$C1MORLIST_9 <- NULL

all.data$C1MORLIST_10_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1MORLIST_10, NA)
all.data$C1MORLIST_10_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1MORLIST_10, NA)
all.data$C1MORLIST_10_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1MORLIST_10, NA)
all.data$C1MORLIST_10_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1MORLIST_10, NA)
all.data$C1MORLIST_10_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1MORLIST_10, NA)
all.data$C1MORLIST_10 <- NULL

all.data$C1MORLIST_11_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1MORLIST_11, NA)
all.data$C1MORLIST_11_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1MORLIST_11, NA)
all.data$C1MORLIST_11_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1MORLIST_11, NA)
all.data$C1MORLIST_11_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1MORLIST_11, NA)
all.data$C1MORLIST_11_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1MORLIST_11, NA)
all.data$C1MORLIST_11 <- NULL

all.data$C1MORLIST_12_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1MORLIST_12, NA)
all.data$C1MORLIST_12_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1MORLIST_12, NA)
all.data$C1MORLIST_12_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1MORLIST_12, NA)
all.data$C1MORLIST_12_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1MORLIST_12, NA)
all.data$C1MORLIST_12_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1MORLIST_12, NA)
all.data$C1MORLIST_12 <- NULL

all.data$C1MORLIST_13_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1MORLIST_13, NA)
all.data$C1MORLIST_13_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1MORLIST_13, NA)
all.data$C1MORLIST_13_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1MORLIST_13, NA)
all.data$C1MORLIST_13_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1MORLIST_13, NA)
all.data$C1MORLIST_13_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1MORLIST_13, NA)
all.data$C1MORLIST_13 <- NULL

all.data$C1MORLIST_14_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1MORLIST_14, NA)
all.data$C1MORLIST_14_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1MORLIST_14, NA)
all.data$C1MORLIST_14_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1MORLIST_14, NA)
all.data$C1MORLIST_14_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1MORLIST_14, NA)
all.data$C1MORLIST_14_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1MORLIST_14, NA)
all.data$C1MORLIST_14 <- NULL

all.data$C1MORLIST_15_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1MORLIST_15, NA)
all.data$C1MORLIST_15_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1MORLIST_15, NA)
all.data$C1MORLIST_15_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1MORLIST_15, NA)
all.data$C1MORLIST_15_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1MORLIST_15, NA)
all.data$C1MORLIST_15_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1MORLIST_15, NA)
all.data$C1MORLIST_15 <- NULL

all.data$C1MORLIST_16_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1MORLIST_16, NA)
all.data$C1MORLIST_16_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1MORLIST_16, NA)
all.data$C1MORLIST_16_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1MORLIST_16, NA)
all.data$C1MORLIST_16_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1MORLIST_16, NA)
all.data$C1MORLIST_16_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1MORLIST_16, NA)
all.data$C1MORLIST_16 <- NULL

all.data$C1MORLIST_17_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1MORLIST_17, NA)
all.data$C1MORLIST_17_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1MORLIST_17, NA)
all.data$C1MORLIST_17_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1MORLIST_17, NA)
all.data$C1MORLIST_17_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1MORLIST_17, NA)
all.data$C1MORLIST_17_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1MORLIST_17, NA)
all.data$C1MORLIST_17 <- NULL

all.data$C1MORLIST_19_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1MORLIST_19, NA)
all.data$C1MORLIST_19_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1MORLIST_19, NA)
all.data$C1MORLIST_19_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1MORLIST_19, NA)
all.data$C1MORLIST_19_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1MORLIST_19, NA)
all.data$C1MORLIST_19_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1MORLIST_19, NA)
all.data$C1MORLIST_19 <- NULL

all.data$C1MORLIST_20_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1MORLIST_20, NA)
all.data$C1MORLIST_20_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1MORLIST_20, NA)
all.data$C1MORLIST_20_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1MORLIST_20, NA)
all.data$C1MORLIST_20_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1MORLIST_20, NA)
all.data$C1MORLIST_20_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1MORLIST_20, NA)
all.data$C1MORLIST_20 <- NULL

all.data$C1HOMHOS_1_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1HOMHOS_1, NA)
all.data$C1HOMHOS_1_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1HOMHOS_1, NA)
all.data$C1HOMHOS_1_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1HOMHOS_1, NA)
all.data$C1HOMHOS_1_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1HOMHOS_1, NA)
all.data$C1HOMHOS_1_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1HOMHOS_1, NA)
all.data$C1HOMHOS_1 <- NULL

all.data$C1HOMHOS_2_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1HOMHOS_2, NA)
all.data$C1HOMHOS_2_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1HOMHOS_2, NA)
all.data$C1HOMHOS_2_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1HOMHOS_2, NA)
all.data$C1HOMHOS_2_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1HOMHOS_2, NA)
all.data$C1HOMHOS_2_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1HOMHOS_2, NA)
all.data$C1HOMHOS_2 <- NULL

all.data$C1HOMHOS_3_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1HOMHOS_3, NA)
all.data$C1HOMHOS_3_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1HOMHOS_3, NA)
all.data$C1HOMHOS_3_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1HOMHOS_3, NA)
all.data$C1HOMHOS_3_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1HOMHOS_3, NA)
all.data$C1HOMHOS_3_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1HOMHOS_3, NA)
all.data$C1HOMHOS_3 <- NULL

all.data$C1VAC_IF_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1VAC_IF, NA)
all.data$C1VAC_IF_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1VAC_IF, NA)
all.data$C1VAC_IF_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1VAC_IF, NA)
all.data$C1VAC_IF_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1VAC_IF, NA)
all.data$C1VAC_IF_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1VAC_IF, NA)
all.data$C1VAC_IF<- NULL

all.data$C1VACWHY_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1VACWHY, NA)
all.data$C1VACWHY_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1VACWHY, NA)
all.data$C1VACWHY_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1VACWHY, NA)
all.data$C1VACWHY_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1VACWHY, NA)
all.data$C1VACWHY_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1VACWHY, NA)
all.data$C1VACWHY<- NULL

all.data$C1VACWN1_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1VACWN1, NA)
all.data$C1VACWN1_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1VACWN1, NA)
all.data$C1VACWN1_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1VACWN1, NA)
all.data$C1VACWN1_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1VACWN1, NA)
all.data$C1VACWN1_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1VACWN1, NA)
all.data$C1VACWN1 <- NULL

all.data$C1VACWN2_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1VACWN2, NA)
all.data$C1VACWN2_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1VACWN2, NA)
all.data$C1VACWN2_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1VACWN2, NA)
all.data$C1VACWN2_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1VACWN2, NA)
all.data$C1VACWN2_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1VACWN2, NA)
all.data$C1VACWN2<- NULL

all.data$C1VACWN3_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1VACWN3, NA)
all.data$C1VACWN3_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1VACWN3, NA)
all.data$C1VACWN3_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1VACWN3, NA)
all.data$C1VACWN3_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1VACWN3, NA)
all.data$C1VACWN3_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1VACWN3, NA)
all.data$C1VACWN3 <- NULL

all.data$C1VACWHC_1_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1VACWHC_1, NA)
all.data$C1VACWHC_1_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1VACWHC_1, NA)
all.data$C1VACWHC_1_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1VACWHC_1, NA)
all.data$C1VACWHC_1_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1VACWHC_1, NA)
all.data$C1VACWHC_1_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1VACWHC_1, NA)
all.data$C1VACWHC_1 <- NULL

all.data$C1VACWHC_2_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1VACWHC_2, NA)
all.data$C1VACWHC_2_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1VACWHC_2, NA)
all.data$C1VACWHC_2_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1VACWHC_2, NA)
all.data$C1VACWHC_2_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1VACWHC_2, NA)
all.data$C1VACWHC_2_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1VACWHC_2, NA)
all.data$C1VACWHC_2 <- NULL

all.data$C1VACWHC_3_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1VACWHC_3, NA)
all.data$C1VACWHC_3_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1VACWHC_3, NA)
all.data$C1VACWHC_3_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1VACWHC_3, NA)
all.data$C1VACWHC_3_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1VACWHC_3, NA)
all.data$C1VACWHC_3_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1VACWHC_3, NA)
all.data$C1VACWHC_3 <- NULL

all.data$C1VACWHC_4_3mdn <- ifelse(all.data$INCLUSIE_3MONTHS == 1, all.data$C1VACWHC_4, NA)
all.data$C1VACWHC_4_6mdn <- ifelse(all.data$INCLUSIE_6MONTHS == 1, all.data$C1VACWHC_4, NA)
all.data$C1VACWHC_4_12mdn <- ifelse(all.data$INCLUSIE_12MONTHS == 1, all.data$C1VACWHC_4, NA)
all.data$C1VACWHC_4_18mdn <- ifelse(all.data$INCLUSIE_18MONTHS == 1, all.data$C1VACWHC_4, NA)
all.data$C1VACWHC_4_24mdn <- ifelse(all.data$INCLUSIE_24MONTHS == 1, all.data$C1VACWHC_4, NA)
all.data$C1VACWHC_4 <- NULL

names(all.data) <- gsub("HEADACHE", "HEA", names(all.data), fixed = TRUE)

names(all.data)[names(all.data) == "C1HLPRLIS_1_3"] <- "C1HLPRLIS_1_3mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_2_3"] <- "C1HLPRLIS_2_3mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_3_3"] <- "C1HLPRLIS_3_3mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_4_3"] <- "C1HLPRLIS_4_3mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_5_3"] <- "C1HLPRLIS_5_3mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_6_3"] <- "C1HLPRLIS_6_3mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_7_3"] <- "C1HLPRLIS_7_3mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_8_3"] <- "C1HLPRLIS_8_3mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_9_3"] <- "C1HLPRLIS_9_3mdn"

names(all.data)[names(all.data) == "C1HLPRLIS_1_6"] <- "C1HLPRLIS_1_6mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_2_6"] <- "C1HLPRLIS_2_6mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_3_6"] <- "C1HLPRLIS_3_6mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_4_6"] <- "C1HLPRLIS_4_6mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_5_6"] <- "C1HLPRLIS_5_6mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_6_6"] <- "C1HLPRLIS_6_6mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_7_6"] <- "C1HLPRLIS_7_6mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_8_6"] <- "C1HLPRLIS_8_6mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_9_6"] <- "C1HLPRLIS_9_6mdn"

names(all.data)[names(all.data) == "C1HLPRLIS_1_12"] <- "C1HLPRLIS_1_12mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_2_12"] <- "C1HLPRLIS_2_12mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_3_12"] <- "C1HLPRLIS_3_12mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_4_12"] <- "C1HLPRLIS_4_12mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_5_12"] <- "C1HLPRLIS_5_12mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_6_12"] <- "C1HLPRLIS_6_12mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_7_12"] <- "C1HLPRLIS_7_12mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_8_12"] <- "C1HLPRLIS_8_12mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_9_12"] <- "C1HLPRLIS_9_12mdn"

names(all.data)[names(all.data) == "C1HLPRLIS_1_18"] <- "C1HLPRLIS_1_18mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_2_18"] <- "C1HLPRLIS_2_18mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_3_18"] <- "C1HLPRLIS_3_18mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_4_18"] <- "C1HLPRLIS_4_18mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_5_18"] <- "C1HLPRLIS_5_18mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_6_18"] <- "C1HLPRLIS_6_18mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_7_18"] <- "C1HLPRLIS_7_18mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_8_18"] <- "C1HLPRLIS_8_18mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_9_18"] <- "C1HLPRLIS_9_18mdn"

names(all.data)[names(all.data) == "C1HLPRLIS_1_24"] <- "C1HLPRLIS_1_24mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_2_24"] <- "C1HLPRLIS_2_24mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_3_24"] <- "C1HLPRLIS_3_24mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_4_24"] <- "C1HLPRLIS_4_24mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_5_24"] <- "C1HLPRLIS_5_24mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_6_24"] <- "C1HLPRLIS_6_24mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_7_24"] <- "C1HLPRLIS_7_24mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_8_24"] <- "C1HLPRLIS_8_24mdn"
names(all.data)[names(all.data) == "C1HLPRLIS_9_24"] <- "C1HLPRLIS_9_24mdn"

names(all.data)[names(all.data) == "C1HLPHLIS_1_3"] <- "C1HLPHLIS_1_3mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_2_3"] <- "C1HLPHLIS_2_3mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_3_3"] <- "C1HLPHLIS_3_3mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_4_3"] <- "C1HLPHLIS_4_3mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_5_3"] <- "C1HLPHLIS_5_3mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_6_3"] <- "C1HLPHLIS_6_3mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_7_3"] <- "C1HLPHLIS_7_3mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_8_3"] <- "C1HLPHLIS_8_3mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_9_3"] <- "C1HLPHLIS_9_3mdn"

names(all.data)[names(all.data) == "C1HLPHLIS_1_6"] <- "C1HLPHLIS_1_6mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_2_6"] <- "C1HLPHLIS_2_6mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_3_6"] <- "C1HLPHLIS_3_6mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_4_6"] <- "C1HLPHLIS_4_6mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_5_6"] <- "C1HLPHLIS_5_6mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_6_6"] <- "C1HLPHLIS_6_6mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_7_6"] <- "C1HLPHLIS_7_6mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_8_6"] <- "C1HLPHLIS_8_6mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_9_6"] <- "C1HLPHLIS_9_6mdn"

names(all.data)[names(all.data) == "C1HLPHLIS_1_12"] <- "C1HLPHLIS_1_12mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_2_12"] <- "C1HLPHLIS_2_12mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_3_12"] <- "C1HLPHLIS_3_12mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_4_12"] <- "C1HLPHLIS_4_12mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_5_12"] <- "C1HLPHLIS_5_12mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_6_12"] <- "C1HLPHLIS_6_12mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_7_12"] <- "C1HLPHLIS_7_12mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_8_12"] <- "C1HLPHLIS_8_12mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_9_12"] <- "C1HLPHLIS_9_12mdn"

names(all.data)[names(all.data) == "C1HLPHLIS_1_18"] <- "C1HLPHLIS_1_18mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_2_18"] <- "C1HLPHLIS_2_18mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_3_18"] <- "C1HLPHLIS_3_18mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_4_18"] <- "C1HLPHLIS_4_18mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_5_18"] <- "C1HLPHLIS_5_18mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_6_18"] <- "C1HLPHLIS_6_18mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_7_18"] <- "C1HLPHLIS_7_18mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_8_18"] <- "C1HLPHLIS_8_18mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_9_18"] <- "C1HLPHLIS_9_18mdn"

names(all.data)[names(all.data) == "C1HLPHLIS_1_24"] <- "C1HLPHLIS_1_24mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_2_24"] <- "C1HLPHLIS_2_24mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_3_24"] <- "C1HLPHLIS_3_24mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_4_24"] <- "C1HLPHLIS_4_24mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_5_24"] <- "C1HLPHLIS_5_24mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_6_24"] <- "C1HLPHLIS_6_24mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_7_24"] <- "C1HLPHLIS_7_24mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_8_24"] <- "C1HLPHLIS_8_24mdn"
names(all.data)[names(all.data) == "C1HLPHLIS_9_24"] <- "C1HLPHLIS_9_24mdn"

names(all.data)[names(all.data) == "C1HLPYLIS_1_3"] <- "C1HLPYLIS_1_3mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_2_3"] <- "C1HLPYLIS_2_3mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_3_3"] <- "C1HLPYLIS_3_3mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_4_3"] <- "C1HLPYLIS_4_3mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_5_3"] <- "C1HLPYLIS_5_3mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_6_3"] <- "C1HLPYLIS_6_3mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_7_3"] <- "C1HLPYLIS_7_3mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_8_3"] <- "C1HLPYLIS_8_3mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_9_3"] <- "C1HLPYLIS_9_3mdn"

names(all.data)[names(all.data) == "C1HLPYLIS_1_6"] <- "C1HLPYLIS_1_6mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_2_6"] <- "C1HLPYLIS_2_6mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_3_6"] <- "C1HLPYLIS_3_6mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_4_6"] <- "C1HLPYLIS_4_6mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_5_6"] <- "C1HLPYLIS_5_6mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_6_6"] <- "C1HLPYLIS_6_6mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_7_6"] <- "C1HLPYLIS_7_6mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_8_6"] <- "C1HLPYLIS_8_6mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_9_6"] <- "C1HLPYLIS_9_6mdn"

names(all.data)[names(all.data) == "C1HLPYLIS_1_12"] <- "C1HLPYLIS_1_12mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_2_12"] <- "C1HLPYLIS_2_12mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_3_12"] <- "C1HLPYLIS_3_12mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_4_12"] <- "C1HLPYLIS_4_12mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_5_12"] <- "C1HLPYLIS_5_12mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_6_12"] <- "C1HLPYLIS_6_12mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_7_12"] <- "C1HLPYLIS_7_12mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_8_12"] <- "C1HLPYLIS_8_12mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_9_12"] <- "C1HLPYLIS_9_12mdn"

names(all.data)[names(all.data) == "C1HLPYLIS_1_18"] <- "C1HLPYLIS_1_18mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_2_18"] <- "C1HLPYLIS_2_18mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_3_18"] <- "C1HLPYLIS_3_18mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_4_18"] <- "C1HLPYLIS_4_18mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_5_18"] <- "C1HLPYLIS_5_18mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_6_18"] <- "C1HLPYLIS_6_18mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_7_18"] <- "C1HLPYLIS_7_18mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_8_18"] <- "C1HLPYLIS_8_18mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_9_18"] <- "C1HLPYLIS_9_18mdn"

names(all.data)[names(all.data) == "C1HLPYLIS_1_24"] <- "C1HLPYLIS_1_24mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_2_24"] <- "C1HLPYLIS_2_24mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_3_24"] <- "C1HLPYLIS_3_24mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_4_24"] <- "C1HLPYLIS_4_24mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_5_24"] <- "C1HLPYLIS_5_24mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_6_24"] <- "C1HLPYLIS_6_24mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_7_24"] <- "C1HLPYLIS_7_24mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_8_24"] <- "C1HLPYLIS_8_24mdn"
names(all.data)[names(all.data) == "C1HLPYLIS_9_24"] <- "C1HLPYLIS_9_24mdn"

names(all.data)[names(all.data) == "C1HADS_01_3"] <- "C1HADS_01_3mdn"
names(all.data)[names(all.data) == "C1HADS_02_3"] <- "C1HADS_02_3mdn"
names(all.data)[names(all.data) == "C1HADS_03_3"] <- "C1HADS_03_3mdn"
names(all.data)[names(all.data) == "C1HADS_04_3"] <- "C1HADS_04_3mdn"
names(all.data)[names(all.data) == "C1HADS_05_3"] <- "C1HADS_05_3mdn"
names(all.data)[names(all.data) == "C1HADS_06_3"] <- "C1HADS_06_3mdn"
names(all.data)[names(all.data) == "C1HADS_07_3"] <- "C1HADS_07_3mdn"
names(all.data)[names(all.data) == "C1HADS_08_3"] <- "C1HADS_08_3mdn"
names(all.data)[names(all.data) == "C1HADS_09_3"] <- "C1HADS_09_3mdn"
names(all.data)[names(all.data) == "C1HADS_10_3"] <- "C1HADS_10_3mdn"
names(all.data)[names(all.data) == "C1HADS_11_3"] <- "C1HADS_11_3mdn"
names(all.data)[names(all.data) == "C1HADS_12_3"] <- "C1HADS_12_3mdn"
names(all.data)[names(all.data) == "C1HADS_13_3"] <- "C1HADS_13_3mdn"
names(all.data)[names(all.data) == "C1HADS_14_3"] <- "C1HADS_14_3mdn"

names(all.data)[names(all.data) == "C1HADS_01_6"] <- "C1HADS_01_6mdn"
names(all.data)[names(all.data) == "C1HADS_02_6"] <- "C1HADS_02_6mdn"
names(all.data)[names(all.data) == "C1HADS_03_6"] <- "C1HADS_03_6mdn"
names(all.data)[names(all.data) == "C1HADS_04_6"] <- "C1HADS_04_6mdn"
names(all.data)[names(all.data) == "C1HADS_05_6"] <- "C1HADS_05_6mdn"
names(all.data)[names(all.data) == "C1HADS_06_6"] <- "C1HADS_06_6mdn"
names(all.data)[names(all.data) == "C1HADS_07_6"] <- "C1HADS_07_6mdn"
names(all.data)[names(all.data) == "C1HADS_08_6"] <- "C1HADS_08_6mdn"
names(all.data)[names(all.data) == "C1HADS_09_6"] <- "C1HADS_09_6mdn"
names(all.data)[names(all.data) == "C1HADS_10_6"] <- "C1HADS_10_6mdn"
names(all.data)[names(all.data) == "C1HADS_11_6"] <- "C1HADS_11_6mdn"
names(all.data)[names(all.data) == "C1HADS_12_6"] <- "C1HADS_12_6mdn"
names(all.data)[names(all.data) == "C1HADS_13_6"] <- "C1HADS_13_6mdn"
names(all.data)[names(all.data) == "C1HADS_14_6"] <- "C1HADS_14_6mdn"

names(all.data)[names(all.data) == "C1HADS_01_12"] <- "C1HADS_01_12mdn"
names(all.data)[names(all.data) == "C1HADS_02_12"] <- "C1HADS_02_12mdn"
names(all.data)[names(all.data) == "C1HADS_03_12"] <- "C1HADS_03_12mdn"
names(all.data)[names(all.data) == "C1HADS_04_12"] <- "C1HADS_04_12mdn"
names(all.data)[names(all.data) == "C1HADS_05_12"] <- "C1HADS_05_12mdn"
names(all.data)[names(all.data) == "C1HADS_06_12"] <- "C1HADS_06_12mdn"
names(all.data)[names(all.data) == "C1HADS_07_12"] <- "C1HADS_07_12mdn"
names(all.data)[names(all.data) == "C1HADS_08_12"] <- "C1HADS_08_12mdn"
names(all.data)[names(all.data) == "C1HADS_09_12"] <- "C1HADS_09_12mdn"
names(all.data)[names(all.data) == "C1HADS_10_12"] <- "C1HADS_10_12mdn"
names(all.data)[names(all.data) == "C1HADS_11_12"] <- "C1HADS_11_12mdn"
names(all.data)[names(all.data) == "C1HADS_12_12"] <- "C1HADS_12_12mdn"
names(all.data)[names(all.data) == "C1HADS_13_12"] <- "C1HADS_13_12mdn"
names(all.data)[names(all.data) == "C1HADS_14_12"] <- "C1HADS_14_12mdn"

names(all.data)[names(all.data) == "C1HADS_01_18"] <- "C1HADS_01_18mdn"
names(all.data)[names(all.data) == "C1HADS_02_18"] <- "C1HADS_02_18mdn"
names(all.data)[names(all.data) == "C1HADS_03_18"] <- "C1HADS_03_18mdn"
names(all.data)[names(all.data) == "C1HADS_04_18"] <- "C1HADS_04_18mdn"
names(all.data)[names(all.data) == "C1HADS_05_18"] <- "C1HADS_05_18mdn"
names(all.data)[names(all.data) == "C1HADS_06_18"] <- "C1HADS_06_18mdn"
names(all.data)[names(all.data) == "C1HADS_07_18"] <- "C1HADS_07_18mdn"
names(all.data)[names(all.data) == "C1HADS_08_18"] <- "C1HADS_08_18mdn"
names(all.data)[names(all.data) == "C1HADS_09_18"] <- "C1HADS_09_18mdn"
names(all.data)[names(all.data) == "C1HADS_10_18"] <- "C1HADS_10_18mdn"
names(all.data)[names(all.data) == "C1HADS_11_18"] <- "C1HADS_11_18mdn"
names(all.data)[names(all.data) == "C1HADS_12_18"] <- "C1HADS_12_18mdn"
names(all.data)[names(all.data) == "C1HADS_13_18"] <- "C1HADS_13_18mdn"
names(all.data)[names(all.data) == "C1HADS_14_18"] <- "C1HADS_14_18mdn"

names(all.data)[names(all.data) == "C1HADS_01_24"] <- "C1HADS_01_24mdn"
names(all.data)[names(all.data) == "C1HADS_02_24"] <- "C1HADS_02_24mdn"
names(all.data)[names(all.data) == "C1HADS_03_24"] <- "C1HADS_03_24mdn"
names(all.data)[names(all.data) == "C1HADS_04_24"] <- "C1HADS_04_24mdn"
names(all.data)[names(all.data) == "C1HADS_05_24"] <- "C1HADS_05_24mdn"
names(all.data)[names(all.data) == "C1HADS_06_24"] <- "C1HADS_06_24mdn"
names(all.data)[names(all.data) == "C1HADS_07_24"] <- "C1HADS_07_24mdn"
names(all.data)[names(all.data) == "C1HADS_08_24"] <- "C1HADS_08_24mdn"
names(all.data)[names(all.data) == "C1HADS_09_24"] <- "C1HADS_09_24mdn"
names(all.data)[names(all.data) == "C1HADS_10_24"] <- "C1HADS_10_24mdn"
names(all.data)[names(all.data) == "C1HADS_11_24"] <- "C1HADS_11_24mdn"
names(all.data)[names(all.data) == "C1HADS_12_24"] <- "C1HADS_12_24mdn"
names(all.data)[names(all.data) == "C1HADS_13_24"] <- "C1HADS_13_24mdn"
names(all.data)[names(all.data) == "C1HADS_14_24"] <- "C1HADS_14_24mdn"

names(all.data)[names(all.data) == "C1EQX_COG3"] <- "C1EQX_COG_3mdn"
names(all.data)[names(all.data) == "C1EQX_COG6"] <- "C1EQX_COG_6mdn"
names(all.data)[names(all.data) == "C1EQX_COG12"] <- "C1EQX_COG_12mdn"
names(all.data)[names(all.data) == "C1EQX_COG18"] <- "C1EQX_COG_18mdn"
names(all.data)[names(all.data) == "C1EQX_COG24"] <- "C1EQX_COG_24mdn"

names(all.data)[names(all.data) == "C1EQX_FAT3"] <- "C1EQX_FAT_3mdn"
names(all.data)[names(all.data) == "C1EQX_FAT6"] <- "C1EQX_FAT_6mdn"
names(all.data)[names(all.data) == "C1EQX_FAT12"] <- "C1EQX_FAT_12mdn"
names(all.data)[names(all.data) == "C1EQX_FAT18"] <- "C1EQX_FAT_18mdn"
names(all.data)[names(all.data) == "C1EQX_FAT24"] <- "C1EQX_FAT_24mdn"

names(all.data)[names(all.data) == "C1EQX_SLP3"] <- "C1EQX_SLP_3mdn"
names(all.data)[names(all.data) == "C1EQX_SLP6"] <- "C1EQX_SLP_6mdn"
names(all.data)[names(all.data) == "C1EQX_SLP12"] <- "C1EQX_SLP_12mdn"
names(all.data)[names(all.data) == "C1EQX_SLP18"] <- "C1EQX_SLP_18mdn"
names(all.data)[names(all.data) == "C1EQX_SLP24"] <- "C1EQX_SLP_24mdn"

names(all.data)[names(all.data) == "C1EQX_APP3"] <- "C1EQX_APP_3mdn"
names(all.data)[names(all.data) == "C1EQX_APP6"] <- "C1EQX_APP_6mdn"
names(all.data)[names(all.data) == "C1EQX_APP12"] <- "C1EQX_APP_12mdn"
names(all.data)[names(all.data) == "C1EQX_APP18"] <- "C1EQX_APP_18mdn"
names(all.data)[names(all.data) == "C1EQX_APP24"] <- "C1EQX_APP_24mdn"

names(all.data)[names(all.data) == "C1EQX_SML3"] <- "C1EQX_SML_3mdn"
names(all.data)[names(all.data) == "C1EQX_SML6"] <- "C1EQX_SML_6mdn"
names(all.data)[names(all.data) == "C1EQX_SML12"] <- "C1EQX_SML_12mdn"
names(all.data)[names(all.data) == "C1EQX_SML18"] <- "C1EQX_SML_18mdn"
names(all.data)[names(all.data) == "C1EQX_SML24"] <- "C1EQX_SML_24mdn"

names(all.data)[names(all.data) == "C1EQX_SML3"] <- "C1EQX_SML_3mdn"
names(all.data)[names(all.data) == "C1EQX_SML6"] <- "C1EQX_SML_6mdn"
names(all.data)[names(all.data) == "C1EQX_SML12"] <- "C1EQX_SML_12mdn"
names(all.data)[names(all.data) == "C1EQX_SML18"] <- "C1EQX_SML_18mdn"
names(all.data)[names(all.data) == "C1EQX_SML24"] <- "C1EQX_SML_24mdn"

names(all.data)[names(all.data) == "C1EQX_HEA3"] <- "C1EQX_HEA_3mdn"
names(all.data)[names(all.data) == "C1EQX_HEA6"] <- "C1EQX_HEA_6mdn"
names(all.data)[names(all.data) == "C1EQX_HEA12"] <- "C1EQX_HEA_12mdn"
names(all.data)[names(all.data) == "C1EQX_HEA18"] <- "C1EQX_HEA_18mdn"
names(all.data)[names(all.data) == "C1EQX_HEA24"] <- "C1EQX_HEA_24mdn"

names(all.data)[names(all.data) == "C1EQX_CHG3"] <- "C1EQX_CHG_3mdn"
names(all.data)[names(all.data) == "C1EQX_CHG6"] <- "C1EQX_CHG_6mdn"
names(all.data)[names(all.data) == "C1EQX_CHG12"] <- "C1EQX_CHG_12mdn"
names(all.data)[names(all.data) == "C1EQX_CHG18"] <- "C1EQX_CHG_18mdn"
names(all.data)[names(all.data) == "C1EQX_CHG24"] <- "C1EQX_CHG_24mdn"

names(all.data)[names(all.data) == "C1EQX_RSP3"] <- "C1EQX_RSP_3mdn"
names(all.data)[names(all.data) == "C1EQX_RSP6"] <- "C1EQX_RSP_6mdn"
names(all.data)[names(all.data) == "C1EQX_RSP12"] <- "C1EQX_RSP_12mdn"
names(all.data)[names(all.data) == "C1EQX_RSP18"] <- "C1EQX_RSP_18mdn"
names(all.data)[names(all.data) == "C1EQX_RSP24"] <- "C1EQX_RSP_24mdn"

names(all.data)[names(all.data) == "C1EQX_PNR3"] <- "C1EQX_PNR_3mdn"
names(all.data)[names(all.data) == "C1EQX_PNR6"] <- "C1EQX_PNR_6mdn"
names(all.data)[names(all.data) == "C1EQX_PNR12"] <- "C1EQX_PNR_12mdn"
names(all.data)[names(all.data) == "C1EQX_PNR18"] <- "C1EQX_PNR_18mdn"
names(all.data)[names(all.data) == "C1EQX_PNR24"] <- "C1EQX_PNR_24mdn"

names(all.data)[names(all.data) == "C1EQX_ANG3"] <- "C1EQX_ANG_3mdn"
names(all.data)[names(all.data) == "C1EQX_ANG6"] <- "C1EQX_ANG_6mdn"
names(all.data)[names(all.data) == "C1EQX_ANG12"] <- "C1EQX_ANG_12mdn"
names(all.data)[names(all.data) == "C1EQX_ANG18"] <- "C1EQX_ANG_18mdn"
names(all.data)[names(all.data) == "C1EQX_ANG24"] <- "C1EQX_ANG_24mdn"

names(all.data)[names(all.data) == "C1EQX_PLP3"] <- "C1EQX_PLP_3mdn"
names(all.data)[names(all.data) == "C1EQX_PLP6"] <- "C1EQX_PLP_6mdn"
names(all.data)[names(all.data) == "C1EQX_PLP12"] <- "C1EQX_PLP_12mdn"
names(all.data)[names(all.data) == "C1EQX_PLP18"] <- "C1EQX_PLP_18mdn"
names(all.data)[names(all.data) == "C1EQX_PLP24"] <- "C1EQX_PLP_24mdn"

names(all.data)[names(all.data) == "C1EQX_DIZ3"] <- "C1EQX_DIZ_3mdn"
names(all.data)[names(all.data) == "C1EQX_DIZ6"] <- "C1EQX_DIZ_6mdn"
names(all.data)[names(all.data) == "C1EQX_DIZ12"] <- "C1EQX_DIZ_12mdn"
names(all.data)[names(all.data) == "C1EQX_DIZ18"] <- "C1EQX_DIZ_18mdn"
names(all.data)[names(all.data) == "C1EQX_DIZ24"] <- "C1EQX_DIZ_24mdn"

names(all.data)[names(all.data) == "C1EQX_ANK3"] <- "C1EQX_ANK_3mdn"
names(all.data)[names(all.data) == "C1EQX_ANK6"] <- "C1EQX_ANK_6mdn"
names(all.data)[names(all.data) == "C1EQX_ANK12"] <- "C1EQX_ANK_12mdn"
names(all.data)[names(all.data) == "C1EQX_ANK18"] <- "C1EQX_ANK_18mdn"
names(all.data)[names(all.data) == "C1EQX_ANK24"] <- "C1EQX_ANK_24mdn"

names(all.data)[names(all.data) == "C1EQX_MSC3"] <- "C1EQX_MSC_3mdn"
names(all.data)[names(all.data) == "C1EQX_MSC6"] <- "C1EQX_MSC_6mdn"
names(all.data)[names(all.data) == "C1EQX_MSC12"] <- "C1EQX_MSC_12mdn"
names(all.data)[names(all.data) == "C1EQX_MSC18"] <- "C1EQX_MSC_18mdn"
names(all.data)[names(all.data) == "C1EQX_MSC24"] <- "C1EQX_MSC_24mdn"

names(all.data)[names(all.data) == "C1EQX_SCF3"] <- "C1EQX_SCF_3mdn"
names(all.data)[names(all.data) == "C1EQX_SCF6"] <- "C1EQX_SCF_6mdn"
names(all.data)[names(all.data) == "C1EQX_SCF12"] <- "C1EQX_SCF_12mdn"
names(all.data)[names(all.data) == "C1EQX_SCF18"] <- "C1EQX_SCF_18mdn"
names(all.data)[names(all.data) == "C1EQX_SCF24"] <- "C1EQX_SCF_24mdn"

names(all.data)[names(all.data) == "C1EQX_CON3"] <- "C1EQX_CON_3mdn"
names(all.data)[names(all.data) == "C1EQX_CON6"] <- "C1EQX_CON_6mdn"
names(all.data)[names(all.data) == "C1EQX_CON12"] <- "C1EQX_CON_12mdn"
names(all.data)[names(all.data) == "C1EQX_CON18"] <- "C1EQX_CON_18mdn"
names(all.data)[names(all.data) == "C1EQX_CON24"] <- "C1EQX_CON_24mdn"

## Date variables
all.data$admission_date <- as.Date(substr(all.data$admission_date, 1, 10),
                                   format = "%d-%m-%Y")
all.data$admission_icu_date <- as.Date(all.data$admission_icu_date,
                                       format = "%d.%m.%Y")
all.data$discharge_icu_date <- as.Date(all.data$discharge_icu_date,
                                       format = "%d.%m.%Y")

## Export as SPSS .sav dataset
# Delete (near-)empty rows
omit <- ifelse(is.na(all.data$INGEVULD_3MONTHS) &
               is.na(all.data$INGEVULD_6MONTHS) &
               is.na(all.data$INGEVULD_12MONTHS) &
               is.na(all.data$INGEVULD_18MONTHS) &
               is.na(all.data$INGEVULD_24MONTHS), 1, 0)
all.data <- subset(all.data, omit == 0)

setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/MaastrICCht/TOTAAL")
write.csv(all.data, "Maastricht_CORFU_data.csv")
write_sav(all.data, "MaastrICCht_CORFU_data.sav")
rm(all.data, omit)

## End of file.