
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
library(plyr)
library(data.table)

# Complete data
setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/ELVIS")
full <-  read_spss("ELVIS_acute en vragenlijst data extractie van 09.03.2023 MI.sav",
                   user_na = TRUE)
full <- full[, colSums(is.na(full)) < nrow(full)]
full <- subset(full, full$Cohort_ID != "")
full$proven_infection_date <- as.Date(full$proven_infection_date,
                                      origin = "1899-12-30")

# Select acute data and questionnaire data
first <- full[, c(1:310)]
first <- subset(first, first$code != "")
first$months <- as.numeric(round(difftime(first$INGEVULD,
                                 first$admission_hospital_date, units = "weeks")/(52/12)))
first$moment <- ifelse(first$months < 9, 6,
                       ifelse(first$months < 15, 12,
                              ifelse(first$months < 21, 18, 24)))

# Second round of questionnaires
second <- full[, c(1:95, 311:490)]
second <- subset(second, second$code_12mdn != "")
second$months <- as.numeric(round(difftime(second$INGEVULD_12mdn,
                            second$admission_hospital_date, units = "weeks")/(52/12)))
second$moment <- ifelse(second$months < 9, 6,
                       ifelse(second$months < 15, 12,
                              ifelse(second$months < 21, 18, 24)))
second$moment[is.na(second$moment)] <- 12 # Assumption!

# Third round of questionnaires
third <- full[, c(1:95, 491:687)]
third <- subset(third, third$code_24mdn != "")
third$months <- as.numeric(round(difftime(third$INGEVULD_24mdn,
                           third$admission_hospital_date, units = "weeks")/(52/12)))
third$moment <- ifelse(third$months < 9, 6,
                        ifelse(third$months < 15, 12,
                               ifelse(third$months < 21, 18, 24)))
third$moment[is.na(third$moment)] <- 24 # Assumption
rm(full)

## Merge to long format dataframe before going wide
# Make sure second and third questionnaires are of same length

ch_sec <- names(second[, grep("_12mdn", names(second), fixed = TRUE, value = FALSE)])
names(second)[grep("_12mdn", names(second), fixed = TRUE, value = FALSE)] <-
substr(ch_sec, 1, nchar(ch_sec) - 6)

ch_thi <- names(third[, grep("_24mdn", names(third), fixed = TRUE, value = FALSE)])
names(third)[grep("_24mdn", names(third), fixed = TRUE, value = FALSE)] <-
substr(ch_thi, 1, nchar(ch_thi) - 6)

rm(ch_sec, ch_thi)

# Combine all moments in long format
TEMP <- rbind.fill(first, second)
TEMP <- rbind.fill(TEMP, third)
all.data.long <- TEMP
rm(first, second, third, TEMP)

# long to wide
all.data.long$studieid <- NULL
all.data.long$STARTDATE_A <- NULL
all.data.long$CNUM <- NULL # These numbers don't match Cohort_ID!

sum(is.na(all.data.long$admission_hospital_date)) # Will be omitted in next step!
all.data.long <- subset(all.data.long, !is.na(all.data.long$admission_hospital_date))

all.data.long <- all.data.long[!(duplicated(all.data.long[c("Cohort_ID","moment")],
                                            fromLast = FALSE)), ]
all.data.wide <- reshape(all.data.long, idvar = "Cohort_ID",
                         v.names = c("INGEVULD", "code", "months",
                                     grep("C1", names(all.data.long), value = TRUE)),
                         timevar = "moment", direction = "wide")

setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/data_r_sander")
save(all.data.long, file = "elvis_long.rds")

# rm(all.data.long) # May be useful for longitudinal analyses

names(all.data.wide) <- gsub(x = names(all.data.wide), pattern = ".6",
                             replacement = "_6mdn", fixed = TRUE) 
names(all.data.wide) <- gsub(x = names(all.data.wide), pattern = ".12",
                             replacement = "_12mdn", fixed = TRUE) 
names(all.data.wide) <- gsub(x = names(all.data.wide), pattern = ".18",
                             replacement = "_18mdn", fixed = TRUE) 
names(all.data.wide) <- gsub(x = names(all.data.wide), pattern = ".24",
                             replacement = "_24mdn", fixed = TRUE) 

all.data <- all.data.wide[, c(1:95,
                          grep("_6mdn", names(all.data.wide)),
                          grep("_12mdn", names(all.data.wide)),
                          grep("_18mdn", names(all.data.wide)),
                          grep("_24mdn", names(all.data.wide)))]

# Shorten strings with input that is too long.
all.data$C1ENQOTH_6mdn  <- substr(all.data$C1ENQOTH_6mdn, 1, 8)
all.data$C1ENQOTH_12mdn <- substr(all.data$C1ENQOTH_12mdn, 1, 8)
all.data$C1ENQOTH_18mdn <- substr(all.data$C1ENQOTH_18mdn, 1, 8)
all.data$C1ENQOTH_24mdn <- substr(all.data$C1ENQOTH_24mdn, 1, 8)

all.data$C1HLPALL_X_6mdn  <- substr(all.data$C1HLPALL_X_6mdn, 1, 8)
all.data$C1HLPALL_X_12mdn <- substr(all.data$C1HLPALL_X_12mdn, 1, 8)
all.data$C1HLPALL_X_18mdn <- substr(all.data$C1HLPALL_X_18mdn, 1, 8)
all.data$C1HLPALL_X_24mdn <- substr(all.data$C1HLPALL_X_24mdn, 1, 8)

all.data$C1OTHER_X_6mdn   <- substr(all.data$C1OTHER_X_6mdn, 1, 8)
all.data$C1OTHER_X_12mdn  <- substr(all.data$C1OTHER_X_12mdn, 1, 8)
all.data$C1OTHER_X_18mdn  <- substr(all.data$C1OTHER_X_18mdn, 1, 8)
all.data$C1OTHER_X_24mdn  <- substr(all.data$C1OTHER_X_24mdn, 1, 8)

setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/ELVIS/TOTAAL")
write_sav(all.data, "ELVIS_CORFU_FINAL.sav")

### End of file.