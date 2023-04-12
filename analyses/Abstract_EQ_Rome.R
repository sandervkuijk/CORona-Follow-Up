### Session info ####

# R version 4.0.2 (2020-06-22)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 7 x64 (build 7601) Service Pack 1
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=Dutch_Netherlands.1252  LC_CTYPE=Dutch_Netherlands.1252    LC_MONETARY=Dutch_Netherlands.1252
# [4] LC_NUMERIC=C                       LC_TIME=Dutch_Netherlands.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# loaded via a namespace (and not attached):
#   [1] compiler_4.0.2 tools_4.0.2

rm(list = ls())

## Packages ####
library(foreign)
library(eq5d)

## Read-in data ####
setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/ELVIS/TOTAAL")
elvis <- read.spss("ELVIS_CORFU_FINAL.sav", to.data.frame = TRUE)
setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/BERNHOVEN/TOTAAL")
covas <- read.csv("COVAS_CORFU_data.csv", header = TRUE)
setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/MaastrICCht/TOTAAL")
maast <- read.spss("MaastrICCht_CORFU_data.sav", to.data.frame = TRUE)
maast <- subset(maast, !is.na(maast$age_inclusion)) # Good proxy for near-empty row

## Add EQ-5D-5L utility score ####
elviseq <- data.frame(MO = elvis$C1EQ5DPMO_24mdn, SC = elvis$C1EQ5DPSC_24mdn,
                      UA = elvis$C1EQ5DPUA_24mdn, PD = elvis$C1EQ5DPPD_24mdn,
                      AD = elvis$C1EQ5DPAD_24mdn)
elvis$utility <- eq5d(elviseq, version = "5L", type = "VT",
                      country = "Netherlands", ignore.invalid = TRUE)

covaseq <- data.frame(MO = covas$C1EQ5DPMO_24mdn, SC = covas$C1EQ5DPSC_24mdn,
                      UA = covas$C1EQ5DPUA_24mdn, PD = covas$C1EQ5DPPD_24mdn,
                      AD = covas$C1EQ5DPAD_24mdn)
covas$utility <- eq5d(covaseq, version = "5L", type = "VT",
                      country = "Netherlands", ignore.invalid = TRUE)

maasteq <- data.frame(MO = maast$C1EQ5DPMO_24mdn, SC = maast$C1EQ5DPSC_24mdn,
                      UA = maast$C1EQ5DPUA_24mdn, PD = maast$C1EQ5DPPD_24mdn,
                      AD = maast$C1EQ5DPAD_24mdn)
maast$utility <- eq5d(maasteq, version = "5L", type = "VT",
                      country = "Netherlands", ignore.invalid = TRUE)
rm(elviseq, covaseq, maasteq)

## Baseline table ####
ss <- nrow(elvis) + nrow(covas) + nrow(maast); ss

age <- c(elvis$age_inclusion, covas$age_inclusion, maast$age_inclusion)
age[age < 18] <- NA
range(age, na.rm = TRUE)
round(mean(age, na.rm = TRUE), 1)
round(sd(age, na.rm = TRUE), 1)

sex <- c(elvis$sex, covas$sex, maast$sex)
table(sex) # Code book: males 1 
round(prop.table(table(sex))*100, 1)

bmi <- c(elvis$bmi, covas$bmi, maast$bmi)
bmi[bmi > 60] <- NA
bmi[bmi < 15] <- NA
range(bmi, na.rm = TRUE)
round(mean(bmi, na.rm = TRUE), 1)
round(sd(bmi, na.rm = TRUE), 1)

## Add hospital and ICU dummies ####
elvis$icu <- ifelse(!is.na(elvis$admission_icu_date), 1, 0)
covas$icu <- ifelse(!is.na(covas$admission_icu_date), 1, 0)
covas$ward <- ifelse(!is.na(covas$admission_hospital_date) &
                       is.na(covas$admission_icu_date), 1, 0)

# Totals for home, ward, and ICU
sum(covas$icu == 0 & covas$ward == 0)         # Home
sum(elvis$icu != 1) + sum(covas$ward)         # Ward
sum(elvis$icu) + sum(covas$icu) + nrow(maast) # ICU

round((sum(covas$icu == 0 & covas$ward == 0))/ss*100, 1)
round((sum(elvis$icu != 1) + sum(covas$ward))/ss*100, 1)
round((sum(elvis$icu) + sum(covas$icu) + nrow(maast))/ss*100, 1)

## Analyses onHRQoL utility score ####
ut <- c(elvis$utility, covas$utility, maast$utility)
ic <- c(elvis$icu, covas$icu, rep(1, nrow(maast)))
wa <- c(1-elvis$icu, covas$ward, rep(0, nrow(maast)))
util <- data.frame(ut, ic, wa)
util$ho <- ifelse(util$ic == 0 & util$wa == 0, 1, 0)

round(mean(util$ut[util$ho == 1], na.rm = TRUE), 2)
round(sd(util$ut[util$ho == 1], na.rm = TRUE), 2)

round(mean(util$ut[util$wa == 1], na.rm = TRUE), 2)
round(sd(util$ut[util$wa == 1], na.rm = TRUE), 2)

round(mean(util$ut[util$ic == 1], na.rm = TRUE), 2)
round(sd(util$ut[util$ic == 1], na.rm = TRUE), 2)

util$cat <- ifelse(util$wa == 1, 1,
                   ifelse(util$ic == 1, 2, 0))
table(util$cat)
anova(lm(ut ~ factor(cat), data = util))

## Analyses on symptom Fatigue ####
fa <- c(elvis$C1EQX_FAT_24mdn, covas$C1EQX_FAT_24mdn, maast$C1EQX_FAT24)
fati <- data.frame(fa, ic, wa, ut)
fati$ho <- ifelse(fati$ic == 0 & fati$wa == 0, 1, 0)

table(fati$fa > 2, fati$ho)[, 2]
round(prop.table(table(fati$fa > 2, fati$ho), 2)*100, 1)[, 2]

table(fati$fa > 2, fati$ic)[, 2]
round(prop.table(table(fati$fa > 2, fati$ic), 2)*100, 1)[, 2]

table(fati$fa > 2, fati$wa)[, 2]
round(prop.table(table(fati$fa > 2, fati$wa), 2)*100, 1)[, 2]

round(cor.test(fati$fa, fati$ut, method = "spearman")$estimate, 2)

## Analyses on symptom Muscle weakness ####
mw <- c(elvis$C1EQX_MSC_24mdn, covas$C1EQX_MSC_24mdn, maast$C1EQX_MSC24)
musc <- data.frame(mw, ic, wa, ut)
musc$ho <- ifelse(musc$ic == 0 & musc$wa == 0, 1, 0)

table(musc$mw > 2, musc$ho)[, 2]
round(prop.table(table(musc$mw > 2, musc$ho), 2)*100, 1)[, 2]

table(musc$mw > 2, musc$ic)[, 2]
round(prop.table(table(musc$mw > 2, musc$ic), 2)*100, 1)[, 2]

table(musc$mw > 2, musc$wa)[, 2]
round(prop.table(table(musc$mw > 2, musc$wa), 2)*100, 1)[, 2]

round(cor.test(musc$mw, musc$ut, method = "spearman")$estimate, 2)

## Analyses on symptom Problems with respiration ####
re <- c(elvis$C1EQX_RSP_24mdn, covas$C1EQX_RSP_24mdn, maast$C1EQX_RSP24)
resp <- data.frame(re, ic, wa, ut)
resp$ho <- ifelse(resp$ic == 0 & resp$wa == 0, 1, 0)

table(resp$re > 2, resp$ho)[, 2]
round(prop.table(table(resp$re > 2, resp$ho), 2)*100, 1)[, 2]

table(resp$re > 2, resp$ic)[, 2]
round(prop.table(table(resp$re > 2, resp$ic), 2)*100, 1)[, 2]

table(resp$re > 2, resp$wa)[, 2]
round(prop.table(table(resp$re > 2, resp$wa), 2)*100, 1)[, 2]

round(cor.test(resp$re, resp$ut, method = "spearman")$estimate, 2)

## Analyses on symptom Sleep ####
sl <- c(elvis$C1EQX_SLP_24mdn, covas$C1EQX_SLP_24mdn, maast$C1EQX_SLP24)
slee <- data.frame(sl, ic, wa)
slee$ho <- ifelse(slee$ic == 0 & slee$wa == 0, 1, 0)

table(slee$sl > 2, slee$ho)[, 2]
round(prop.table(table(slee$sl > 2, slee$ho), 2)*100, 1)[, 2]

table(slee$sl > 2, slee$ic)[, 2]
round(prop.table(table(slee$sl > 2, slee$ic), 2)*100, 1)[, 2]

table(slee$sl > 2, slee$wa)[, 2]
round(prop.table(table(slee$sl > 2, slee$wa), 2)*100, 1)[, 2]

## Add working definition of Long COVID ####
elvis$longcovid <- ifelse(elvis$C1EQX_FAT_24mdn > 3 |
                          elvis$C1EQX_COG_24mdn > 3 |
                          elvis$C1EQX_SOC_24mdn > 3 |
                          elvis$C1EQX_SLP_24mdn > 3 |
                          elvis$C1EQX_APP_24mdn > 3 |
                          elvis$C1EQX_SML_24mdn > 3 |
                          elvis$C1EQX_CGH_24mdn > 3 |
                          elvis$C1EQX_RSP_24mdn > 3 |
                          elvis$C1EQX_PNR_24mdn > 3 |
                          elvis$C1EQX_ANG_24mdn > 3 |
                          elvis$C1EQX_PLP_24mdn > 3 |
                          elvis$C1EQX_DIZ_24mdn > 3 |
                          elvis$C1EQX_ANK_24mdn > 3 |
                          elvis$C1EQX_MSC_24mdn > 3 |
                          elvis$C1EQX_SCF_24mdn > 3, 1, 0)

covas$longcovid <- ifelse(covas$C1EQX_FAT_24mdn > 3 |
                          covas$C1EQX_COG_24mdn > 3 |
                          covas$C1EQX_SOC_24mdn > 3 |
                          covas$C1EQX_SLP_24mdn > 3 |
                          covas$C1EQX_APP_24mdn > 3 |
                          covas$C1EQX_SML_24mdn > 3 |
                          covas$C1EQX_CGH_24mdn > 3 |
                          covas$C1EQX_RSP_24mdn > 3 |
                          covas$C1EQX_PNR_24mdn > 3 |
                          covas$C1EQX_ANG_24mdn > 3 |
                          covas$C1EQX_PLP_24mdn > 3 |
                          covas$C1EQX_DIZ_24mdn > 3 |
                          covas$C1EQX_ANK_24mdn > 3 |
                          covas$C1EQX_MSC_24mdn > 3 |
                          covas$C1EQX_SCF_24mdn > 3, 1, 0)

maast$longcovid <- ifelse(maast$C1EQX_FAT24 > 3 |
                          maast$C1EQX_COG24 > 3 |
                      #   maast$C1EQX_SOC24 > 3 | # Niet in data?
                          maast$C1EQX_SLP24 > 3 |
                          maast$C1EQX_APP24 > 3 |
                          maast$C1EQX_SML24 > 3 |
                          maast$C1EQX_CGH24 > 3 |
                          maast$C1EQX_RSP24 > 3 |
                          maast$C1EQX_PNR24 > 3 |
                          maast$C1EQX_ANG24 > 3 |
                          maast$C1EQX_PLP24 > 3 |
                          maast$C1EQX_DIZ24 > 3 |
                          maast$C1EQX_ANK24 > 3 |
                          maast$C1EQX_MSC24 > 3 |
                          maast$C1EQX_SCF24 > 3, 1, 0)

lc <- c(elvis$longcovid, covas$longcovid, maast$longcovid)
longc <- data.frame(lc, wa, ic, ut)
longc$ho <- ifelse(longc$ic == 0 & longc$wa == 0, 1, 0)

table(longc$lc, longc$ho)[, 2]
round(prop.table(table(longc$lc, longc$ho), 2)*100, 1)[, 2]

table(longc$lc, longc$ic)[, 2]
round(prop.table(table(longc$lc, longc$ic), 2)*100, 1)[, 2]

table(longc$lc, longc$wa)[, 2]
round(prop.table(table(longc$lc, longc$wa), 2)*100, 1)[, 2]

## Association long COVID and HRQoL utility score ####
t.test(longc$ut ~ longc$lc)


## End of file.