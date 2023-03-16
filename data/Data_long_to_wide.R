
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
# 
# loaded via a namespace (and not attached):
# [1] compiler_4.0.2 tools_4.0.2 

library(foreign)
library(reshape2)

setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/WERK DATA cohorten/MERGE FILES")
d <- read.spss("CORFU DATABASE_3_6_12_18_24mdn_COVAS_ACUUT_FU_23_2_23 - MI.sav",
               to.data.frame = TRUE)
