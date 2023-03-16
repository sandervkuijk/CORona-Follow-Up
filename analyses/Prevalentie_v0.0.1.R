
### Code header ####
# Sander van Kuijk
# 09-02-2023

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
# other attached packages:
#   [1] TrialSize_1.4
# 
# loaded via a namespace (and not attached):
#   [1] compiler_4.0.2 tools_4.0.2

### Packages ####
library(grid)
library(lattice)
library(latticeExtra)
library(HH)

### Read-in data ####
setwd("L:/SCEN/PZ-KEMTA/PROJECTEN/CORFU/Data/ORGINELE DATA van cohorten/Zuyderland/data 26012023 Sander")

d <- read.csv2("Acute en vragenlijst data extractie van 26.01.2023.csv")
d <- d[, colSums(is.na(d)) < nrow(d)] # Remove empty columns
d[d == -999] <- NA

### Brief check ####
names(d)[1:100]
str(d)

### Baseline: later! ####
# aggregate(, by = list())
# aggregate(, by = list())
# aggregate(, by = list(d$noemmaarwat))

### long COVID complaints ####

table(d$C1EQX_FAT) # Fatigue. Name.1 & .2 nearly completely missing (goes for all)
table(d$C1EQX_COG) # Cognition
table(d$C1EQX_SOC) # Social relations
table(d$C1EQX_SLP) # Sleep
table(d$C1EQX_APP) # Appetite
table(d$C1EQX_SML) # Smell/ taste
table(d$C1EQX_HEA) # Headache
table(d$C1EQX_CGH) # Coughing
table(d$C1EQX_RSP) # Respiratory
table(d$C1EQX_PNR) # Respiratory pain
table(d$C1EQX_ANG) # Angina pectoris(?)
table(d$C1EQX_PLP) # Palpitations
table(d$C1EQX_DIZ) # Dizzyness
table(d$C1EQX_ANK) # Swollen ankles
table(d$C1EQX_MSC) # Muscel weakness
table(d$C1EQX_SCF) # Self confidence
# table(d$C1EQX_PAR)
# table(d$C1EQX_CON)

### Likert plot(s) ####

dc.fig  <- d[, names(d)[grepl("C1EQX_", names(d))][1:16]]
dc.fig  <- dc.fig[rowSums(is.na(dc.fig)) != ncol(dc.fig), ]
dc.figt <- apply(dc.fig, 2, function(x) table(x))

dc.figt[[5]]  <- matrix(c(209, 49, 10, 1, 0), nrow = 1)
dc.figt[[7]]  <- matrix(c(182, 49, 30, 5, 0), nrow = 1)
dc.figt[[10]] <- matrix(c(235, 22, 7, 1, 0), nrow = 1)
dc.figt[[11]] <- matrix(c(183, 64, 16, 1, 0), nrow = 1)
dc.figt[[12]] <- matrix(c(164, 78, 21, 5, 0), nrow = 1)

dc.figt2 <- data.frame(t(matrix(unlist(dc.figt), nrow = 5)))
colnames(dc.figt2) <- c("none", "some", "moderate", "severe", "very severe")
row.names(dc.figt2) <- c("Social relations", "Cognition", "Fatigue", "Sleep",
                         "Appetite", "Smell/ taste", "Headache", "Coughing",
                         "Respiration", "Respiratory pain", "Angina pectoris", "Palpitations",
                         "Dizzyness", "Swollen ankles", "Muscel weakness", "Self confidence")
# dc.fig2 <- dc.fig2[, ncol(dc.fig2):1]

?likertColor
likertColor(6, 4, colorFunction = "diverge_hcl")


png("lC_zuyderland.png", width = 600, height = 500, pointsize = 20)
plot.likert(dc.figt2, main = "long COVID symptoms", positive.order = TRUE,
            sub="", as.percent = TRUE, rightAxis = FALSE, xlim = c(-100, 100),
            ReferenceZero = 2.5, col = c("#768BE6", "#A2ACE5", "#E8ACB7", "#E6899D", "#E16A86"))
dev.off()
