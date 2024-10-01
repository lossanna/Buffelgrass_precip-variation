# Created: 2024-09-23
# Updated: 2024-10-01

# Purpose: Run generalized linear models with Reproductive_culms and Total_Live_Culms 
#   as response variable.

library(tidyverse)
library(glmmTMB)
library(performance)
library(DHARMa)

# Load data ---------------------------------------------------------------

dat <- read_csv("data/cleaned/04_demography-data_clean.csv")


# Reproductive culms ------------------------------------------------------

## Poisson ----------------------------------------------------------------

# All variables
pos.repro <- glmmTMB(Reproductive_culms ~ Perc_dev_abs + Elevation_ft + MAT +
                       Aspect + PlotSlope + Prev_year_precip + (1 | Site / Plot),
                     data = dat,
                     family = genpois)
summary(pos.repro)
r2(pos.repro)
res.pos.repro <- simulateResiduals(pos.repro)
plotQQunif(res.pos.repro)
plotResiduals(res.pos.repro)
check_overdispersion(pos.repro) # no overdispersion detected
check_zeroinflation(pos.repro) # model is underfitting zeros (ratio = 0.5)
check_collinearity(pos.repro)

# 1: Site as fixed effect, drop MAT
pos.repro1 <- glmmTMB(Reproductive_culms ~ Perc_dev_abs + Elevation_ft + 
                       Aspect + PlotSlope + Prev_year_precip + Site + (1 | Plot),
                     data = dat,
                     family = genpois)
summary(pos.repro1)
r2(pos.repro1)
res.pos.repro1 <- simulateResiduals(pos.repro1)
plotQQunif(res.pos.repro1)
plotResiduals(res.pos.repro1)
check_overdispersion(pos.repro1) # underdispersion detected
check_zeroinflation(pos.repro1) # model is underfitting zeros
check_collinearity(pos.repro1) # Site has high correlation


## Zero-inflated Poisson --------------------------------------------------

# All variables
zip.repro <- glmmTMB(Reproductive_culms ~ Perc_dev_abs + Elevation_ft + MAT +
                       Aspect + PlotSlope + Prev_year_precip + (1 | Site / Plot),
                     data = dat,
                     family = poisson,
                     ziformula = ~.)
summary(zip.repro)
r2(zip.repro) # lol this R^2 is terrible
res.zip.repro <- simulateResiduals(zip.repro)
plotQQunif(res.zip.repro)
plotResiduals(res.zip.repro)
check_overdispersion(zip.repro) # no overdispersion detected
check_zeroinflation(zip.repro) # model is underfitting zeros
check_collinearity(zip.repro)

# 1: Drop MAT: did not really improve anything
zip.repro1 <- glmmTMB(Reproductive_culms ~ Perc_dev_abs + Elevation_ft + 
                       Aspect + PlotSlope + Prev_year_precip + (1 | Site / Plot),
                     data = dat,
                     family = poisson,
                     ziformula = ~.)
summary(zip.repro1)
r2(zip.repro1) # lol this R^2 is terrible
res.zip.repro1 <- simulateResiduals(zip.repro1)
plotQQunif(res.zip.repro1)
plotResiduals(res.zip.repro1)
check_overdispersion(zip.repro1) # no overdispersion detected
check_zeroinflation(zip.repro1) # model is underfitting zeros
check_collinearity(zip.repro1)


# Total culms -------------------------------------------------------------

## Poisson ----------------------------------------------------------------

# All variables
pos.total <- glmmTMB(Total_Live_Culms ~ Perc_dev_abs + Elevation_ft + MAT +
                       Aspect + PlotSlope + Prev_year_precip + (1 | Site / Plot),
                     data = dat,
                     family = genpois)
summary(pos.total)
r2(pos.total)
res.pos.total <- simulateResiduals(pos.total)
plotQQunif(res.pos.total)
plotResiduals(res.pos.total)
check_overdispersion(pos.total) # no overdispersion detected
check_zeroinflation(pos.total) # model is underfitting zeros
check_collinearity(pos.total)


## Zero-inflated Poisson --------------------------------------------------

# All variables
zip.total <- glmmTMB(Total_Live_Culms ~ Perc_dev_abs + Elevation_ft + MAT +
                       Aspect + PlotSlope + Prev_year_precip + (1 | Site / Plot),
                     data = dat,
                     family = poisson,
                     ziformula = ~.)
summary(zip.total)
r2(zip.total) # this R^2 is rough
res.zip.total <- simulateResiduals(zip.total)
plotQQunif(res.zip.total)
plotResiduals(res.zip.total)
check_overdispersion(zip.total) # no overdispersion detected
check_zeroinflation(zip.total) # model is underfitting zeros
check_collinearity(zip.total)




save.image("RData/06_generalized-linear-models_culms.RData")
