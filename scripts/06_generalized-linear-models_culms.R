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

# Data wrangling ----------------------------------------------------------

# Examine character cols
str(dat)


# Reproductive culms ------------------------------------------------------

## Poisson ----------------------------------------------------------------

# All variables
pos.repro <- glmmTMB(Reproductive_culms ~ Perc_dev_abs + Elevation_ft + MAT +
                       Aspect + PlotSlope + Year + Prev_year_precip + (1 | Site / Plot),
                     data = dat,
                     family = genpois)
summary(pos.repro)
r2(pos.repro)
res.pos.repro <- simulateResiduals(pos.repro)
plotQQunif(res.pos.repro)
plotResiduals(res.pos.repro)
check_overdispersion(pos.repro) # no overdispersion detected
check_zeroinflation(pos.repro) # model is underfitting zeros
check_collinearity(pos.repro)


## Zero-inflated Poisson --------------------------------------------------

zip.repro <- glmmTMB(Reproductive_culms ~ Perc_dev_abs + Elevation_ft + MAT +
                       Aspect + PlotSlope + Year + Prev_year_precip + (1 | Site / Plot),
                     data = dat,
                     family = poisson,
                     ziformula = ~.) # did not converge


## Zero-inflated negative binomial ----------------------------------------

# All variables: does not converge
zinb.repro <- glmmTMB(Reproductive_culms ~ Perc_dev_abs + Elevation_ft + MAT +
                       Aspect + PlotSlope + Year + Prev_year_precip + (1 | Site / Plot),
                     data = dat,
                     family = nbinom2,
                     ziformula = ~.) # did not converge



# Total culms -------------------------------------------------------------

## Poisson ----------------------------------------------------------------

pos.total <- glmmTMB(Total_Live_Culms ~ Perc_dev_abs + Elevation_ft + MAT +
                       Aspect + PlotSlope + Year + Prev_year_precip + (1 | Site / Plot),
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

# All variables: does not converge
zip.total <- glmmTMB(Total_Live_Culms ~ Perc_dev_abs + Elevation_ft + MAT +
                       Aspect + PlotSlope + Year + Prev_year_precip + (1 | Site / Plot),
                     data = dat,
                     family = poisson,
                     ziformula = ~.) # did not converge


# Zero-inflated negative binomial -----------------------------------------

# All variables
zinb.total <- glmmTMB(Total_Live_Culms ~ Perc_dev_abs + Elevation_ft + MAT +
                        Aspect + PlotSlope + Year + Prev_year_precip + (1 | Site / Plot),
                      data = dat,
                      family = nbinom2,
                      ziformula = ~.)
summary(zinb.total)
r2(zinb.total)
res.zinb.total <- simulateResiduals(zinb.total)
plotQQunif(res.zinb.total)
plotResiduals(res.zinb.total)
check_overdispersion(zinb.total) # no overdispersion detected
check_zeroinflation(zinb.total) # no zero-inflation
check_collinearity(zinb.total) # drop Year or Prev_year_precip

# All variables
zinb.total1 <- glmmTMB(Total_Live_Culms ~ Perc_dev_abs + Elevation_ft + MAT +
                        Aspect + PlotSlope + Prev_year_precip + (1 | Site / Plot),
                      data = dat,
                      family = nbinom2,
                      ziformula = ~.)
summary(zinb.total1)
r2(zinb.total1)
res.zinb.total1 <- simulateResiduals(zinb.total1)
plotQQunif(res.zinb.total1)
plotResiduals(res.zinb.total1)
check_overdispersion(zinb.total1) # no overdispersion detected
check_zeroinflation(zinb.total1) # model is underfitting zeros
check_collinearity(zinb.total1)

# 2: Site as fixed effect and Plot as random effect, Year dropped: does not converge
zinb.total2 <- glmmTMB(Total_Live_Culms ~ Perc_dev_abs + Elevation_ft + MAT +
                        Aspect + PlotSlope + Year + Prev_year_precip + Site +
                         (1 | Plot),
                      data = dat,
                      family = nbinom2,
                      ziformula = ~.) # did not converge


save.image("RData/06_generalized-linear-models_culms.RData")
