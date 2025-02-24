# Created: 2024-09-23
# Updated: 2025-02-21

# Purpose: Run generalized linear models with Reproductive_culms and Total_Live_Culms
#   as response variable.

# Continuous variables are centered and scaled.

# Zero-inflated Poisson models needed for models of both reproductive and total culms.
# Mixed-effects linear models needed for buffelgrass density & cover (response variables are
#   approximately normally distributed).

# Previous_year_precip and Perc_dev are too highly correlated, so Previous_year_precip must be dropped.
# MAT also dropped because sites were close enough that they probably didn't experience 
#   different temperatures.


library(tidyverse)
library(glmmTMB)
library(performance)
library(DHARMa)
library(lme4)

# Load data ---------------------------------------------------------------

dat <- read_csv("data/cleaned/04_demography-data_clean.csv")


# Data wrangling ----------------------------------------------------------

# Center and scale numeric variables
dat <- dat |> 
  mutate(Perc_dev_scaled = scale(Perc_dev, center = TRUE, scale = TRUE)[, ],
         Elevation_ft_scaled = scale(Elevation_ft, center = TRUE, scale = TRUE)[, 1],
         MAT_scaled = scale(MAT, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1])

# Separate out plot-level data
dat.plot <- dat |>
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm) |>
  distinct(.keep_all = TRUE)

dat.plot_scaled <- dat.plot |> 
  mutate(Perc_dev_scaled = scale(Perc_dev,center = TRUE, scale = TRUE)[, 1],
         Elevation_ft_scaled = scale(Elevation_ft, center = TRUE, scale = TRUE)[, 1],
         MAT_scaled = scale(MAT, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1])



# Reproductive culms ------------------------------------------------------

## Poisson ----------------------------------------------------------------

# All variables, scaled
pos.repro <- glmmTMB(Reproductive_culms ~ Perc_dev_scaled + Elevation_ft_scaled + MAT_scaled +
                     Aspect + PlotSlope_scaled + Prev_year_precip_scaled + (1 | Site / Plot),
                     data = dat,
                     family = genpois)
summary(pos.repro)
r2(pos.repro)
res.pos.repro <- simulateResiduals(pos.repro)
plotQQunif(res.pos.repro)
plotResiduals(res.pos.repro)
check_overdispersion(pos.repro) # underdispersion detected
check_zeroinflation(pos.repro) # model is underfitting zeros (ratio = 0.61)
check_collinearity(pos.repro) # Perc_dev and Previous_year_precip correlated

# 1: Drop Previous_year_precip & MAT, scaled
pos.repro1 <- glmmTMB(Reproductive_culms ~ Perc_dev_scaled + Elevation_ft_scaled +
                      Aspect + PlotSlope_scaled + (1 | Site / Plot),
                      data = dat,
                      family = genpois)
summary(pos.repro1)
r2(pos.repro1)
res.pos.repro1 <- simulateResiduals(pos.repro1)
plotQQunif(res.pos.repro1)
plotResiduals(res.pos.repro1)
check_overdispersion(pos.repro1) # no overdispersion detected
check_zeroinflation(pos.repro1) # model is underfitting zeros (ratio = 0.47)
check_collinearity(pos.repro1) 

# 2: Drop Previous_year_precip & MAT, no scaling
pos.repro2 <- glmmTMB(Reproductive_culms ~ Perc_dev + Elevation_ft + 
                        Aspect + PlotSlope + (1 | Site / Plot),
                      data = dat,
                      family = genpois)
summary(pos.repro2)
r2(pos.repro2)
res.pos.repro2 <- simulateResiduals(pos.repro2)
plotQQunif(res.pos.repro2)
plotResiduals(res.pos.repro2)
check_overdispersion(pos.repro2) # no overdispersion detected
check_zeroinflation(pos.repro2) # model is underfitting zeros (ratio = 0.47)
check_collinearity(pos.repro2)


## Zero-inflated Poisson --------------------------------------------------

# All variables, scaled
zip.repro <- glmmTMB(Reproductive_culms ~ Perc_dev_scaled + Elevation_ft_scaled + MAT_scaled +
                     Aspect + PlotSlope_scaled + Prev_year_precip_scaled + (1 | Site / Plot),
                     data = dat,
                     family = poisson,
                     ziformula = ~.)
summary(zip.repro)
r2(zip.repro) # can't compute
res.zip.repro <- simulateResiduals(zip.repro)
plotQQunif(res.zip.repro)
plotResiduals(res.zip.repro)
check_overdispersion(zip.repro) # no overdispersion detected
check_zeroinflation(zip.repro) # model is okay
check_collinearity(zip.repro) # Perc_dev and Prev_year_precip correlated

# ***1: Drop Prev_year_precip & MAT, scaled***
zip.repro1 <- glmmTMB(Reproductive_culms ~ Perc_dev_scaled + Elevation_ft_scaled +
                      Aspect + PlotSlope_scaled +  (1 | Site / Plot),
                      data = dat,
                      family = poisson,
                      ziformula = ~.)
summary(zip.repro1)
r2(zip.repro1) # R^2 is very low
res.zip.repro1 <- simulateResiduals(zip.repro1)
plotQQunif(res.zip.repro1)
plotResiduals(res.zip.repro1)
check_overdispersion(zip.repro1) # no overdispersion detected
check_zeroinflation(zip.repro1) # model is underfitting zeros (ratio = 0.70)
check_collinearity(zip.repro1)

# 2: Drop Prev_year_precip & MAT, no scaling
zip.repro2 <- glmmTMB(Reproductive_culms ~ Perc_dev + Elevation_ft +
                      Aspect + PlotSlope + (1 | Site / Plot),
                      data = dat,
                      family = poisson,
                      ziformula = ~.) 
summary(zip.repro2)
r2(zip.repro2) # R^2 is still very low
res.zip.repro2 <- simulateResiduals(zip.repro2)
plotQQunif(res.zip.repro2)
plotResiduals(res.zip.repro2)
check_overdispersion(zip.repro2) # no overdispersion detected
check_zeroinflation(zip.repro2) # model is underfitting zeros (ratio = 0.66)
check_collinearity(zip.repro2)




# Total culms -------------------------------------------------------------

## Poisson ----------------------------------------------------------------

# All variables, scaled
pos.total <- glmmTMB(Total_Live_Culms ~ Perc_dev_scaled + Elevation_ft_scaled + MAT_scaled +
                     Aspect + PlotSlope_scaled + Prev_year_precip_scaled + (1 | Site / Plot),
                     data = dat,
                     family = genpois)
summary(pos.total)
r2(pos.total)
res.pos.total <- simulateResiduals(pos.total)
plotQQunif(res.pos.total)
plotResiduals(res.pos.total)
check_overdispersion(pos.total) # no overdispersion detected
check_zeroinflation(pos.total) # model is underfitting zeros (ratio = 0.17)
check_collinearity(pos.total)

# 1: Drop Prev_year_precip & MAT, scaled
pos.total1 <- glmmTMB(Total_Live_Culms ~ Perc_dev_scaled + Elevation_ft_scaled +
                      Aspect + PlotSlope_scaled + (1 | Site / Plot),
                      data = dat,
                      family = genpois)
summary(pos.total1)
r2(pos.total1)
res.pos.total1 <- simulateResiduals(pos.total1)
plotQQunif(res.pos.total1)
plotResiduals(res.pos.total1)
check_overdispersion(pos.total1) # no overdispersion detected
check_zeroinflation(pos.total1) # model is underfitting zeros (ratio = 0.17)
check_collinearity(pos.total1) 


## Zero-inflated Poisson --------------------------------------------------

# All variables, scaled
zip.total <- glmmTMB(Total_Live_Culms ~ Perc_dev_scaled + Elevation_ft_scaled + MAT_scaled +
                     Aspect + PlotSlope_scaled + Prev_year_precip_scaled + (1 | Site / Plot),
                     data = dat,
                     family = poisson,
                     ziformula = ~.)
summary(zip.total)
r2(zip.total) 
res.zip.total <- simulateResiduals(zip.total)
plotQQunif(res.zip.total)
plotResiduals(res.zip.total)
check_overdispersion(zip.total) # no overdispersion detected
check_zeroinflation(zip.total) # model is overfitting zeros (ratio = 1.20)
check_collinearity(zip.total) # Perc_dev and Prev_year_precip correlated

# ***1: Drop Prev_year_precip & MAT, scaled***
zip.total1 <- glmmTMB(Total_Live_Culms ~ Perc_dev_scaled + Elevation_ft_scaled + 
                      Aspect + PlotSlope_scaled + (1 | Site / Plot),
                      data = dat,
                      family = poisson,
                      ziformula = ~.)
summary(zip.total1)
r2(zip.total1) 
res.zip.total1 <- simulateResiduals(zip.total1)
plotQQunif(res.zip.total1)
plotResiduals(res.zip.total1)
check_overdispersion(zip.total1) # no overdispersion detected
check_zeroinflation(zip.total1) # model is underfitting zeros (ratio = 0.57)
check_collinearity(zip.total1)



# Buffelgrass density -----------------------------------------------------

# All variables, scaled
lm.bgden <- lmer(BGDensity ~ Perc_dev_scaled + Elevation_ft_scaled + MAT_scaled +
                 Aspect + PlotSlope_scaled + Prev_year_precip_scaled + (1 | Site / Plot),
                 data = dat.plot_scaled) 
summary(lm.bgden)
r2(lm.bgden) # cannot be calculated
res.lm.bgden <- simulateResiduals(lm.bgden)
plotQQunif(res.lm.bgden)
plotResiduals(res.lm.bgden)
check_overdispersion(lm.bgden) # no overdispersion detected
check_collinearity(lm.bgden) # Prev_year_precip and Perc_dev correlated

# ***1: Drop Prev_year_precip & MAT, scaled***
lm.bgden1 <- lmer(BGDensity ~ Perc_dev_scaled + Elevation_ft_scaled + 
                  Aspect + PlotSlope_scaled + (1 | Site / Plot),
                  data = dat.plot_scaled) 
summary(lm.bgden1)
r2(lm.bgden1) 
res.lm.bgden1 <- simulateResiduals(lm.bgden1)
plotQQunif(res.lm.bgden1)
plotResiduals(res.lm.bgden1)
check_overdispersion(lm.bgden1) # no overdispersion detected
check_collinearity(lm.bgden1)



# Buffelgrass cover -------------------------------------------------------

# All variables, scaled
lm.bgcov <- lmer(BGCover ~ Perc_dev_scaled + Elevation_ft_scaled + MAT_scaled +
                 Aspect + PlotSlope_scaled + Prev_year_precip_scaled + (1 | Site / Plot),
                 data = dat.plot_scaled)
summary(lm.bgcov)
r2(lm.bgcov)
res.lm.bgcov <- simulateResiduals(lm.bgcov)
plotQQunif(res.lm.bgcov)
plotResiduals(res.lm.bgcov)
check_overdispersion(lm.bgcov) # no overdispersion detected
check_collinearity(lm.bgcov) # Prev_year_precip and Perc_dev correlated

# ***1: Drop Prev_year_precip & MAT, scaled***
lm.bgcov1 <- lmer(BGCover ~ Perc_dev_scaled + Elevation_ft_scaled + 
                  Aspect + PlotSlope_scaled + (1 | Site / Plot),
                  data = dat.plot_scaled)
summary(lm.bgcov1)
r2(lm.bgcov1)
res.lm.bgcov1 <- simulateResiduals(lm.bgcov1)
plotQQunif(res.lm.bgcov1)
plotResiduals(res.lm.bgcov1)
check_overdispersion(lm.bgcov1) # no overdispersion detected
check_collinearity(lm.bgcov1)


save.image("RData/06_generalized-linear-models.RData")
