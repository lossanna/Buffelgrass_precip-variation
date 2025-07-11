# Created: 2024-09-23
# Updated: 2025-02-28

# Purpose: Run generalized linear models with Reproductive_culms and Total_Live_Culms
#   as response variable, and linear models with BGDensity and BGCover 
#   as response variable.

# Continuous variables are centered and scaled.

# Zero-inflated Poisson models needed for models of both reproductive and total culms.
# Mixed-effects linear models needed for buffelgrass density & cover (response variables are
#   approximately normally distributed).

# Previous_year_precip and Perc_dev are too highly correlated, so Previous_year_precip must be dropped.
# MAT also dropped because sites were close enough that they probably didn't experience 
#   different temperatures (range was only 1.6 degrees C); would have needed more local
#   measurements of higher resolution to see temperature effects.


library(tidyverse)
library(glmmTMB)
library(performance)
library(DHARMa)
library(nlme)
library(performance)
library(lme4)

# Load data ---------------------------------------------------------------

dat <- read_csv("data/cleaned/04_demography-data_clean.csv")


# Data wrangling ----------------------------------------------------------

# Center and scale numeric variables
dat <- dat %>% 
  mutate(Perc_dev_scaled = scale(Perc_dev, center = TRUE, scale = TRUE)[, ],
         Elevation_ft_scaled = scale(Elevation_ft, center = TRUE, scale = TRUE)[, 1],
         MAT_scaled = scale(MAT, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1])

# Separate out plot-level data
dat.plot <- dat %>%
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm) %>%
  distinct(.keep_all = TRUE)

dat.plot_scaled <- dat.plot %>% 
  mutate(Perc_dev_scaled = scale(Perc_dev,center = TRUE, scale = TRUE)[, 1],
         Elevation_ft_scaled = scale(Elevation_ft, center = TRUE, scale = TRUE)[, 1],
         MAT_scaled = scale(MAT, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1])



# Reproductive culms ------------------------------------------------------

## Poisson ----------------------------------------------------------------

# All variables
pos.repro <- glmmTMB(Reproductive_culms ~ Perc_dev_scaled + Elevation_ft_scaled + MAT_scaled +
                       Aspect + PlotSlope_scaled + Prev_year_precip_scaled + (1 | Site / Transect),
                     data = dat,
                     family = genpois)
summary(pos.repro)
r2(pos.repro)
res.pos.repro <- simulateResiduals(pos.repro)
plotQQunif(res.pos.repro)
plotResiduals(res.pos.repro)
check_overdispersion(pos.repro) # underdispersion detected
check_zeroinflation(pos.repro) # model is underfitting zeros (ratio = 0.70)
check_collinearity(pos.repro) # Perc_dev and Previous_year_precip correlated

# 1: Drop Previous_year_precip & MAT
pos.repro1 <- glmmTMB(Reproductive_culms ~ Perc_dev_scaled + Elevation_ft_scaled +
                        Aspect + PlotSlope_scaled + (1 | Site / Transect),
                      data = dat,
                      family = genpois)
summary(pos.repro1)
r2(pos.repro1)
res.pos.repro1 <- simulateResiduals(pos.repro1)
plotQQunif(res.pos.repro1)
plotResiduals(res.pos.repro1)
check_overdispersion(pos.repro1) # no overdispersion detected
check_zeroinflation(pos.repro1) # model is underfitting zeros (ratio = 0.52)
check_collinearity(pos.repro1) 

# 2: Drop Previous_year_precip & MAT, add interactions
pos.repro2 <- glmmTMB(Reproductive_culms ~ Perc_dev_scaled + Elevation_ft_scaled + 
                        Aspect + PlotSlope_scaled + Perc_dev_scaled * Aspect +
                        Perc_dev_scaled * PlotSlope_scaled + (1 | Site / Transect),
                      data = dat,
                      family = genpois) 
summary(pos.repro2)
r2(pos.repro2)
res.pos.repro2 <- simulateResiduals(pos.repro2)
plotQQunif(res.pos.repro2)
plotResiduals(res.pos.repro2)
check_overdispersion(pos.repro2) # underdispersion detected
check_zeroinflation(pos.repro2) # model is underfitting zeros (ratio = 0.56)
check_collinearity(pos.repro2) 


## Zero-inflated Poisson --------------------------------------------------

# All variables
zip.repro <- glmmTMB(Reproductive_culms ~ Perc_dev_scaled + Elevation_ft_scaled + MAT_scaled +
                       Aspect + PlotSlope_scaled + Prev_year_precip_scaled + (1 | Site / Transect),
                     data = dat,
                     family = poisson,
                     ziformula = ~.)
summary(zip.repro)
r2(zip.repro) # can't compute
res.zip.repro <- simulateResiduals(zip.repro)
plotQQunif(res.zip.repro)
plotResiduals(res.zip.repro)
check_overdispersion(zip.repro) # overdispersion detected (ratio = 2.59)
check_zeroinflation(zip.repro) # model is okay
check_collinearity(zip.repro) # Perc_dev and Prev_year_precip correlated

# 1: Drop Prev_year_precip & MAT
zip.repro1 <- glmmTMB(Reproductive_culms ~ Perc_dev_scaled + Elevation_ft_scaled +
                        Aspect + PlotSlope_scaled +  (1 | Site / Transect),
                      data = dat,
                      family = poisson,
                      ziformula = ~.)
summary(zip.repro1)
r2(zip.repro1) # R^2 is very low
res.zip.repro1 <- simulateResiduals(zip.repro1)
plotQQunif(res.zip.repro1)
plotResiduals(res.zip.repro1)
check_overdispersion(zip.repro1) # overdispersion detected (ratio = 2.37)
check_zeroinflation(zip.repro1) # model is underfitting zeros (ratio = 0.77)
check_collinearity(zip.repro1)

# ***2: Drop Prev_year_precip & MAT, add interactions***
zip.repro2 <- glmmTMB(Reproductive_culms ~ Perc_dev_scaled + Elevation_ft_scaled + 
                        Aspect + PlotSlope_scaled + Perc_dev_scaled * Aspect +
                        Perc_dev_scaled * PlotSlope_scaled + (1 | Site / Transect),
                      data = dat,
                      family = poisson,
                      ziformula = ~.) 
summary(zip.repro2)
r2(zip.repro2) 
res.zip.repro2 <- simulateResiduals(zip.repro2)
plotQQunif(res.zip.repro2)
plotResiduals(res.zip.repro2)
check_overdispersion(zip.repro2) # no overdispersion detected
check_zeroinflation(zip.repro2) # model is underfitting zeros (ratio = 0.86)
check_collinearity(zip.repro2) # Perc_dev and Perc_dev * Aspect highly correlated

# 3: Drop Perc_dev * Aspect (for collinearity) - fit is really not good at all
zip.repro3 <- glmmTMB(Reproductive_culms ~ Perc_dev_scaled + Elevation_ft_scaled + 
                        Aspect + PlotSlope_scaled + 
                        Perc_dev_scaled * PlotSlope_scaled + (1 | Site / Transect),
                      data = dat,
                      family = poisson,
                      ziformula = ~.) 
summary(zip.repro3)
r2(zip.repro3) 
res.zip.repro3 <- simulateResiduals(zip.repro3)
plotQQunif(res.zip.repro3)
plotResiduals(res.zip.repro3)
check_overdispersion(zip.repro3) # overdispersion detected
check_zeroinflation(zip.repro3) # model is underfitting zeros (ratio = 0.424)
check_collinearity(zip.repro3)



# Total culms -------------------------------------------------------------

## Poisson ----------------------------------------------------------------

# All variables
pos.total <- glmmTMB(Total_Live_Culms ~ Perc_dev_scaled + Elevation_ft_scaled + MAT_scaled +
                     Aspect + PlotSlope_scaled + Prev_year_precip_scaled + (1 | Site / Transect),
                     data = dat,
                     family = genpois)
summary(pos.total)
r2(pos.total)
res.pos.total <- simulateResiduals(pos.total)
plotQQunif(res.pos.total)
plotResiduals(res.pos.total)
check_overdispersion(pos.total) # underdispersion detected (ratio = 0.58)
check_zeroinflation(pos.total) # model is underfitting zeros (ratio = 0.20)
check_collinearity(pos.total) # Perc_dev and Prev_year_precip highly correlated

# 1: Drop Prev_year_precip & MAT
pos.total1 <- glmmTMB(Total_Live_Culms ~ Perc_dev_scaled + Elevation_ft_scaled +
                        Aspect + PlotSlope_scaled + (1 | Site / Transect),
                      data = dat,
                      family = genpois)
summary(pos.total1)
r2(pos.total1)
res.pos.total1 <- simulateResiduals(pos.total1)
plotQQunif(res.pos.total1)
plotResiduals(res.pos.total1)
check_overdispersion(pos.total1) # no overdispersion detected
check_zeroinflation(pos.total1) # model is underfitting zeros (ratio = 0.19)
check_collinearity(pos.total1) 

# 2: Drop Prev_year_precip & MAT, add interactions
pos.total2 <- glmmTMB(Total_Live_Culms ~ Perc_dev_scaled + Elevation_ft_scaled +
                        Aspect + PlotSlope_scaled + Perc_dev_scaled * Aspect +
                        Perc_dev_scaled * PlotSlope_scaled + (1 | Site / Transect),
                      data = dat,
                      family = genpois)
summary(pos.total2)
r2(pos.total2)
res.pos.total2 <- simulateResiduals(pos.total2)
plotQQunif(res.pos.total2)
plotResiduals(res.pos.total2)
check_overdispersion(pos.total2) # no overdispersion detected
check_zeroinflation(pos.total2) # model is underfitting zeros (ratio = 0.20)
check_collinearity(pos.total2) 


## Zero-inflated Poisson --------------------------------------------------

# All variables
zip.total <- glmmTMB(Total_Live_Culms ~ Perc_dev_scaled + Elevation_ft_scaled + MAT_scaled +
                     Aspect + PlotSlope_scaled + Prev_year_precip_scaled + (1 | Site / Transect),
                     data = dat,
                     family = poisson,
                     ziformula = ~.)
summary(zip.total)
r2(zip.total) # can't compute
res.zip.total <- simulateResiduals(zip.total)
plotQQunif(res.zip.total)
plotResiduals(res.zip.total)
check_overdispersion(zip.total) # no overdispersion detected
check_zeroinflation(zip.total) # model is overfitting zeros (ratio = 1.11)
check_collinearity(zip.total) # Perc_dev and Prev_year_precip correlated

# 1: Drop Prev_year_precip & MAT
zip.total1 <- glmmTMB(Total_Live_Culms ~ Perc_dev_scaled + Elevation_ft_scaled + 
                      Aspect + PlotSlope_scaled + (1 | Site / Transect),
                      data = dat,
                      family = poisson,
                      ziformula = ~.)
summary(zip.total1)
r2(zip.total1) 
res.zip.total1 <- simulateResiduals(zip.total1)
plotQQunif(res.zip.total1)
plotResiduals(res.zip.total1)
check_overdispersion(zip.total1) # no overdispersion detected
check_zeroinflation(zip.total1) # model is underfitting zeros (ratio = 0.65)
check_collinearity(zip.total1)


# 2: ***Drop Prev_year_precip & MAT, add interactions***
zip.total2 <- glmmTMB(Total_Live_Culms ~ Perc_dev_scaled + Elevation_ft_scaled +
                        Aspect + PlotSlope_scaled + Perc_dev_scaled * Aspect +
                        Perc_dev_scaled * PlotSlope_scaled + (1 | Site / Transect),
                      data = dat,
                      family = poisson,
                      ziformula = ~.)
summary(zip.total2)
r2(zip.total2)
res.zip.total2 <- simulateResiduals(zip.total2)
plotQQunif(res.zip.total2)
plotResiduals(res.zip.total2)
check_overdispersion(zip.total2) # no overdispersion detected
check_zeroinflation(zip.total2) # model is underfitting zeros (ratio = 0.65)
check_collinearity(zip.total2) # Perc_dev * Aspect has high correlation



# Buffelgrass density -----------------------------------------------------

# All variables
lm.bgden <- lme(BGDensity ~ Perc_dev_scaled + Elevation_ft_scaled + MAT_scaled +
                  Aspect + PlotSlope_scaled + Prev_year_precip_scaled, 
                random = ~ 1 | Site / Transect,
                data = dat.plot_scaled) 
summary(lm.bgden)
r2(lm.bgden)
check_model(lm.bgden)
check_collinearity(lm.bgden) # Prev_year_precip and Perc_dev correlated

# 1: Drop Prev_year_precip & MAT
lm.bgden1 <- lme(BGDensity ~ Perc_dev_scaled + Elevation_ft_scaled + 
                   Aspect + PlotSlope_scaled,
                 random = ~ 1 | Site / Transect,
                 data = dat.plot_scaled) 
summary(lm.bgden1)
r2(lm.bgden1) 
check_model(lm.bgden1)
check_collinearity(lm.bgden1)

# ***2: Drop Prev_year_precip & MAT, add interactions***
lm.bgden2 <- lme(BGDensity ~ Perc_dev_scaled + Elevation_ft_scaled + 
                    Aspect + PlotSlope_scaled + Perc_dev_scaled * Aspect +
                    Perc_dev_scaled * PlotSlope_scaled,
                 random = ~ 1 | Site / Transect,
                  data = dat.plot_scaled) 
summary(lm.bgden2)
r2(lm.bgden2) 
check_model(lm.bgden2) 
check_collinearity(lm.bgden2) # Perc_dev * Aspect has high correlation

# 2: lme4 version
lm.bgden2.lme4 <- lmer(BGDensity ~ Perc_dev_scaled + Elevation_ft_scaled + 
                    Aspect + PlotSlope_scaled + Perc_dev_scaled * Aspect +
                    Perc_dev_scaled * PlotSlope_scaled + (1 | Site / Transect),
                  data = dat.plot_scaled) 
summary(lm.bgden2.lme4)
res.lm.bgden2.lme4 <- simulateResiduals(lm.bgden2.lme4)
plotQQunif(res.lm.bgden2.lme4)
plotResiduals(res.lm.bgden2.lme4)
check_overdispersion(lm.bgden2.lme4) # no overdispersion detected
check_collinearity(lm.bgden2.lme4)



# Buffelgrass cover -------------------------------------------------------

# All variables
lm.bgcov <- lmer(BGCover ~ Perc_dev_scaled + Elevation_ft_scaled + MAT_scaled +
                 Aspect + PlotSlope_scaled + Prev_year_precip_scaled + (1 | Site / Transect),
                 data = dat.plot_scaled)
summary(lm.bgcov)
r2(lm.bgcov)
res.lm.bgcov <- simulateResiduals(lm.bgcov)
plotQQunif(res.lm.bgcov)
plotResiduals(res.lm.bgcov)
check_overdispersion(lm.bgcov) # no overdispersion detected
check_collinearity(lm.bgcov) # Prev_year_precip and Perc_dev correlated

# 1: Drop Prev_year_precip & MAT
lm.bgcov1 <- lmer(BGCover ~ Perc_dev_scaled + Elevation_ft_scaled + 
                  Aspect + PlotSlope_scaled + (1 | Site / Transect),
                  data = dat.plot_scaled)
summary(lm.bgcov1)
r2(lm.bgcov1)
res.lm.bgcov1 <- simulateResiduals(lm.bgcov1)
plotQQunif(res.lm.bgcov1)
plotResiduals(res.lm.bgcov1)
check_overdispersion(lm.bgcov1) # no overdispersion detected
check_collinearity(lm.bgcov1) # Perc_dev * Aspect has high correlation


# ***2: Drop Prev_year_precip & MAT, add interactions***
lm.bgcov2 <- lme(BGCover ~ Perc_dev_scaled + Elevation_ft_scaled + 
                    Aspect + PlotSlope_scaled + Perc_dev_scaled * Aspect +
                    Perc_dev_scaled * PlotSlope_scaled,
                 random = ~ 1 | Site / Transect,
                  data = dat.plot_scaled) 
summary(lm.bgcov2)
r2(lm.bgcov2) 
check_model(lm.bgcov2) 
check_collinearity(lm.bgcov2) # Perc_dev * Aspect has high correlation

# 2: lme4 version
lm.bgcov2.lme4 <- lmer(BGCover ~ Perc_dev_scaled + Elevation_ft_scaled + 
                    Aspect + PlotSlope_scaled + Perc_dev_scaled * Aspect +
                    Perc_dev_scaled * PlotSlope_scaled + (1 | Site / Transect),
                  data = dat.plot_scaled) 
summary(lm.bgcov2.lme4)
r2(lm.bgcov2.lme4) 
res.lm.bgcov2.lme4 <- simulateResiduals(lm.bgcov2.lme4)
plotQQunif(res.lm.bgcov2.lme4)
plotResiduals(res.lm.bgcov2.lme4)
check_overdispersion(lm.bgcov2.lme4) # no overdispersion detected
check_collinearity(lm.bgcov2.lme4) # Perc_dev * Aspect has high correlation



save.image("RData/06_generalized-linear-models.RData")
