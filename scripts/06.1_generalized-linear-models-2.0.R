# Created: 2025-03-10
# Updated: 2025-03-10

# Purpose: Run generalized linear models with Reproductive_culms and Total_Live_Culms
#   as response variable, and linear models with BGDensity and BGCover as response variable.

# Continuous explanatory variables are centered and scaled. 

# Updates from 06.R script:
#   Buffelgrass density and cover are log-transformed to improve normality.
#   MAT & MAP not used as explantory variable; MAT was often too closely correlated with precip
#     variables. Also didn't have much variation in MAT between sites.
#   Tested version of models with Prev_year_precip.

# Zero-inflated Poisson models needed for models of both reproductive and total culms.
# Mixed-effects linear models needed for buffelgrass density & cover (log transformed).


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
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1])

# Separate out plot-level data
dat.plot <- dat %>%
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm) %>%
  distinct(.keep_all = TRUE)

dat.plot_scaled <- dat.plot %>% 
  mutate(Perc_dev_scaled = scale(Perc_dev,center = TRUE, scale = TRUE)[, 1],
         Elevation_ft_scaled = scale(Elevation_ft, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1])



# Reproductive culms ------------------------------------------------------

## Poisson ----------------------------------------------------------------

# 1: With Perc_Dev
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
check_zeroinflation(pos.repro1) # model is underfitting zeros (ratio = 0.49)
check_collinearity(pos.repro1) 

# 2: With Perc_dev, add interactions
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
check_overdispersion(pos.repro2) # no overdispersion detected
check_zeroinflation(pos.repro2) # model is underfitting zeros (ratio = 0.53)
check_collinearity(pos.repro2)


# 3: With Prev_year_precip
pos.repro3 <- glmmTMB(Reproductive_culms ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                        Aspect + PlotSlope_scaled + (1 | Site / Transect),
                      data = dat,
                      family = genpois)
summary(pos.repro3)
r2(pos.repro3)
res.pos.repro3 <- simulateResiduals(pos.repro3)
plotQQunif(res.pos.repro3)
plotResiduals(res.pos.repro3)
check_overdispersion(pos.repro3) # no overdispersion detected
check_zeroinflation(pos.repro3) # model is underfitting zeros (ratio = 0.50)
check_collinearity(pos.repro3) 

# 4: With Perc_dev, add interactions
pos.repro4 <- glmmTMB(Reproductive_culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                        Aspect + PlotSlope_scaled + Prev_year_precip_scaled * Aspect +
                        Prev_year_precip_scaled * PlotSlope_scaled + (1 | Site / Transect),
                      data = dat,
                      family = genpois) 
summary(pos.repro4)
r2(pos.repro4)
res.pos.repro4 <- simulateResiduals(pos.repro4)
plotQQunif(res.pos.repro4)
plotResiduals(res.pos.repro4)
check_overdispersion(pos.repro4) # no overdispersion detected
check_zeroinflation(pos.repro4) # model is underfitting zeros (ratio = 0.54)
check_collinearity(pos.repro4)


## Zero-inflated Poisson --------------------------------------------------

# 1: With Perc_dev
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
check_overdispersion(zip.repro1) # overdispersion detected
check_zeroinflation(zip.repro1) # model is underfitting zeros (ratio = 0.77)
check_collinearity(zip.repro1)

# 2: With Perc_dev, add interactions
zip.repro2 <- glmmTMB(Reproductive_culms ~ Perc_dev_scaled + Elevation_ft_scaled + 
                        Aspect + PlotSlope_scaled + Perc_dev_scaled * Aspect +
                        Perc_dev_scaled * PlotSlope_scaled + (1 | Site / Transect),
                      data = dat,
                      family = poisson,
                      ziformula = ~.) 
summary(zip.repro2)
r2(zip.repro2) # R^2 slightly improved
res.zip.repro2 <- simulateResiduals(zip.repro2)
plotQQunif(res.zip.repro2)
plotResiduals(res.zip.repro2)
check_overdispersion(zip.repro2) # no overdispersion detected
check_zeroinflation(zip.repro2) # model is underfitting zeros (ratio = 0.86)
check_collinearity(zip.repro2) 

# 3: With Prev_year_precip 
zip.repro3 <- glmmTMB(Reproductive_culms ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                        Aspect + PlotSlope_scaled + (1 | Site / Transect),
                      data = dat,
                      family = poisson,
                      ziformula = ~.)
summary(zip.repro3)
r2(zip.repro3) # R^2 very low
res.zip.repro3 <- simulateResiduals(zip.repro3)
plotQQunif(res.zip.repro3)
plotResiduals(res.zip.repro3)
check_overdispersion(zip.repro3) # no overdispersion detected
check_zeroinflation(zip.repro3) # model is underfitting zeros (ratio = 0.76)
check_collinearity(zip.repro3)

# 2: With Prev_year_precip, add interactions
zip.repro4 <- glmmTMB(Reproductive_culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                        Aspect + PlotSlope_scaled + Prev_year_precip_scaled * Aspect +
                        Prev_year_precip_scaled * PlotSlope_scaled + (1 | Site / Transect),
                      data = dat,
                      family = poisson,
                      ziformula = ~.) 
summary(zip.repro4)
r2(zip.repro4) # R^2 slightly improved
res.zip.repro4 <- simulateResiduals(zip.repro4)
plotQQunif(res.zip.repro4)
plotResiduals(res.zip.repro4)
check_overdispersion(zip.repro4) # no overdispersion detected
check_zeroinflation(zip.repro4) # model is underfitting zeros (ratio = 0.81)
check_collinearity(zip.repro4) 



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
check_overdispersion(pos.total) # underdispersion detected
check_zeroinflation(pos.total) # model is underfitting zeros (ratio = 0.18)
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
check_zeroinflation(pos.total1) # model is underfitting zeros (ratio = 0.17)
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
check_zeroinflation(pos.total2) # model is underfitting zeros (ratio = 0.18)
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
check_zeroinflation(zip.total) # model is okay
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
check_zeroinflation(zip.total1) # model is underfitting zeros (ratio = 0.64)
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
check_zeroinflation(zip.total2) # model is underfitting zeros (ratio = 0.67)
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
