# Created: 2026-01-28
# Updated: 2026-01-28

# Purpose: Check versions of linear models that round 1 reviewers suggested.

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
  mutate(PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         ShrubCover_scaled = scale(ShrubCover, center = TRUE, scale = TRUE)[, 1],
         HerbCover_scaled = scale(HerbCover, center = TRUE, scale = TRUE)[, 1],
         BGDensity_scaled = scale(BGDensity, center = TRUE, scale = TRUE)[, 1]) %>% 
  filter(Aspect != "flat")

# Separate out plot-level data
dat.plot <- dat %>%
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm) %>%
  distinct(.keep_all = TRUE)

dat.plot_scaled <- dat.plot %>% 
  mutate(PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         ShrubCover_scaled = scale(ShrubCover, center = TRUE, scale = TRUE)[, 1],
         HerbCover_scaled = scale(HerbCover, center = TRUE, scale = TRUE)[, 1],
         BGDensity_scaled = scale(BGDensity, center = TRUE, scale = TRUE)[, 1])



# Total culms -------------------------------------------------------------

## Poisson ----------------------------------------------------------------

# Version 1: Nested Site / Transect
pos.total1 <- glmmTMB(Total_Live_Culms ~ Prev_year_precip_scaled +
                       Aspect + PlotSlope_scaled + BGDensity_scaled + 
                        ShrubCover_scaled + HerbCover_scaled +
                       (1 | Site / Transect),
                     data = dat,
                     family = genpois)
summary(pos.total1)
r2(pos.total1)
res.pos.total1 <- simulateResiduals(pos.total1)
plotQQunif(res.pos.total1)
plotResiduals(res.pos.total1)
check_overdispersion(pos.total1) # no overdispersion detected
check_zeroinflation(pos.total1) # model is underfitting zeros (ratio = 0.18)
check_collinearity(pos.total1)


# Version 2: Transect
pos.total2 <- glmmTMB(Total_Live_Culms ~ Prev_year_precip_scaled +
                        Aspect + PlotSlope_scaled + BGDensity_scaled + 
                        ShrubCover_scaled + HerbCover_scaled +
                        (1 | Transect),
                      data = dat,
                      family = genpois)
summary(pos.total2)
r2(pos.total2)
res.pos.total2 <- simulateResiduals(pos.total2)
plotQQunif(res.pos.total2)
plotResiduals(res.pos.total2)
check_overdispersion(pos.total2) # underdispersion detected
check_zeroinflation(pos.total2) # model is underfitting zeros (ratio = 0.31)
check_collinearity(pos.total2)


# Version 3: Site, Transect
pos.total3 <- glmmTMB(Total_Live_Culms ~ Prev_year_precip_scaled +
                        Aspect + PlotSlope_scaled + BGDensity_scaled + 
                        ShrubCover_scaled + HerbCover_scaled +
                        (1 | Site) + (1 | Transect),
                      data = dat,
                      family = genpois)
summary(pos.total3)
r2(pos.total3)
res.pos.total3 <- simulateResiduals(pos.total3)
plotQQunif(res.pos.total3)
plotResiduals(res.pos.total3)
check_overdispersion(pos.total3) # no overdispersion detected
check_zeroinflation(pos.total3) # model is underfitting zeros (ratio = 0.17)
check_collinearity(pos.total3)


# Version 4: Nested Site / Transect / Plant_ID
pos.total4 <- glmmTMB(Total_Live_Culms ~ Prev_year_precip_scaled +
                        Aspect + PlotSlope_scaled + BGDensity_scaled + 
                        ShrubCover_scaled + HerbCover_scaled +
                        (1 | Site / Transect / Plant_ID),
                      data = dat,
                      family = genpois)
summary(pos.total4)
r2(pos.total4)
res.pos.total4 <- simulateResiduals(pos.total4)
plotQQunif(res.pos.total4)
plotResiduals(res.pos.total4)
check_overdispersion(pos.total4) # no overdispersion detected
check_zeroinflation(pos.total4) # model is underfitting zeros (ratio = 0.22)
check_collinearity(pos.total4)


# Version 5: Nested Site / Transect, add precip interactions
pos.total5 <- glmmTMB(Total_Live_Culms ~ Prev_year_precip_scaled +
                        Aspect + PlotSlope_scaled + BGDensity_scaled + 
                        ShrubCover_scaled + HerbCover_scaled +
                        Prev_year_precip_scaled * BGDensity_scaled +
                        Prev_year_precip_scaled * ShrubCover_scaled +
                        Prev_year_precip_scaled * HerbCover_scaled +
                        (1 | Site / Transect),
                      data = dat,
                      family = genpois)
summary(pos.total5)
r2(pos.total5)
res.pos.total5 <- simulateResiduals(pos.total5)
plotQQunif(res.pos.total5)
plotResiduals(res.pos.total5)
check_overdispersion(pos.total5) # no overdispersion detected
check_zeroinflation(pos.total5) # model is underfitting zeros (ratio = 0.17)
check_collinearity(pos.total5)


# Version 6: Nested Site / Transect / Plant_ID, add precip interactions
pos.total6 <- glmmTMB(Total_Live_Culms ~ Prev_year_precip_scaled +
                        Aspect + PlotSlope_scaled + BGDensity_scaled + 
                        ShrubCover_scaled + HerbCover_scaled +
                        Prev_year_precip_scaled * BGDensity_scaled +
                        Prev_year_precip_scaled * ShrubCover_scaled +
                        Prev_year_precip_scaled * HerbCover_scaled +
                        (1 | Site / Transect / Plant_ID),
                      data = dat,
                      family = genpois)
summary(pos.total6)
r2(pos.total6)
res.pos.total6 <- simulateResiduals(pos.total6)
plotQQunif(res.pos.total6)
plotResiduals(res.pos.total6)
check_overdispersion(pos.total6) # no overdispersion detected
check_zeroinflation(pos.total6) # model is underfitting zeros (ratio = 0.20)
check_collinearity(pos.total6)

# Version 7: Site, Transect, Plant_ID, add precip interactions
pos.total7 <- glmmTMB(Total_Live_Culms ~ Prev_year_precip_scaled +
                        Aspect + PlotSlope_scaled + BGDensity_scaled + 
                        ShrubCover_scaled + HerbCover_scaled +
                        Prev_year_precip_scaled * BGDensity_scaled +
                        Prev_year_precip_scaled * ShrubCover_scaled +
                        Prev_year_precip_scaled * HerbCover_scaled +
                        (1 | Site ) + (1 | Transect) + (1 | Plant_ID),
                      data = dat,
                      family = genpois)
summary(pos.total7)
r2(pos.total7)
res.pos.total7 <- simulateResiduals(pos.total7)
plotQQunif(res.pos.total7)
plotResiduals(res.pos.total7)
check_overdispersion(pos.total7) # no overdispersion detected
check_zeroinflation(pos.total7) # model is underfitting zeros (ratio = 0.20)
check_collinearity(pos.total7)




## Zero-inflated Poisson --------------------------------------------------

# Version 4: Nested Site / Transect / Plant_ID
zip.total4 <- glmmTMB(Total_Live_Culms ~ Prev_year_precip_scaled +
                        Aspect + PlotSlope_scaled + ShrubCover_scaled + HerbCover_scaled +
                        (1 | Site / Transect / Plant_ID),
                      data = dat,
                      family = genpois,
                      ziformula = ~.)
summary(zip.total4)
r2(zip.total4)
res.zip.total4 <- simulateResiduals(zip.total4)
plotQQunif(res.zip.total4)
plotResiduals(res.zip.total4)
check_overdispersion(zip.total4) # no overdispersion detected
check_zeroinflation(zip.total4) # model is underfitting zeros (ratio = 0.22)
check_collinearity(zip.total4)


# Version 5: Nested Site / Transect, add precip interactions
zip.total5 <- glmmTMB(Total_Live_Culms ~ Prev_year_precip_scaled +
                        Aspect + PlotSlope_scaled + BGDensity_scaled + 
                        ShrubCover_scaled + HerbCover_scaled +
                        Prev_year_precip_scaled * BGDensity_scaled +
                        Prev_year_precip_scaled * ShrubCover_scaled +
                        Prev_year_precip_scaled * HerbCover_scaled +
                        (1 | Site / Transect),
                      data = dat,
                      family = genpois,
                      ziformula = ~.)
summary(zip.total5)
r2(zip.total5)
res.zip.total5 <- simulateResiduals(zip.total5)
plotQQunif(res.zip.total5)
plotResiduals(res.zip.total5)
check_overdispersion(zip.total5) # no overdispersion detected
check_zeroinflation(zip.total5) # model is underfitting zeros (ratio = 0.66)


# Version 6: Nested Site / Transect / Plant_ID, add precip interactions
zip.total6 <- glmmTMB(Total_Live_Culms ~ Prev_year_precip_scaled +
                        Aspect + PlotSlope_scaled + BGDensity_scaled + 
                        ShrubCover_scaled + HerbCover_scaled +
                        Prev_year_precip_scaled * BGDensity_scaled +
                        Prev_year_precip_scaled * ShrubCover_scaled +
                        Prev_year_precip_scaled * HerbCover_scaled +
                        (1 | Site / Transect / Plant_ID),
                      data = dat,
                      family = genpois,
                      ziformula = ~.)
summary(zip.total6)
r2(zip.total6)
res.zip.total6 <- simulateResiduals(zip.total6)
plotQQunif(res.zip.total6)
plotResiduals(res.zip.total6)
check_overdispersion(zip.total6) # no overdispersion detected
check_zeroinflation(zip.total6) # model is underfitting zeros (ratio = 0.64)


save(zip.total6,
     file = "RData/09_zip.total.6.RData")
