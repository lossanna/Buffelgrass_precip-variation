# Created: 2026-02-04
# Updated: 2026-02-05

# Purpose: Rerun models and include inital BG density, shrub cover, and herb cover values
#   as explanatory variables.

library(tidyverse)
library(glmmTMB)
library(performance)
library(DHARMa)
library(lme4)
library(lmerTest)
library(modelbased)

# Load data ---------------------------------------------------------------

dat <- read_csv("data/cleaned/11.1_demography-data_clean.csv")
culm.change.raw <- read_csv("data/cleaned/11.1_change-in-culm-density-cover_clean.csv")
survival.dat <- read_csv("data/cleaned/11.2_survival-data_clean.csv")

# Data wrangling ----------------------------------------------------------

# Center and scale numeric variables for culm change data
culm.change.flat.rm <- culm.change.raw %>% 
  filter(Aspect != "flat") %>% 
  mutate(PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         Change_BGDensity_scaled = scale(Change_BGDensity, scale = TRUE)[, 1],
         Change_ShrubCover_scaled = scale(Change_ShrubCover, scale = TRUE)[, 1],
         Change_HerbCover_scaled = scale(Change_HerbCover, scale = TRUE)[, 1],
         Change_BGCover_scaled = scale(Change_BGCover, scale = TRUE)[, 1],
         Init_BGDensity_scaled = scale(Init_BGDensity, scale = TRUE)[, 1],
         Init_ShrubCover_scaled = scale(Init_ShrubCover, scale = TRUE)[, 1],
         Init_HerbCover_scaled = scale(Init_HerbCover, scale = TRUE)[, 1],
         Init_BGCover_scaled = scale(Init_BGCover, scale = TRUE)[, 1]) %>% 
  mutate(Plant_ID = as.character(Plant_ID))

# Center and scale numeric variables for plot-level data
plot.change <- culm.change.raw %>% 
  select(-Plant_ID, -Change_Reproductive_culms, -Change_Total_Live_Culms) %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(Aspect != "flat") %>% 
  mutate(PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         Change_ShrubCover_scaled = scale(Change_ShrubCover, scale = TRUE)[, 1],
         Change_HerbCover_scaled = scale(Change_HerbCover, scale = TRUE)[, 1],
         Change_BGCover_scaled = scale(Change_BGCover, scale = TRUE)[, 1],
         Init_BGDensity_scaled = scale(Init_BGDensity, scale = TRUE)[, 1],
         Init_ShrubCover_scaled = scale(Init_ShrubCover, scale = TRUE)[, 1],
         Init_HerbCover_scaled = scale(Init_HerbCover, scale = TRUE)[, 1],
         Init_BGCover_scaled = scale(Init_BGCover, scale = TRUE)[, 1]) %>% 
  mutate(Plot = as.character(Plot))


# Center and scale numeric variables for survival data
dat.survival <- survival.dat %>% 
  filter(Aspect != "flat") %>% 
  mutate(Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         ShrubCover_scaled = scale(ShrubCover, center = TRUE, scale = TRUE)[, 1],
         HerbCover_scaled = scale(HerbCover, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         BGDensity_scaled = scale(BGDensity, center = TRUE, scale = TRUE)[, 1],
         Init_BGDensity_scaled = scale(Init_BGDensity, scale = TRUE)[, 1],
         Init_ShrubCover_scaled = scale(Init_ShrubCover, scale = TRUE)[, 1],
         Init_HerbCover_scaled = scale(Init_HerbCover, scale = TRUE)[, 1],
         Init_BGCover_scaled = scale(Init_BGCover, scale = TRUE)[, 1]) %>% 
  mutate(Plot = as.character(Plot))


# Total culm change -------------------------------------------------------

# Version 1: Initial conditions as numeric, nested Site/Transect
total1 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_BGDensity_scaled +
                 Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 (1 | Site / Transect),
               data = culm.change.flat.rm)
summary(total1)
r2(total1) # marginal: 0.127; conditional: 0.417
res.total1 <- simulateResiduals(total1)
plotQQunif(res.total1)
plotResiduals(res.total1) 
check_collinearity(total1)


# Version 2: Initial conditions as numeric, nested Site/Transect/Plant_ID
total2 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_BGDensity_scaled +
                 Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 (1 | Site / Transect / Plant_ID),
               data = culm.change.flat.rm)
summary(total2)
r2(total2) # marginal: 0.129; conditional: 0.463
res.total2 <- simulateResiduals(total2)
plotQQunif(res.total2)
plotResiduals(res.total2) 

#   Predicted vs. observed 
total2.pred <- estimate_expectation(total2) # idk why there are only 1242 rows
total1.pred <- estimate_expectation(total1) # idk why there are only 1242 rows



# Reproductive culm change ------------------------------------------------

# Version 1: Initial conditions as numeric, nested Site/Transect
repro1 <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_BGDensity_scaled +
                 Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 (1 | Site / Transect),
               data = culm.change.flat.rm)
summary(repro1)
r2(repro1) # marginal: 0.099; conditional: 0.306
res.repro1 <- simulateResiduals(repro1)
plotQQunif(res.repro1)
plotResiduals(res.repro1) 
check_collinearity(repro1)


# Version 2: Initial conditions as numeric, nested Site/Transect/Plant_ID
repro2 <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_BGDensity_scaled +
                 Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 (1 | Site / Transect / Plant_ID),
               data = culm.change.flat.rm)
summary(repro2) # singular fit issues
r2(repro2) # marginal: 0.125; conditional: can't calculate
res.repro2 <- simulateResiduals(repro2)
plotQQunif(res.repro2)
plotResiduals(res.repro2) 

#   Predicted vs. observed 
repro2.pred <- estimate_expectation(repro2) # idk why there are only 1242 rows



# BG density change -------------------------------------------------------

# Version 1: Initial conditions as numeric, nested Site/Transect
bgden1 <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + 
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 (1 | Site / Transect),
               data = plot.change)
summary(bgden1)
r2(bgden1) # marginal: 0.423; conditional: 0.465
res.bgden1 <- simulateResiduals(bgden1)
plotQQunif(res.bgden1)
plotResiduals(res.bgden1) 
check_collinearity(bgden1)


# Version 2: Initial conditions as numeric, nested Site/Transect/Plot
bgden2 <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + 
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 (1 | Site / Transect / Plot),
               data = plot.change)
summary(bgden2) # singular fit issues
r2(bgden2) # marginal: 0.441; conditional: can't compute
res.bgden2 <- simulateResiduals(bgden2)
plotQQunif(res.bgden2)
plotResiduals(res.bgden2)

#   Predicted vs. observed
bgden2.pred <- estimate_expectation(bgden2) # this also is fewer rows than it should be



# BG cover change ---------------------------------------------------------

# Version 1: Initial conditions as numeric, nested Site/Transect
bgcov1 <- lmer(Change_BGCover ~ Prev_year_precip_scaled + 
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 (1 | Site / Transect),
               data = plot.change)
summary(bgcov1)
r2(bgcov1) # marginal: 0.271; conditional: 0.309
res.bgcov1 <- simulateResiduals(bgcov1)
plotQQunif(res.bgcov1)
plotResiduals(res.bgcov1) 

# Version 2: Initial conditions as numeric, nested Site/Transect/Plot
bgcov2 <- lmer(Change_BGCover ~ Prev_year_precip_scaled + 
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 (1 | Site / Transect / Plot),
               data = plot.change)
summary(bgcov2) # singular fit issues
r2(bgcov2) # marginal: 0.282; conditional: can't calculate
res.bgcov2 <- simulateResiduals(bgcov2)
plotQQunif(res.bgcov2)
plotResiduals(res.bgcov2) 



# Survival ----------------------------------------------------------------

# Version 1: Initial conditions as numeric, nested Site/Transect
survival1 <- glmer(cbind(seedlings_surviving,
                         remaining_toothpicks - seedlings_surviving) ~ Prev_year_precip_scaled + 
                     Aspect + PlotSlope_scaled + BGDensity_scaled +
                     ShrubCover_scaled + HerbCover_scaled + 
                     Prev_year_precip_scaled * BGDensity_scaled + 
                     Prev_year_precip_scaled * ShrubCover_scaled +
                     Prev_year_precip_scaled * HerbCover_scaled +
                     (1 | Site / Transect),
                   family = binomial,
                   data = dat.survival)
summary(survival1)
r2(survival1) # marginal: 0.493; conditional: 0.966
res.survival1 <- simulateResiduals(survival1)
plotQQunif(res.survival1)
plotResiduals(res.survival1) 
check_collinearity(survival1)


# Version 2: Initial conditions as numeric, nested Site/Transect/Plot
survival2 <- glmer(cbind(seedlings_surviving,
                         remaining_toothpicks - seedlings_surviving) ~ Prev_year_precip_scaled + 
                     Aspect + PlotSlope_scaled + BGDensity_scaled +
                     ShrubCover_scaled + HerbCover_scaled + 
                     Prev_year_precip_scaled * BGDensity_scaled + 
                     Prev_year_precip_scaled * ShrubCover_scaled +
                     Prev_year_precip_scaled * HerbCover_scaled +
                     (1 | Site / Transect / Plot),
                   family = binomial,
                   data = dat.survival) # convergence issues
summary(survival2)
r2(survival2) # marginal: 0.488; conditional: 0.967
res.survival2 <- simulateResiduals(survival2)
plotQQunif(res.survival2)
plotResiduals(res.survival2) 


# Save --------------------------------------------------------------------

# Needed for graphs
save(culm.change.flat.rm, plot.change, dat.survival, 
     total1, repro1, bgden1, bgcov1, survival1,
     file = "RData/12.1_data-and-models-revision1.2.RData")


save.image("RData/12.1_linear-models-revision1.2.RData")
