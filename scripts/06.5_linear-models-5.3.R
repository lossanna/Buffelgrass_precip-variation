# Created: 2025-07-11
# Updated: 2025-07-11

# Purpose: Compile version 3 linear models with Change_Reproductive_culms,  
#   Change_Total_Live_Culms, Change_BGDensity, Change_BGCover, and survival_perc 
#   as response variable.

# Models iteration 3a from 06.4_linear-models-5.0.R script for all except survival,
#   which is iteration 4 (Site had to be dropped due to high VIF).

# Explanatory variables include precip, aspect, plot slope, change in native cover, 
#   change in buffelgrass plot density, precip-biotic interactions, and site,
#   with (1 | Transect) as a random effect to account for repeat measures.
# Continuous explanatory variables are centered and scaled. 

# lme4 package used for all models except survival to use REML; survival modeled as 
#   Tweedie GLM with glmmTMB package.


library(tidyverse)
library(glmmTMB)
library(performance)
library(DHARMa)
library(lme4)
library(lmerTest)
library(MuMIn)

# Load data ---------------------------------------------------------------

dat <- read_csv("data/cleaned/04_demography-data_clean.csv")
culm.change.raw <- read_csv("data/cleaned/04_change-in-culm-density-cover_clean.csv")

# Data wrangling ----------------------------------------------------------

# Center and scale numeric variables
culm.change <- culm.change.raw %>% 
  mutate(PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         Change_BGDensity_scaled = scale(Change_BGDensity, scale = TRUE)[, 1],
         Change_ShrubCover_scaled = scale(Change_ShrubCover, scale = TRUE)[, 1],
         Change_HerbCover_scaled = scale(Change_HerbCover, scale = TRUE)[, 1])

culm.change.flat.rm <- culm.change %>% 
  filter(Aspect != "flat")

# Separate out plot-level data
plot.change <- culm.change.flat.rm %>% 
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm,
         -Change_Reproductive_culms, -Change_Total_Live_Culms) %>% 
  distinct(.keep_all = TRUE)


# Prepare survival data
dat.survival <- dat %>% 
  filter(!is.na(survival_perc),
         Aspect != "flat") %>% 
  mutate(Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         ShrubCover_scaled = scale(ShrubCover, center = TRUE, scale = TRUE)[, 1],
         HerbCover_scaled = scale(HerbCover, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         BGDensity_scaled = scale(BGDensity, center = TRUE, scale = TRUE)[, 1])

dat.survival <- dat.survival %>% 
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm) %>% 
  distinct(.keep_all = TRUE)



# Total culm change -------------------------------------------------------

# Global model
total <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                  Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  Site + (1 | Transect),
                data = culm.change.flat.rm)

# Model selection
options(na.action = "na.fail")
total_set <- dredge(total) 

# Examine best model
total_best.model <- get.models(total_set, 1)[[1]]
summary(total_best.model)
r2(total_best.model) # marginal: 0.359; conditional: 0.408
res.total_best.model <- simulateResiduals(total_best.model)
plotQQunif(res.total_best.model)
plotResiduals(res.total_best.model)
check_model(total_best.model) # posterior prediction poor at 0

# Examine models within 2 AICc units of best and assign each top model to separate object
total_top <- subset(total_set, delta <= 2) # 1 model

# R^2 of top models
r2(total_best.model) # marginal: 0.359; conditional: 0.408

# Model averaging of top models
#   averaging not needed for only 1 top model
summary(total_best.model)



# Reproductive culm change ------------------------------------------------

# Global model
repro <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                  Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  Site + (1 | Transect),
                data = culm.change.flat.rm)

# Model selection
options(na.action = "na.fail")
repro_set <- dredge(repro) 

# Examine best model
repro_best.model <- get.models(repro_set, 1)[[1]]
summary(repro_best.model)
r2(repro_best.model) # marginal: 0.292; conditional: 0.338
res.repro_best.model <- simulateResiduals(repro_best.model)
plotQQunif(res.repro_best.model)
plotResiduals(res.repro_best.model)
check_model(repro_best.model) # posterior prediction is poor at 0

# Examine models within 2 AICc units of best and assign each top model to separate object
repro_top <- subset(repro_set, delta <= 2) # 8 models
for (i in 1:nrow(repro_top)) {
  assign(paste0("repro_model", i), get.models(repro_top, subset = i)[[1]])
} 

# R^2 of top models
r2(repro_model1) # marginal: 0.292; conditional: 0.338
r2(repro_model2) # marginal: 0.291; conditional: 0.339
r2(repro_model3) # marginal: 0.292; conditional: 0.343
r2(repro_model4) # marginal: 0.293; conditional: 0.341
r2(repro_model5) # marginal: 0.292; conditional: 0.337
r2(repro_model6) # marginal: 0.293; conditional: 0.341
r2(repro_model7) # marginal: 0.292; conditional: 0.343
r2(repro_model8) # marginal: 0.291; conditional: 0.339

# Model averaging of top models
repro_avg <- model.avg(repro_set, subset = delta <= 2)
summary(repro_avg)



# Buffelgrass density change ----------------------------------------------

# Global model
bgden <- lmer(Change_BGDensity ~ Prev_year_precip_scaled +
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                  Change_HerbCover_scaled + 
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  Site + (1 | Transect),
                data = plot.change)

# Model selection
options(na.action = "na.fail")
bgden_set <- dredge(bgden) 

# Examine best model
bgden_best.model <- get.models(bgden_set, 1)[[1]]
summary(bgden_best.model)
r2(bgden_best.model) # marginal: 0.411; conditional: 0.598
res.bgden_best.model <- simulateResiduals(bgden_best.model)
plotQQunif(res.bgden_best.model)
plotResiduals(res.bgden_best.model) # looks a bit janky
check_model(bgden_best.model) # posterior prediction is poor around 0-1

# Examine models within 2 AICc units of best and assign each top model to separate object
bgden_top <- subset(bgden_set, delta <= 2) # 2 models
for (i in 1:nrow(bgden_top)) {
  assign(paste0("bgden_model", i), get.models(bgden_top, subset = i)[[1]])
} 

# R^2 of top models
r2(bgden_model1) # marginal: 0.411; conditional: 0.598
r2(bgden_model2) # marginal: 0.414; conditional: 0.598

# Model averaging of top models
bgden_avg <- model.avg(bgden_set, subset = delta <= 2)
summary(bgden_avg)



# Buffelgrass cover change ------------------------------------------------

# Global model
bgcov <- lmer(Change_BGCover ~ Prev_year_precip_scaled +
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                  Change_HerbCover_scaled + 
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  Site + (1 | Transect),
                data = plot.change)

# Model selection
options(na.action = "na.fail")
bgcov_set <- dredge(bgcov) 

# Examine best model
bgcov_best.model <- get.models(bgcov_set, 1)[[1]]
summary(bgcov_best.model)
r2(bgcov_best.model) # marginal: 0.248; conditional: 0.377
res.bgcov_best.model <- simulateResiduals(bgcov_best.model)
plotQQunif(res.bgcov_best.model)
plotResiduals(res.bgcov_best.model)
check_model(bgcov_best.model) 

# Examine models within 2 AICc units of best and assign each top model to separate object
bgcov_top <- subset(bgcov_set, delta <= 2) # 5 models
for (i in 1:nrow(bgcov_top)) {
  assign(paste0("bgcov_model", i), get.models(bgcov_top, subset = i)[[1]])
} 

# R^2 of top models
r2(bgcov_model1) # marginal: 0.248; conditional: 0.377
r2(bgcov_model2) # marginal: 0.248; conditional: 0.377
r2(bgcov_model3) # marginal: 0.248; conditional: 0.377
r2(bgcov_model4) # marginal: 0.238; conditional: 0.375
r2(bgcov_model5) # marginal: 0.238; conditional: 0.376

# Model averaging of top models
bgcov_avg <- model.avg(bgcov_set, subset = delta <= 2)
summary(bgcov_avg)



# Survival ----------------------------------------------------------------

# Global model
survival <- glmmTMB(survival_perc ~ Prev_year_precip_scaled +
                       Aspect + PlotSlope_scaled + ShrubCover_scaled +
                       HerbCover_scaled + BGDensity_scaled +
                       Prev_year_precip_scaled * ShrubCover_scaled +
                       Prev_year_precip_scaled * HerbCover_scaled +
                       Prev_year_precip_scaled * BGDensity_scaled +
                       (1 | Transect),
                     data = dat.survival,
                     family = tweedie(link = "log"))

# Model selection
options(na.action = "na.fail")
survival_set <- dredge(survival)

# Examine best model
survival_best.model <- get.models(survival_set, 1)[[1]]
summary(survival_best.model)
r2(survival_best.model) # marginal: 0.554; conditional: 0.833
res.survival_best.model <- simulateResiduals(survival_best.model)
plotQQunif(res.survival_best.model)
plotResiduals(res.survival_best.model) # kind of janky
check_model(survival_best.model) # posterior prediction looks weird at 0

# Examine models within 2 AICc units of best and assign each top model to separate object
survival_top <- subset(survival_set, delta <= 2) # 8 models
for (i in 1:nrow(survival_top)) {
  assign(paste0("survival_model", i), get.models(survival_top, subset = i)[[1]])
} 

# R^2 of top models
r2(survival_model1) # marginal: 0.554; conditional: 0.833
r2(survival_model2) # marginal: 0.571; conditional: 0.824
r2(survival_model3) # marginal: 0.565; conditional: 0.820
r2(survival_model4) # marginal: 0.551; conditional: 0.842
r2(survival_model5) # marginal: 0.558; conditional: 0.832
r2(survival_model6) # marginal: 0.559; conditional: 0.826
r2(survival_model7) # marginal: 0.563; conditional: 0.829
r2(survival_model8) # marginal: 0.544; conditional: 0.831


# Model averaging of top models
survival_avg <- model.avg(survival_set, subset = delta <= 2)
summary(survival_avg)


save.image("RData/06.5_linear-models-5.3.RData")
