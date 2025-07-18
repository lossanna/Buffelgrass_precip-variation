# Created: 2025-07-15
# Updated: 2025-07-18

# Purpose: Compile version 1 linear models with Change_Reproductive_culms,  
#   Change_Total_Live_Culms, Change_BGDensity, Change_BGCover, and survival_perc 
#   as response variable.

# Models iteration 1a from 06.4_linear-models-5.0.R script for all.

# Explanatory variables include precip, aspect, plot slope, change in native cover, 
#   change in buffelgrass plot density, precip-biotic interactions, and site,
#   with (1 | Site / Transect) as a random effect to account for repeat measures.
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
library(modelbased)

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
                  (1 | Site / Transect),
                data = culm.change.flat.rm)

# Model selection
options(na.action = "na.fail")
total_set <- dredge(total) 

# Examine best model
total_best.model <- get.models(total_set, 1)[[1]]
summary(total_best.model)
r2(total_best.model) # marginal: 0.119; conditional: 0.410
check_model(total_best.model) # posterior prediction is poor around 0

# Examine models within 2 AICc units of best and assign each top model to separate object
total_top <- subset(total_set, delta <= 2) # 1 model

# R^2 of top models
r2(total_best.model) # marginal: 0.119; conditional: 0.410

# Model averaging of top models
#   model averaging not needed for lme4 version; only 1 top model
summary(total_best.model)

# Predicted vs. observed
total_pred <- estimate_expectation(total_best.model)
total_pred$Change_Total_Live_Culms <- culm.change.flat.rm$Change_Total_Live_Culms

total_pred %>% 
  ggplot(aes(x = Change_Total_Live_Culms, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1)



# Reproductive culm change ------------------------------------------------

# Global model
repro <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                  Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  (1 | Site / Transect),
                data = culm.change.flat.rm)

# Model selection
options(na.action = "na.fail")
repro_set <- dredge(repro) # got warning about convergence, but table looks complete (Model failed to converge with max|grad| = 0.00295032 (tol = 0.002, component 1))

# Examine best model
repro_best.model <- get.models(repro_set, 1)[[1]]
summary(repro_best.model)
r2(repro_best.model) # marginal: 0.094; conditional: 0.299
check_model(repro_best.model) # posterior prediction is poor at 0

# Examine models within 2 AICc units of best and assign each top model to separate object
repro_top <- subset(repro_set, delta <= 2) # 8 models
for (i in 1:nrow(repro_top)) {
  assign(paste0("repro_model", i), get.models(repro_top, subset = i)[[1]])
} 

# R^2 of top models
r2(repro_model1) # marginal: 0.094; conditional: 0.299
r2(repro_model2) # marginal: 0.095; conditional: 0.301
r2(repro_model3) # marginal: 0.096; conditional: 0.298
r2(repro_model4) # marginal: 0.092; conditional: 0.302
r2(repro_model5) # marginal: 0.095; conditional: 0.296
r2(repro_model6) # marginal: 0.098; conditional: 0.295
r2(repro_model7) # marginal: 0.097; conditional: 0.298
r2(repro_model8) # marginal: 0.094; conditional: 0.300

# Model averaging of top models
repro_avg <- model.avg(repro_set, subset = delta <= 2)
summary(repro_avg)

# Predicted vs. observed (best model)
repro_pred <- estimate_expectation(repro_best.model)
repro_pred$Change_Reproductive_culms <- culm.change.flat.rm$Change_Reproductive_culms

repro_pred %>% 
  ggplot(aes(x = Change_Reproductive_culms, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1)



# Buffelgrass density change ----------------------------------------------

# Global model
bgden <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + 
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                  Change_HerbCover_scaled + 
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  (1 | Site / Transect),
                data = plot.change)

# Model selection
options(na.action = "na.fail")
bgden_set <- dredge(bgden) # got warning about convergence, but table looks complete (Model failed to converge with 1 negative eigenvalue: -8.2e-01)

# Examine best model
bgden_best.model <- get.models(bgden_set, 1)[[1]]
summary(bgden_best.model)
r2(bgden_best.model) # marginal: 0.305; conditional: 0.562
res.bgden_best.model <- simulateResiduals(bgden_best.model)
plotQQunif(res.bgden_best.model)
plotResiduals(res.bgden_best.model)
check_model(bgden_best.model) # posterior prediction is poor around 0-1

# Examine models within 2 AICc units of best and assign each top model to separate object
bgden_top <- subset(bgden_set, delta <= 2) # 2 models
for (i in 1:nrow(bgden_top)) {
  assign(paste0("bgden_model", i), get.models(bgden_top, subset = i)[[1]])
} 

# R^2 of top models
r2(bgden_model1) # marginal: 0.305; conditional: 0.562
r2(bgden_model2) # marginal: 0.301; conditional: 0.567

# Model averaging of top models
bgden_avg <- model.avg(bgden_set, subset = delta <= 2)
summary(bgden_avg)

# Predicted vs. observed (best model)
bgden_pred <- estimate_expectation(bgden_best.model)
bgden_pred$Change_BGDensity <- plot.change$Change_BGDensity

bgden_pred %>% 
  ggplot(aes(x = Change_BGDensity, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1)



# Buffelgrass cover -------------------------------------------------------

# Global model
bgcov <- lmer(Change_BGCover ~ Prev_year_precip_scaled + 
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                  Change_HerbCover_scaled + 
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  (1 | Site / Transect),
                data = plot.change)

# Model selection
options(na.action = "na.fail")
bgcov_set <- dredge(bgcov) 

# Examine best model
bgcov_best.model <- get.models(bgcov_set, 1)[[1]]
summary(bgcov_best.model)
r2(bgcov_best.model) # marginal: 0.240; conditional: 0.393
check_model(bgcov_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
bgcov_top <- subset(bgcov_set, delta <= 2) # 3 models
for (i in 1:nrow(bgcov_top)) {
  assign(paste0("bgcov_model", i), get.models(bgcov_top, subset = i)[[1]])
} 

# R^2 of top models
r2(bgcov_model1) # marginal: 0.240; conditional: 0.393
r2(bgcov_model2) # marginal: 0.239; conditional: 0.393
r2(bgcov_model3) # marginal: 0.240; conditional: 0.391

# Model averaging of top models
bgcov_avg <- model.avg(bgcov_set, subset = delta <= 2)
summary(bgcov_avg)

# Predicted vs. observed (best model)
bgcov_pred <- estimate_expectation(bgcov_best.model)
bgcov_pred$Change_BGCover <- plot.change$Change_BGCover

bgcov_pred %>% 
  ggplot(aes(x = Change_BGCover, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1)



# Survival ----------------------------------------------------------------

# Global model
survival <- glmmTMB(survival_perc ~ Prev_year_precip_scaled +
                       Aspect + PlotSlope_scaled + ShrubCover_scaled +
                       HerbCover_scaled + BGDensity_scaled +
                       Prev_year_precip_scaled * ShrubCover_scaled +
                       Prev_year_precip_scaled * HerbCover_scaled +
                       Prev_year_precip_scaled * BGDensity_scaled +
                       (1 | Site / Transect),
                     data = dat.survival,
                     family = tweedie(link = "log"))

# Model selection
options(na.action = "na.fail")
survival_set <- dredge(survival) # not all converged

# Examine best model
survival_best.model <- get.models(survival_set, 1)[[1]]
summary(survival_best.model)
r2(survival_best.model) # marginal: 0.362; conditional: 0.908
res.survival_best.model <- simulateResiduals(survival_best.model)
plotQQunif(res.survival_best.model)
plotResiduals(res.survival_best.model) # really does not look great
check_model(survival_best.model) # posterior prediction looks weird

# Examine models within 2 AICc units of best and assign each top model to separate object
survival_top <- subset(survival_set, delta <= 2) %>% 
  filter(!is.na(df)) # not all models converged; 17 top models
for (i in 1:nrow(survival_top)) {
  assign(paste0("survival_model", i), get.models(survival_top, subset = i)[[1]])
} 

# R^2 of top models
r2(survival_model1) # marginal: 0.362; conditional: 0.908
r2(survival_model2) # marginal: 0.349; conditional: 0.915
r2(survival_model3) # marginal: 0.358; conditional: 0.905
r2(survival_model4) # can't compute
r2(survival_model5) # marginal: 0.329; conditional: 0.919
r2(survival_model6) # marginal: 0.341; conditional: 0.917
r2(survival_model7) # marginal: 0.337; conditional: 0.914
r2(survival_model8) # marginal: 0.345; conditional: 0.912
r2(survival_model9) # marginal: 0.355; conditional: 0.908
r2(survival_model10) # marginal: 0.350; conditional: 0.906
r2(survival_model11) # marginal: 0.341; conditional: 0.913
r2(survival_model12) # can't compute
r2(survival_model13) # marginal: 0.353; conditional: 0.911
r2(survival_model14) # can't compute
r2(survival_model15) # marginal: 0.342; conditional: 0.917
r2(survival_model16) # marginal: 0.353; conditional: 0.911
r2(survival_model17) # marginal: 0.330; conditional: 0.920


# Model averaging of top models
survival_set_with.delta <- survival_set %>% 
  filter(!is.na(delta))
survival_avg <- model.avg(survival_set_with.delta, subset = delta <= 2)
summary(survival_avg)

# Predicted vs. observed (best model)
survival_pred <- estimate_expectation(survival_best.model)
survival_pred$survival_perc <- dat.survival$survival_perc

survival_pred %>% 
  ggplot(aes(x = survival_perc, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1)




save.image("RData/06.6_linear-models-5.1.RData")
save(total_best.model,
     file = "RData/06.6-lm-5.1_total-best-model.RData")
