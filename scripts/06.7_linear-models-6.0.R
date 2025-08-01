# Created: 2025-08-01
# Updated: 2025-08-01

# Purpose: Compile linear models with Change_Reproductive_culms, Change_Total_Live_Culms, 
#  Change_BGDensity, Change_BGCover, and survival_transf as response variable.

# Continuous explanatory variables are centered and scaled. 
# lme4 package used for all models except survival to use REML; survival modeled as 
#   beta regression with glmmTMB package.

# Change from 06.6_linear-models-5.1.R:
#   plot.change data had duplicates of the same observation; was 238 rows but should
#     be 133 (duplicates created because some plants had different Notes column value);
#   dat.survival data also had the same duplicates due to Notes column; was 228 rows but should be
#     only 138 rows.
#   Total and repro culm data did not change, but I have replicated model versions of different random effects.

# Random effects tested: 
#   1. (1|Site/Transect)
#   2. (1|Site)
#   3. (1|Transect)

# Differs from v5.0 comparisons because only lme4 versions are considered (except for survival), 
#   Site not included as fixed effect, and survival is beta regression.

# Predicted vs. actual graphs (made with modelbased) don't really look different between any of 
#   the three versions of random effects.
# Model diagnostics also look pretty similar for all three versions.

# Issues with random effects (singularity or couldn't compute conditional R2) for density models v1 & v2,
#   and for survival model v1, now that there are fewer rows.

# Will go with version 3 for all (1|Transect), as it is easier to apply the same random effect to all mdoels,
#   and this is the only one that doesn't cause problems for density model.


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
         -Change_Reproductive_culms, -Change_Total_Live_Culms, -Notes) %>% 
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
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm,
         -Notes) %>% 
  distinct(.keep_all = TRUE)

#   Transform 0s and 1s
dat.survival <- dat.survival %>% 
  mutate(survival_transf = pmin(pmax(survival_perc, 1e-6), 1 - 1e-6))



# Total culm change -------------------------------------------------------

# Model diagnostics and predicted vs. actual plots look pretty similar for all 3 versions.
#   (Posterior predictive check notably poor at 0; other diagnostics look okay.)

# No singularity issues with random effects for any version.


## Total 1: (1 | Site / Transect) -----------------------------------------

# Global model
total1 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                Change_BGDensity_scaled +
                Prev_year_precip_scaled * Change_BGDensity_scaled +
                Prev_year_precip_scaled * Change_ShrubCover_scaled +
                Prev_year_precip_scaled * Change_HerbCover_scaled +
                (1 | Site / Transect),
              data = culm.change.flat.rm)

# Model selection
options(na.action = "na.fail")
total1_set <- dredge(total1) 

# Examine best model
total1_best.model <- get.models(total1_set, 1)[[1]]
summary(total1_best.model)
r2(total1_best.model) # marginal: 0.119; conditional: 0.410
res.total1_best.model <- simulateResiduals(total1_best.model)
plotQQunif(res.total1_best.model)
plotResiduals(res.total1_best.model) # mostly fine
check_model(total1_best.model) # posterior prediction is poor around 0

# Examine models within 2 AICc units of best and assign each top model to separate object
total1_top <- subset(total1_set, delta <= 2) # 1 model

# R^2 of top models
r2(total1_best.model) # marginal: 0.119; conditional: 0.410

# Model averaging of top models
#   model averaging not needed for lme4 version; only 1 top model
summary(total1_best.model)

# Predicted vs. observed
total1_pred <- estimate_expectation(total1_best.model)
total1_pred$Change_Total_Live_Culms <- culm.change.flat.rm$Change_Total_Live_Culms

total1_pred.plot <- total1_pred %>% 
  ggplot(aes(x = Change_Total_Live_Culms, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  ggtitle("Total culm change model, predicted vs. observed") +
  xlab(expression(Delta ~ "Total live culms [observed]")) +
  theme_bw()
total1_pred.plot


## Total 2: (1 | Site) ----------------------------------------------------

# Global model
total2 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Site),
               data = culm.change.flat.rm)

# Model selection
options(na.action = "na.fail")
total2_set <- dredge(total2) 

# Examine best model
total2_best.model <- get.models(total2_set, 1)[[1]]
summary(total2_best.model)
r2(total2_best.model) # marginal: 0.137; conditional: 0.363
res.total2_best.model <- simulateResiduals(total2_best.model)
plotQQunif(res.total2_best.model)
plotResiduals(res.total2_best.model) # a little janky, but mostly fine
check_model(total2_best.model) # posterior prediction is poor around 0

# Examine models within 2 AICc units of best and assign each top model to separate object
total2_top <- subset(total2_set, delta <= 2) # 3 models
for (i in 1:nrow(total2_top)) {
  assign(paste0("total2_model", i), get.models(total2_top, subset = i)[[1]])
} 

# R^2 of top models
r2(total2_model1) # marginal: 0.137; conditional: 0.363
r2(total2_model2) # marginal: 0.138; conditional: 0.363
r2(total2_model3) # marginal: 0.142; conditional: 0.355

# Model averaging of top models
total2_avg <- model.avg(total2_set, subset = delta <= 2)
summary(total2_avg)

# Predicted vs. observed
total2_pred <- estimate_expectation(total2_best.model)
total2_pred$Change_Total_Live_Culms <- culm.change.flat.rm$Change_Total_Live_Culms

total2_pred.plot <- total2_pred %>% 
  ggplot(aes(x = Change_Total_Live_Culms, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  ggtitle("Total culm change model, predicted vs. observed") +
  xlab(expression(Delta ~ "Total live culms [observed]")) +
  theme_bw()
total2_pred.plot


## Total 3: (1 | Transect) ------------------------------------------------

# Global model
total3 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Transect),
               data = culm.change.flat.rm)

# Model selection
options(na.action = "na.fail")
total3_set <- dredge(total3) 

# Examine best model
total3_best.model <- get.models(total3_set, 1)[[1]]
summary(total3_best.model)
r2(total3_best.model) # marginal: 0.144; conditional: 0.391
res.total3_best.model <- simulateResiduals(total3_best.model)
plotQQunif(res.total3_best.model)
plotResiduals(res.total3_best.model) # mostly fine
check_model(total3_best.model) # posterior prediction is poor around 0

# Examine models within 2 AICc units of best and assign each top model to separate object
total3_top <- subset(total3_set, delta <= 2) # 2 models
for (i in 1:nrow(total3_top)) {
  assign(paste0("total3_model", i), get.models(total3_top, subset = i)[[1]])
} 

# R^2 of top models
r2(total3_model1) # marginal: 0.144; conditional: 0.391
r2(total3_model2) # marginal: 0.147; conditional: 0.386

# Model averaging of top models
total3_avg <- model.avg(total3_set, subset = delta <= 2)
summary(total3_avg)

# Predicted vs. observed
total3_pred <- estimate_expectation(total3_best.model)
total3_pred$Change_Total_Live_Culms <- culm.change.flat.rm$Change_Total_Live_Culms

total3_pred.plot <- total3_pred %>% 
  ggplot(aes(x = Change_Total_Live_Culms, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  ggtitle("Total culm change model, predicted vs. observed") +
  xlab(expression(Delta ~ "Total live culms [observed]")) +
  theme_bw()
total3_pred.plot



# Reproductive culm change ------------------------------------------------

# Model diagnostics and predicted vs. actual plots look pretty similar for all 3 versions.
#   (Posterior predictive check notably poor around 0, and has a small hump around 25;
#     other diagnostics look okay.)

# No singularity issues with random effects for any version.


## Repro 1: (1 | Site / Transect ------------------------------------------

# Global model
repro1 <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                Change_BGDensity_scaled +
                Prev_year_precip_scaled * Change_BGDensity_scaled +
                Prev_year_precip_scaled * Change_ShrubCover_scaled +
                Prev_year_precip_scaled * Change_HerbCover_scaled +
                (1 | Site / Transect),
              data = culm.change.flat.rm)

# Model selection
options(na.action = "na.fail")
repro1_set <- dredge(repro1) # got warning about convergence, but table looks complete (Model failed to converge with max|grad| = 0.00295032 (tol = 0.002, component 1))

# Examine best model
repro1_best.model <- get.models(repro1_set, 1)[[1]]
summary(repro1_best.model)
r2(repro1_best.model) # marginal: 0.094; conditional: 0.299
res.repro1_best.model <- simulateResiduals(repro1_best.model)
plotQQunif(res.repro1_best.model)
plotResiduals(res.repro1_best.model) # a bit janky
check_model(repro1_best.model) # posterior prediction is poor at 0

# Examine models within 2 AICc units of best and assign each top model to separate object
repro1_top <- subset(repro1_set, delta <= 2) # 8 models
for (i in 1:nrow(repro1_top)) {
  assign(paste0("repro1_model", i), get.models(repro1_top, subset = i)[[1]])
} 

# R^2 of top models
r2(repro1_model1) # marginal: 0.094; conditional: 0.299
r2(repro1_model2) # marginal: 0.095; conditional: 0.301
r2(repro1_model3) # marginal: 0.096; conditional: 0.298
r2(repro1_model4) # marginal: 0.092; conditional: 0.302
r2(repro1_model5) # marginal: 0.095; conditional: 0.296
r2(repro1_model6) # marginal: 0.098; conditional: 0.295
r2(repro1_model7) # marginal: 0.097; conditional: 0.298
r2(repro1_model8) # marginal: 0.094; conditional: 0.300

# Model averaging of top models
repro1_avg <- model.avg(repro1_set, subset = delta <= 2)
summary(repro1_avg)

# Predicted vs. observed (best model)
repro1_pred <- estimate_expectation(repro1_best.model)
repro1_pred$Change_Reproductive_culms <- culm.change.flat.rm$Change_Reproductive_culms

repro1_pred.plot <- repro1_pred %>% 
  ggplot(aes(x = Change_Reproductive_culms, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  ggtitle("Reproductive culm change model, predicted vs. observed") +
  xlab(expression(Delta ~ "Reproductive culms [observed]")) +
  theme_bw()
repro1_pred.plot


## Repro 2: (1 | Site) ----------------------------------------------------

# Global model
repro2 <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Site),
               data = culm.change.flat.rm)

# Model selection
options(na.action = "na.fail")
repro2_set <- dredge(repro2) 

# Examine best model
repro2_best.model <- get.models(repro2_set, 1)[[1]]
summary(repro2_best.model)
r2(repro2_best.model) # marginal: 0.109; conditional: 0.249
res.repro2_best.model <- simulateResiduals(repro2_best.model)
plotQQunif(res.repro2_best.model)
plotResiduals(res.repro2_best.model) # a bit janky
check_model(repro2_best.model) # posterior prediction is poor at 0

# Examine models within 2 AICc units of best and assign each top model to separate object
repro2_top <- subset(repro2_set, delta <= 2) # 4 models
for (i in 1:nrow(repro2_top)) {
  assign(paste0("repro2_model", i), get.models(repro2_top, subset = i)[[1]])
} 

# R^2 of top models
r2(repro2_model1) # marginal: 0.109; conditional: 0.249
r2(repro2_model2) # marginal: 0.105; conditional: 0.250
r2(repro2_model3) # marginal: 0.109; conditional: 0.248
r2(repro2_model4) # marginal: 0.105; conditional: 0.251

# Model averaging of top models
repro2_avg <- model.avg(repro2_set, subset = delta <= 2)
summary(repro2_avg)

# Predicted vs. observed (best model)
repro2_pred <- estimate_expectation(repro2_best.model)
repro2_pred$Change_Reproductive_culms <- culm.change.flat.rm$Change_Reproductive_culms

repro2_pred.plot <- repro2_pred %>% 
  ggplot(aes(x = Change_Reproductive_culms, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  ggtitle("Reproductive culm change model, predicted vs. observed") +
  xlab(expression(Delta ~ "Reproductive culms [observed]")) +
  theme_bw()
repro2_pred.plot


## Repro 3: (1 | Transect) ------------------------------------------------

# Global model
repro3 <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Transect),
               data = culm.change.flat.rm)

# Model selection
options(na.action = "na.fail")
repro3_set <- dredge(repro3) 

# Examine best model
repro3_best.model <- get.models(repro3_set, 1)[[1]]
summary(repro3_best.model)
r2(repro3_best.model) # marginal: 0.155; conditional: 0.320
res.repro3_best.model <- simulateResiduals(repro3_best.model)
plotQQunif(res.repro3_best.model)
plotResiduals(res.repro3_best.model) # a bit janky
check_model(repro3_best.model) # posterior prediction is poor at 0

# Examine models within 2 AICc units of best and assign each top model to separate object
repro3_top <- subset(repro3_set, delta <= 2) # 5 models
for (i in 1:nrow(repro3_top)) {
  assign(paste0("repro3_model", i), get.models(repro3_top, subset = i)[[1]])
} 

# R^2 of top models
r2(repro3_model1) # marginal: 0.155; conditional: 0.320
r2(repro3_model2) # marginal: 0.152; conditional: 0.322
r2(repro3_model3) # marginal: 0.155; conditional: 0.318
r2(repro3_model4) # marginal: 0.151; conditional: 0.321
r2(repro3_model5) # marginal: 0.147; conditional: 0.333

# Model averaging of top models
repro3_avg <- model.avg(repro3_set, subset = delta <= 2)
summary(repro3_avg)

# Predicted vs. observed (best model)
repro3_pred <- estimate_expectation(repro3_best.model)
repro3_pred$Change_Reproductive_culms <- culm.change.flat.rm$Change_Reproductive_culms

repro3_pred.plot <- repro3_pred %>% 
  ggplot(aes(x = Change_Reproductive_culms, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  ggtitle("Reproductive culm change model, predicted vs. observed") +
  xlab(expression(Delta ~ "Reproductive culms [observed]")) +
  theme_bw()
repro3_pred.plot



# Buffelgrass density change ----------------------------------------------

# Model diagnostics and predicted vs. actual plots look pretty similar for all 3 versions.
#   (Posterior predictive check poor around 0-1)

# Singularity issues regarding random effects:
#   1. Top model 4, and 19 others.
#   2. Top models 1 & 5, and 22 others.
#   3. No issues with any.


## BG density 1: (1 | Site / Transect -------------------------------------

# Global model
bgden1 <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + 
                Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                Change_HerbCover_scaled + 
                Prev_year_precip_scaled * Change_ShrubCover_scaled +
                Prev_year_precip_scaled * Change_HerbCover_scaled +
                (1 | Site / Transect),
              data = plot.change)

# Model selection
options(na.action = "na.fail")
bgden1_set <- dredge(bgden1) # some models had trouble with random effects; got warning: boundary (singular) fit: see help('isSingular')

# Examine best model
bgden1_best.model <- get.models(bgden1_set, 1)[[1]]
summary(bgden1_best.model)
r2(bgden1_best.model) # marginal: 0.351; conditional: 0.457
res.bgden1_best.model <- simulateResiduals(bgden1_best.model)
plotQQunif(res.bgden1_best.model)
plotResiduals(res.bgden1_best.model) # mostly fine
check_model(bgden1_best.model) # posterior prediction is poor around 0-1 

# Examine models within 2 AICc units of best and assign each top model to separate object
bgden1_top <- subset(bgden1_set, delta <= 2) # 5 models
for (i in 1:nrow(bgden1_top)) {
  assign(paste0("bgden1_model", i), get.models(bgden1_top, subset = i)[[1]])
} 

# R^2 of top models
r2(bgden1_model1) # marginal: 0.350; conditional: 0.457
r2(bgden1_model2) # marginal: 0.364; conditional: 0.441
r2(bgden1_model3) # marginal: 0.341; conditional: 0.460
r2(bgden1_model4) # marginal: 0.403; conditional: can't compute
r2(bgden1_model5) # marginal: 0.354; conditional: 0.444

# Check singularity of top models
bgden1_top.models <- get.models(bgden1_set, subset = delta <= 2)
lapply(bgden1_top.models, isSingular) # model 4 has random effects issues

# Model averaging of top models
bgden1_avg <- model.avg(bgden1_set, subset = delta <= 2)
summary(bgden1_avg)

# Predicted vs. observed (best model)
bgden1_pred <- estimate_expectation(bgden1_best.model)
bgden1_pred$Change_BGDensity <- plot.change$Change_BGDensity

bgden1_pred.plot <- bgden1_pred %>% 
  ggplot(aes(x = Change_BGDensity, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  ggtitle("Plot density change model, predicted vs. observed") +
  xlab(expression(Delta ~ paste("Buffelgrass density (individuals / ", m^2, ") [observed]"))) +
  theme_bw()
bgden1_pred.plot


## BG density 2: (1 | Site) -----------------------------------------------

# Global model
bgden2 <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + 
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Site),
               data = plot.change) # boundary (singular) fit: see help('isSingular')

# Model selection
options(na.action = "na.fail")
bgden2_set <- dredge(bgden2) # some models had trouble with random effects; got warning: boundary (singular) fit: see help('isSingular')

# Examine best model
bgden2_best.model <- get.models(bgden2_set, 1)[[1]]
summary(bgden2_best.model)
r2(bgden2_best.model) # marginal: 0.398; conditional: can't compute
res.bgden2_best.model <- simulateResiduals(bgden2_best.model)
plotQQunif(res.bgden2_best.model)
plotResiduals(res.bgden2_best.model) # mostly fine
check_model(bgden2_best.model) # posterior prediction is poor around 0-1

# Examine models within 2 AICc units of best and assign each top model to separate object
bgden2_top <- subset(bgden2_set, delta <= 2) # 6 models
for (i in 1:nrow(bgden2_top)) {
  assign(paste0("bgden2_model", i), get.models(bgden2_top, subset = i)[[1]])
} 

# R^2 of top models
r2(bgden2_model1) # marginal: 0.398; conditional: can't compute
r2(bgden2_model2) # marginal: 0.388; conditional: 0.391
r2(bgden2_model3) # marginal: 0.375; conditional: 0.383
r2(bgden2_model4) # marginal: 0.381; conditional: 0.388
r2(bgden2_model5) # marginal: 0.396; conditional: can't compute
r2(bgden2_model6) # marginal: 0.367; conditional: 0.382

# Check singularity of top models
bgden2_top.models <- get.models(bgden2_set, subset = delta <= 2)
lapply(bgden2_top.models, isSingular) # models 1 & 5 have random effects issues

# Model averaging of top models
bgden2_avg <- model.avg(bgden2_set, subset = delta <= 2)
summary(bgden2_avg)

# Predicted vs. observed (best model)
bgden2_pred <- estimate_expectation(bgden2_best.model)
bgden2_pred$Change_BGDensity <- plot.change$Change_BGDensity

bgden2_pred.plot <- bgden2_pred %>% 
  ggplot(aes(x = Change_BGDensity, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  ggtitle("Plot density change model, predicted vs. observed") +
  xlab(expression(Delta ~ paste("Buffelgrass density (individuals / ", m^2, ") [observed]"))) +
  theme_bw()
bgden2_pred.plot


## BG density 3: (1 | Transect) -------------------------------------------

# Global model
bgden3 <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + 
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Transect),
               data = plot.change)

# Model selection
options(na.action = "na.fail")
bgden3_set <- dredge(bgden3) # no singularity issues

# Examine best model
bgden3_best.model <- get.models(bgden3_set, 1)[[1]]
summary(bgden3_best.model)
r2(bgden3_best.model) # marginal: 0.364; conditional: 0.456
res.bgden3_best.model <- simulateResiduals(bgden3_best.model)
plotQQunif(res.bgden3_best.model)
plotResiduals(res.bgden3_best.model) # mostly fine
check_model(bgden3_best.model) # posterior prediction is poor around 0-1

# Examine models within 2 AICc units of best and assign each top model to separate object
bgden3_top <- subset(bgden3_set, delta <= 2) # 5 models
for (i in 1:nrow(bgden3_top)) {
  assign(paste0("bgden3_model", i), get.models(bgden3_top, subset = i)[[1]])
} 

# R^2 of top models
r2(bgden3_model1) # marginal: 0.364; conditional: 0.456
r2(bgden3_model2) # marginal: 0.375; conditional: 0.440
r2(bgden3_model3) # marginal: 0.360; conditional: 0.458
r2(bgden3_model4) # marginal: 0.389; conditional: 0.423
r2(bgden3_model5) # marginal: 0.370; conditional: 0.443

# Model averaging of top models
bgden3_avg <- model.avg(bgden3_set, subset = delta <= 2)
summary(bgden3_avg)

# Predicted vs. observed (best model)
bgden3_pred <- estimate_expectation(bgden3_best.model)
bgden3_pred$Change_BGDensity <- plot.change$Change_BGDensity

bgden3_pred.plot <- bgden3_pred %>% 
  ggplot(aes(x = Change_BGDensity, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  ggtitle("Plot density change model, predicted vs. observed") +
  xlab(expression(Delta ~ paste("Buffelgrass density (individuals / ", m^2, ") [observed]"))) +
  theme_bw()
bgden3_pred.plot



# Buffelgrass cover change ------------------------------------------------

# Model diagnostics and predicted vs. actual plots look pretty similar for all 3 versions.
#   (Cover is only response variable where posterior predictive check looks good.)

# Singularity issues regarding random effects:
#   1. No top models, but 15 others.
#   2. No top models, but 7 others.
#   3. No top models, but 5 others.


## BG cover 1: (1 | Site / Transect) --------------------------------------

# Global model
bgcov1 <- lmer(Change_BGCover ~ Prev_year_precip_scaled + 
                Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                Change_HerbCover_scaled + 
                Prev_year_precip_scaled * Change_ShrubCover_scaled +
                Prev_year_precip_scaled * Change_HerbCover_scaled +
                (1 | Site / Transect),
              data = plot.change)

# Model selection
options(na.action = "na.fail")
bgcov1_set <- dredge(bgcov1) # some models had trouble with random effects; got warning: boundary (singular) fit: see help('isSingular')

# Examine best model
bgcov1_best.model <- get.models(bgcov1_set, 1)[[1]]
summary(bgcov1_best.model)
r2(bgcov1_best.model) # marginal: 0.248; conditional: 0.295
res.bgcov1_best.model <- simulateResiduals(bgcov1_best.model)
plotQQunif(res.bgcov1_best.model)
plotResiduals(res.bgcov1_best.model) # actually all lines are black (this has literally never happened before lol)
check_model(bgcov1_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
bgcov1_top <- subset(bgcov1_set, delta <= 2) # 6 models
for (i in 1:nrow(bgcov1_top)) {
  assign(paste0("bgcov1_model", i), get.models(bgcov1_top, subset = i)[[1]])
} 

# R^2 of top models
r2(bgcov1_model1) # marginal: 0.248; conditional: 0.295
r2(bgcov1_model2) # marginal: 0.249; conditional: 0.293
r2(bgcov1_model3) # marginal: 0.250; conditional: 0.298
r2(bgcov1_model4) # marginal: 0.251; conditional: 0.296
r2(bgcov1_model5) # marginal: 0.249; conditional: 0.294
r2(bgcov1_model6) # marginal: 0.250; conditional: 0.297

# Check singularity of top models
bgcov1_top.models <- get.models(bgcov1_set, subset = delta <= 2)
lapply(bgcov1_top.models, isSingular) # no top models have issues

# Model averaging of top models
bgcov1_avg <- model.avg(bgcov1_set, subset = delta <= 2)
summary(bgcov1_avg)

# Predicted vs. observed (best model)
bgcov1_pred <- estimate_expectation(bgcov1_best.model)
bgcov1_pred$Change_BGCover <- plot.change$Change_BGCover

bgcov1_pred.plot <- bgcov1_pred %>% 
  ggplot(aes(x = Change_BGCover, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  ggtitle("Plot cover change model, predicted vs. observed") +
  xlab(expression(Delta ~ "Buffelgrass cover (%) [observed]")) +
  theme_bw()
bgcov1_pred.plot


## BG cover 2: (1 | Site) -------------------------------------------------

# Global model
bgcov2 <- lmer(Change_BGCover ~ Prev_year_precip_scaled + 
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Site),
               data = plot.change)

# Model selection
options(na.action = "na.fail")
bgcov2_set <- dredge(bgcov2) # some models had trouble with random effects; got warning: boundary (singular) fit: see help('isSingular')

# Examine best model
bgcov2_best.model <- get.models(bgcov2_set, 1)[[1]]
summary(bgcov2_best.model)
r2(bgcov2_best.model) # marginal: 0.249; conditional: 0.279
res.bgcov2_best.model <- simulateResiduals(bgcov2_best.model)
plotQQunif(res.bgcov2_best.model)
plotResiduals(res.bgcov2_best.model) # all lines are black
check_model(bgcov2_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
bgcov2_top <- subset(bgcov2_set, delta <= 2) # 6 models
for (i in 1:nrow(bgcov2_top)) {
  assign(paste0("bgcov2_model", i), get.models(bgcov2_top, subset = i)[[1]])
} 

# R^2 of top models
r2(bgcov2_model1) # marginal: 0.249; conditional: 0.279
r2(bgcov2_model2) # marginal: 0.250; conditional: 0.277
r2(bgcov2_model3) # marginal: 0.251; conditional: 0.282
r2(bgcov2_model4) # marginal: 0.251; conditional: 0.279
r2(bgcov2_model5) # marginal: 0.250; conditional: 0.276
r2(bgcov2_model6) # marginal: 0.251; conditional: 0.279

# Check singularity of top models
bgcov2_top.models <- get.models(bgcov2_set, subset = delta <= 2)
lapply(bgcov2_top.models, isSingular) # no top models have issues

# Model averaging of top models
bgcov2_avg <- model.avg(bgcov2_set, subset = delta <= 2)
summary(bgcov2_avg)

# Predicted vs. observed (best model)
bgcov2_pred <- estimate_expectation(bgcov2_best.model)
bgcov2_pred$Change_BGCover <- plot.change$Change_BGCover

bgcov2_pred.plot <- bgcov2_pred %>% 
  ggplot(aes(x = Change_BGCover, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  ggtitle("Plot cover change model, predicted vs. observed") +
  xlab(expression(Delta ~ "Buffelgrass cover (%) [observed]")) +
  theme_bw()
bgcov2_pred.plot


## BG cover 3: (1 | Transect) ---------------------------------------------

# Global model
bgcov3 <- lmer(Change_BGCover ~ Prev_year_precip_scaled + 
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Transect),
               data = plot.change)

# Model selection
options(na.action = "na.fail")
bgcov3_set <- dredge(bgcov3) # some models had trouble with random effects; got warning: boundary (singular) fit: see help('isSingular')

# Examine best model
bgcov3_best.model <- get.models(bgcov3_set, 1)[[1]]
summary(bgcov3_best.model)
r2(bgcov3_best.model) # marginal: 0.243; conditional: 0.284
res.bgcov3_best.model <- simulateResiduals(bgcov3_best.model)
plotQQunif(res.bgcov3_best.model)
plotResiduals(res.bgcov3_best.model) # all lines are black
check_model(bgcov3_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
bgcov3_top <- subset(bgcov3_set, delta <= 2) # 6 models
for (i in 1:nrow(bgcov3_top)) {
  assign(paste0("bgcov3_model", i), get.models(bgcov3_top, subset = i)[[1]])
} 

# R^2 of top models
r2(bgcov3_model1) # marginal: 0.243; conditional: 0.284
r2(bgcov3_model2) # marginal: 0.245; conditional: 0.285
r2(bgcov3_model3) # marginal: 0.244; conditional: 0.287
r2(bgcov3_model4) # marginal: 0.246; conditional: 0.288
r2(bgcov3_model5) # marginal: 0.245; conditional: 0.286
r2(bgcov3_model6) # marginal: 0.246; conditional: 0.289

# Check singularity of top models
bgcov3_top.models <- get.models(bgcov3_set, subset = delta <= 2)
lapply(bgcov3_top.models, isSingular) # no top models have issues

# Model averaging of top models
bgcov3_avg <- model.avg(bgcov3_set, subset = delta <= 2)
summary(bgcov3_avg)

# Predicted vs. observed (best model)
bgcov3_pred <- estimate_expectation(bgcov3_best.model)
bgcov3_pred$Change_BGCover <- plot.change$Change_BGCover

bgcov3_pred.plot <- bgcov3_pred %>% 
  ggplot(aes(x = Change_BGCover, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  ggtitle("Plot cover change model, predicted vs. observed") +
  xlab(expression(Delta ~ "Buffelgrass cover (%) [observed]")) +
  theme_bw()
bgcov3_pred.plot



# Survival ----------------------------------------------------------------

# Model diagnostics and predicted vs. actual plots look pretty similar for all 3 versions.
#   (Cover is only one where post posterior check looks good.)

# Can't compute conditional R2 (indicates issues with random effects; singularity check doesn't
#   work for glmmTMB, I don't think):
#   1. Top models 1-4 & 6.
#   2. No issues with top models.
#   3. No issues with top models.


## Survival 1: (1 | Site / Transect) --------------------------------------

# Global model
survival1 <- glmmTMB(survival_transf ~ Prev_year_precip_scaled +
                      Aspect + PlotSlope_scaled + ShrubCover_scaled +
                      HerbCover_scaled + BGDensity_scaled +
                      Prev_year_precip_scaled * ShrubCover_scaled +
                      Prev_year_precip_scaled * HerbCover_scaled +
                      Prev_year_precip_scaled * BGDensity_scaled +
                      (1 | Site / Transect),
                    data = dat.survival,
                    family = beta_family(link = "logit"))

# Model selection
options(na.action = "na.fail")
survival1_set <- dredge(survival1) 

# Examine best model
survival1_best.model <- get.models(survival1_set, 1)[[1]]
summary(survival1_best.model)
r2(survival1_best.model) # marginal: 0.824; conditional: can't compute
res.survival1_best.model <- simulateResiduals(survival1_best.model)
plotQQunif(res.survival1_best.model)
plotResiduals(res.survival1_best.model) # looks pretty janky
check_model(survival1_best.model) # posterior prediction weird around 0

# Examine models within 2 AICc units of best and assign each top model to separate object
survival1_top <- subset(survival1_set, delta <= 2) # 8 models
for (i in 1:nrow(survival1_top)) {
  assign(paste0("survival1_model", i), get.models(survival1_top, subset = i)[[1]])
} 

# R^2 of top models
r2(survival1_model1) # marginal: 0.824; conditional: can't compute
r2(survival1_model2) # marginal: 0.817; conditional: can't compute
r2(survival1_model3) # marginal: 0.827; conditional: can't compute
r2(survival1_model4) # marginal: 0.818; conditional: can't compute
r2(survival1_model5) # marginal: 0.538; conditional: 0.888
r2(survival1_model6) # marginal: 0.823; conditional: can't compute
r2(survival1_model7) # marginal: 0.496; conditional: 0.899
r2(survival1_model8) # marginal: 0.496; conditional: 0.897

# Model averaging of top models
survival1_avg <- model.avg(survival1_set, subset = delta <= 2)
summary(survival1_avg)

# Predicted vs. observed (best model)
survival1_pred <- estimate_expectation(survival1_best.model)
survival1_pred$survival_transf <- dat.survival$survival_transf

survival1_pred.plot <- survival1_pred %>% 
  ggplot(aes(x = survival_transf, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  ggtitle("Seedling survival model, predicted vs. observed")  +
  xlab("Buffelgrass seedling survival (%) [observed]") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)
survival1_pred.plot


## Survival 2: (1 | Site) -------------------------------------------------

# Global model
survival2 <- glmmTMB(survival_transf ~ Prev_year_precip_scaled +
                       Aspect + PlotSlope_scaled + ShrubCover_scaled +
                       HerbCover_scaled + BGDensity_scaled +
                       Prev_year_precip_scaled * ShrubCover_scaled +
                       Prev_year_precip_scaled * HerbCover_scaled +
                       Prev_year_precip_scaled * BGDensity_scaled +
                       (1 | Site),
                     data = dat.survival,
                     family = beta_family(link = "logit"))

# Model selection
options(na.action = "na.fail")
survival2_set <- dredge(survival2) 

# Examine best model
survival2_best.model <- get.models(survival2_set, 1)[[1]]
summary(survival2_best.model)
r2(survival2_best.model) # marginal: 0.592; conditional: 0.874
res.survival2_best.model <- simulateResiduals(survival2_best.model)
plotQQunif(res.survival2_best.model)
plotResiduals(res.survival2_best.model) # looks pretty janky
check_model(survival2_best.model) # posterior prediction weird around 0

# Examine models within 2 AICc units of best and assign each top model to separate object
survival2_top <- subset(survival2_set, delta <= 2) # 7 models
for (i in 1:nrow(survival2_top)) {
  assign(paste0("survival2_model", i), get.models(survival2_top, subset = i)[[1]])
} 

# R^2 of top models
r2(survival2_model1) # marginal: 0.592; conditional: 0.874
r2(survival2_model2) # marginal: 0.611; conditional: 0.863
r2(survival2_model3) # marginal: 0.535; conditional: 0.888
r2(survival2_model4) # marginal: 0.605; conditional: 0.866
r2(survival2_model5) # marginal: 0.538; conditional: 0.888
r2(survival2_model6) # marginal: 0.589; conditional: 0.874
r2(survival2_model7) # marginal: 0.497; conditional: 0.899

# Model averaging of top models
survival2_avg <- model.avg(survival2_set, subset = delta <= 2)
summary(survival2_avg)

# Predicted vs. observed (best model)
survival2_pred <- estimate_expectation(survival2_best.model)
survival2_pred$survival_transf <- dat.survival$survival_transf

survival2_pred.plot <- survival2_pred %>% 
  ggplot(aes(x = survival_transf, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  ggtitle("Seedling survival model, predicted vs. observed")  +
  xlab("Buffelgrass seedling survival (%) [observed]") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)
survival2_pred.plot


## Survival 3: (1 | Transect) ---------------------------------------------

# Global model
survival3 <- glmmTMB(survival_transf ~ Prev_year_precip_scaled +
                       Aspect + PlotSlope_scaled + ShrubCover_scaled +
                       HerbCover_scaled + BGDensity_scaled +
                       Prev_year_precip_scaled * ShrubCover_scaled +
                       Prev_year_precip_scaled * HerbCover_scaled +
                       Prev_year_precip_scaled * BGDensity_scaled +
                       (1 | Transect),
                     data = dat.survival,
                     family = beta_family(link = "logit"))

# Model selection
options(na.action = "na.fail")
survival3_set <- dredge(survival3) 

# Examine best model
survival3_best.model <- get.models(survival3_set, 1)[[1]]
summary(survival3_best.model)
r2(survival3_best.model) # marginal: 0.688; conditional: 0.793
res.survival3_best.model <- simulateResiduals(survival3_best.model)
plotQQunif(res.survival3_best.model)
plotResiduals(res.survival3_best.model) # also kind of janky
check_model(survival3_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
survival3_top <- subset(survival3_set, delta <= 2) # 15 models
for (i in 1:nrow(survival3_top)) {
  assign(paste0("survival3_model", i), get.models(survival3_top, subset = i)[[1]])
} 

# R^2 of top models
r2(survival3_model1) # marginal: 0.688; conditional: 0.793
r2(survival3_model2) # marginal: 0.683; conditional: 0.788
r2(survival3_model3) # marginal: 0.677; conditional: 0.793
r2(survival3_model4) # marginal: 0.666; conditional: 0.811
r2(survival3_model5) # marginal: 0.690; conditional: 0.781
r2(survival3_model6) # marginal: 0.683; conditional: 0.798
r2(survival3_model7) # marginal: 0.675; conditional: 0.798
r2(survival3_model8) # marginal: 0.692; conditional: 0.779
r2(survival3_model9) # marginal: 0.676; conditional: 0.790
r2(survival3_model10) # marginal: 0.688; conditional: 0.788
r2(survival3_model11) # marginal: 0.695; conditional: 0.785
r2(survival3_model12) # marginal: 0.693; conditional: 0.786
r2(survival3_model13) # marginal: 0.684; conditional: 0.789
r2(survival3_model14) # marginal: 0.674; conditional: 0.802
r2(survival3_model15) # marginal: 0.691; conditional: 0.790

# Model averaging of top models
survival3_avg <- model.avg(survival3_set, subset = delta <= 2)
summary(survival3_avg)

# Predicted vs. observed (best model)
survival3_pred <- estimate_expectation(survival3_best.model)
survival3_pred$survival_transf <- dat.survival$survival_transf

survival3_pred.plot <- survival3_pred %>% 
  ggplot(aes(x = survival_transf, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  ggtitle("Seedling survival model, predicted vs. observed")  +
  xlab("Buffelgrass seedling survival (%) [observed]") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)
survival3_pred.plot




save.image("RData/06.7_linear-models-6.0.RData")
