# Created: 2025-07-09
# Updated: 2025-07-10

# Purpose: Run linear models with Change_Reproductive_culms, Change_Total_Live_Culms, 
#   Change_BGDensity, and Change_BGCover as response variable.

# Continuous explanatory variables are centered and scaled. 

# Updates from 06.2_linear-models-3.0.R script:
#   Mostly this is to restart model numbering now that I have explanatory variables confirmed,
#     and am looking at random effects and glmmTMB (b) vs. lme4 (a).
#   Considered (1 | Site) versus nested (1 | Site / Transect) for culm count models.
#   Also no longer modeling survival as linear model, instead using Tweedie GLM.

# Updates from 06.3_linear-models-4.0.R script:
#   No longer including Aspect*PlotSlope and Precip*Slope interactions in culm count models,
#     and no longer including Precip * PlotSlope interactions in any model to simplify things.

# Density, cover, and survival models did not change that much because they never included Aspect*PlotSlope
#   and did not include Precip*PlotSlope after model selection.
# Total & repro culm count models had Precip*PlotSlope as significant previously, but I think this adds too much
#   complication to the model and isn't really what we are interested in, so that is why it is dropped here.

# Am going with random effect of (1 | Site) to simplify things, given the limited number of sites and
#   plot-level observations.


library(tidyverse)
library(glmmTMB)
library(performance)
library(DHARMa)
library(performance)
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



# Total culms -------------------------------------------------------------

# Total change 1: (1 | Site / Transect) -----------------------------------

# 1: lme4 version
total1a <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                  Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  (1 | Site / Transect),
                data = culm.change.flat.rm)
summary(total1a)
r2(total1a) # marginal: 0.119; conditional: 0.410
check_model(total1a)

# 1: glmmTMB version
total1b <- glmmTMB(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                     Change_BGDensity_scaled +
                     Prev_year_precip_scaled * Change_BGDensity_scaled +
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     (1 | Site / Transect),
                   data = culm.change.flat.rm,
                   family = gaussian) 
summary(total1b)
r2(total1b) # marginal: 0.132; conditional: 0.359
res.total1b <- simulateResiduals(total1b)
plotQQunif(res.total1b)
plotResiduals(res.total1b)
check_model(total1b)


### 1: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
total1a_set <- dredge(total1a) 
total1b_set <- dredge(total1b)

# Examine best model
total1a_best.model <- get.models(total1a_set, 1)[[1]]
summary(total1a_best.model)
r2(total1a_best.model) # marginal: 0.119; conditional: 0.410
check_model(total1a_best.model) # posterior prediction is poor around 0

total1b_best.model <- get.models(total1b_set, 1)[[1]]
summary(total1b_best.model)
r2(total1b_best.model) # marginal: 0.119; conditional: 0.361
res.total1b_best.model <- simulateResiduals(total1b_best.model)
plotQQunif(res.total1b_best.model)
plotResiduals(res.total1b_best.model)
check_model(total1b_best.model) # posterior prediction is poor around 0


# Examine models within 2 AICc units of best and assign each top model to separate object
total1a_top <- subset(total1a_set, delta <= 2) # 1 model

total1b_top <- subset(total1b_set, delta <= 2) # 8 models
for (i in 1:nrow(total1b_top)) {
  assign(paste0("total1b_model", i), get.models(total1b_top, subset = i)[[1]])
} 


# R^2 of top models
r2(total1a_best.model) # marginal: 0.119; conditional: 0.410

r2(total1b_model1) # marginal: 0.119; conditional: 0.361
r2(total1b_model2) # marginal: 0.132; conditional: 0.359
r2(total1b_model3) # marginal: 0.122; conditional: 0.352
r2(total1b_model4) # marginal: 0.136; conditional: 0.353
r2(total1b_model5) # marginal: 0.115; conditional: 0.365
r2(total1b_model6) # marginal: 0.135; conditional: 0.351
r2(total1b_model7) # marginal: 0.118; conditional: 0.356
r2(total1b_model8) # marginal: 0.140; conditional: 0.345


# Model averaging of top models
#   model averaging not needed for lme4 version; only 1 top model
summary(total1a_best.model)

total1b_avg <- model.avg(total1b_set, subset = delta <= 2)
summary(total1b_avg)




## Total change 2: (1 | Site) ---------------------------------------------

# 2: lme4 version
total2a <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                  Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  (1 | Site),
                data = culm.change.flat.rm)
summary(total2a)
r2(total2a) # marginal: 0.137; conditional: 0.363
check_model(total2a)

# 2: glmmTMB version
total2b <- glmmTMB(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                     Change_BGDensity_scaled +
                     Prev_year_precip_scaled * Change_BGDensity_scaled +
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     (1 | Site),
                   data = culm.change.flat.rm,
                   family = gaussian) 
summary(total2b)
r2(total2b) # marginal: 0.147; conditional: 0.325
res.total2b <- simulateResiduals(total2b)
plotQQunif(res.total2b)
plotResiduals(res.total2b)
check_model(total2b)


### 2: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
total2a_set <- dredge(total2a) 
total2b_set <- dredge(total2b)

# Examine best model
total2a_best.model <- get.models(total2a_set, 1)[[1]]
summary(total2a_best.model)
r2(total2a_best.model) # marginal: 0.137; conditional: 0.363
check_model(total2a_best.model) # posterior prediction poor at 0

total2b_best.model <- get.models(total2b_set, 1)[[1]]
summary(total2b_best.model)
r2(total2b_best.model) # marginal: 0.152; conditional: 0.320
res.total2b_best.model <- simulateResiduals(total2b_best.model)
plotQQunif(res.total2b_best.model)
plotResiduals(res.total2b_best.model)
check_model(total2b_best.model) # posterior prediction poor at 0


# Examine models within 2 AICc units of best and assign each top model to separate object
total2a_top <- subset(total2a_set, delta <= 2) # 3 models
for (i in 1:nrow(total2a_top)) {
  assign(paste0("total2a_model", i), get.models(total2a_top, subset = i)[[1]])
} 

total2b_top <- subset(total2b_set, delta <= 2) # 3 models
for (i in 1:nrow(total2b_top)) {
  assign(paste0("total2b_model", i), get.models(total2b_top, subset = i)[[1]])
} 


# R^2 of top models
r2(total2a_model1) # marginal: 0.137; conditional: 0.363
r2(total2a_model2) # marginal: 0.138; conditional: 0.363
r2(total2a_model3) # marginal: 0.142; conditional: 0.355

r2(total2b_model1) # marginal: 0.152; conditional: 0.320
r2(total2b_model2) # marginal: 0.147; conditional: 0.325
r2(total2b_model3) # marginal: 0.156; conditional: 0.306


# Model averaging of top models
total2a_avg <- model.avg(total2a_set, subset = delta <= 2)
summary(total2a_avg)

total2b_avg <- model.avg(total2b_set, subset = delta <= 2)
summary(total2b_avg)




# Reproductive culms ------------------------------------------------------

## Repro change 1: (1 | Site / Transect) ----------------------------------

# 1: lme4 version
repro1a <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                  Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  (1 | Site / Transect),
                data = culm.change.flat.rm)
summary(repro1a)
r2(repro1a) # marginal: 0.098; conditional: 0.295
check_model(repro1a)

# 1: glmmTMB version
repro1b <- glmmTMB(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                     Change_BGDensity_scaled +
                     Prev_year_precip_scaled * Change_BGDensity_scaled +
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     (1 | Site / Transect),
                   data = culm.change.flat.rm,
                   family = gaussian) 
summary(repro1b)
r2(repro1b) # marginal: 0.107; conditional: 0.252
res.repro1b <- simulateResiduals(repro1b)
plotQQunif(res.repro1b)
plotResiduals(res.repro1b)
check_model(repro1b)


### 1: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
repro1a_set <- dredge(repro1a) # got warning about convergence, but table looks complete 
repro1b_set <- dredge(repro1b)

# Examine best model
repro1a_best.model <- get.models(repro1a_set, 1)[[1]]
summary(repro1a_best.model)
r2(repro1a_best.model) # marginal: 0.094; conditional: 0.299
check_model(repro1a_best.model) # posterior prediction is poor at 0

repro1b_best.model <- get.models(repro1b_set, 1)[[1]]
summary(repro1b_best.model)
r2(repro1b_best.model) # marginal: 0.100; conditional: 0.258
res.repro1b_best.model <- simulateResiduals(repro1b_best.model)
plotQQunif(res.repro1b_best.model)
plotResiduals(res.repro1b_best.model)
check_model(repro1b_best.model) # posterior prediction is poor at 0


# Examine models within 2 AICc units of best and assign each top model to separate object
repro1a_top <- subset(repro1a_set, delta <= 2) # 8 models
for (i in 1:nrow(repro1a_top)) {
  assign(paste0("repro1a_model", i), get.models(repro1a_top, subset = i)[[1]])
} 

repro1b_top <- subset(repro1b_set, delta <= 2) # 6 models
for (i in 1:nrow(repro1b_top)) {
  assign(paste0("repro1b_model", i), get.models(repro1b_top, subset = i)[[1]])
} 


# R^2 of top models
r2(repro1a_model1) # marginal: 0.094; conditional: 0.299
r2(repro1a_model2) # marginal: 0.095; conditional: 0.301
r2(repro1a_model3) # marginal: 0.096; conditional: 0.298
r2(repro1a_model4) # marginal: 0.092; conditional: 0.302
r2(repro1a_model5) # marginal: 0.095; conditional: 0.296
r2(repro1a_model6) # marginal: 0.098; conditional: 0.295
r2(repro1a_model7) # marginal: 0.097; conditional: 0.298
r2(repro1a_model8) # marginal: 0.094; conditional: 0.300

r2(repro1b_model1) # marginal: 0.100; conditional: 0.258
r2(repro1b_model2) # marginal: 0.102; conditional: 0.256
r2(repro1b_model3) # marginal: 0.089; conditional: 0.279
r2(repro1b_model4) # marginal: 0.102; conditional: 0.256
r2(repro1b_model5) # marginal: 0.103; conditional: 0.256
r2(repro1b_model6) # marginal: 0.104; conditional: 0.253


# Model averaging of top models
repro1a_avg <- model.avg(repro1a_set, subset = delta <= 2)
summary(repro1a_avg)

repro1b_avg <- model.avg(repro1b_set, subset = delta <= 2)
summary(repro1b_avg)




## Repro change 2: (1 | Site) ---------------------------------------------

# 2: lme4 version
repro2a <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                  Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  (1 | Site),
                data = culm.change.flat.rm)
summary(repro2a)
r2(repro2a) # marginal: 0.109; conditional: 0.248
check_model(repro2a)

# 2: glmmTMB version
repro2b <- glmmTMB(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                     Change_BGDensity_scaled +
                     Prev_year_precip_scaled * Change_BGDensity_scaled +
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     (1 | Site),
                   data = culm.change.flat.rm,
                   family = gaussian) 
summary(repro2b)
r2(repro2b) # marginal: 0.115; conditional: 0.221
res.repro2b <- simulateResiduals(repro2b)
plotQQunif(res.repro2b)
plotResiduals(res.repro2b)
check_model(repro2b)


### 2: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
repro2a_set <- dredge(repro2a) 
repro2b_set <- dredge(repro2b)

# Examine best model
repro2a_best.model <- get.models(repro2a_set, 1)[[1]]
summary(repro2a_best.model)
r2(repro2a_best.model) # marginal: 0.109; conditional: 0.249
check_model(repro2a_best.model) # posterior prediction is poor at 0

repro2b_best.model <- get.models(repro2b_set, 1)[[1]]
summary(repro2b_best.model)
r2(repro2b_best.model) # marginal: 0.111; conditional: 0.222
res.repro2b_best.model <- simulateResiduals(repro2b_best.model)
plotQQunif(res.repro2b_best.model)
plotResiduals(res.repro2b_best.model)
check_model(repro2b_best.model) # posterior prediction is poor at 0


# Examine models within 2 AICc units of best and assign each top model to separate object
repro2a_top <- subset(repro2a_set, delta <= 2) # 4 models
for (i in 1:nrow(repro2a_top)) {
  assign(paste0("repro2a_model", i), get.models(repro2a_top, subset = i)[[1]])
} 

repro2b_top <- subset(repro2b_set, delta <= 2) # 4 models
for (i in 1:nrow(repro2b_top)) {
  assign(paste0("repro2b_model", i), get.models(repro2b_top, subset = i)[[1]])
} 


# R^2 of top models
r2(repro2a_model1) # marginal: 0.109; conditional: 0.249
r2(repro2a_model2) # marginal: 0.105; conditional: 0.250
r2(repro2a_model3) # marginal: 0.109; conditional: 0.248
r2(repro2a_model4) # marginal: 0.105; conditional: 0.251

r2(repro2b_model1) # marginal: 0.111; conditional: 0.222
r2(repro2b_model2) # marginal: 0.114; conditional: 0.221
r2(repro2b_model3) # marginal: 0.109; conditional: 0.222
r2(repro2b_model4) # marginal: 0.112; conditional: 0.221


# Model averaging of top models
repro2a_avg <- model.avg(repro2a_set, subset = delta <= 2)
summary(repro2a_avg)

repro2b_avg <- model.avg(repro2b_set, subset = delta <= 2)
summary(repro2b_avg)




# Buffelgrass density -----------------------------------------------------

## BG density 1: Add (1 | Site / Transect) --------------------------------

# 1: lme4 version
bgden1a <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + 
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                  Change_HerbCover_scaled + 
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  (1 | Site / Transect),
                data = plot.change)
summary(bgden1a)
r2(bgden1a) # marginal: 0.299; conditional: 0.566
check_model(bgden1a)

# 1: glmmTMB version
bgden1b <- glmmTMB(Change_BGDensity ~ Prev_year_precip_scaled +
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                     Change_HerbCover_scaled + 
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     (1 | Site / Transect),
                   data = plot.change,
                   family = gaussian)
summary(bgden1b)
r2(bgden1b) # marginal: 0.340; conditional: 0.531
res.bgden1b <- simulateResiduals(bgden1b)
plotQQunif(res.bgden1b)
plotResiduals(res.bgden1b)
check_model(bgden1b)


### 1: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgden1a_set <- dredge(bgden1a) # got warning about convergence, but table looks complete (Model failed to converge with 1 negative eigenvalue: -8.2e-01)
bgden1b_set <- dredge(bgden1b)


# Examine best model
bgden1a_best.model <- get.models(bgden1a_set, 1)[[1]]
summary(bgden1a_best.model)
r2(bgden1a_best.model) # marginal: 0.305; conditional: 0.562
check_model(bgden1a_best.model) # posterior prediction is poor around 0-1

bgden1b_best.model <- get.models(bgden1b_set, 1)[[1]]
summary(bgden1b_best.model)
r2(bgden1b_best.model) # marginal: 0.339; conditional: 0.532
res.bgden1b_best.model <- simulateResiduals(bgden1b_best.model)
plotQQunif(res.bgden1b_best.model)
plotResiduals(res.bgden1b_best.model)
check_model(bgden1b_best.model) # posterior prediction is poor around 0-1


# Examine models within 2 AICc units of best and assign each top model to separate object
bgden1a_top <- subset(bgden1a_set, delta <= 2) # 2 models
for (i in 1:nrow(bgden1a_top)) {
  assign(paste0("bgden1a_model", i), get.models(bgden1a_top, subset = i)[[1]])
} 

bgden1b_top <- subset(bgden1b_set, delta <= 2) # 2 models
for (i in 1:nrow(bgden1b_top)) {
  assign(paste0("bgden1b_model", i), get.models(bgden1b_top, subset = i)[[1]])
} 


# R^2 of top models
r2(bgden1a_model1) # marginal: 0.305; conditional: 0.562
r2(bgden1a_model2) # marginal: 0.301; conditional: 0.567

r2(bgden1b_model1) # marginal: 0.339; conditional: 0.532
r2(bgden1b_model2) # marginal: 0.336; conditional: 0.535


# Model averaging of top models
bgden1a_avg <- model.avg(bgden1a_set, subset = delta <= 2)
summary(bgden1a_avg)

bgden1b_avg <- model.avg(bgden1b_set, subset = delta <= 2) 
summary(bgden1b_avg)
confint(bgden1b_avg)




# BG density 2: Add (1 | Site) --------------------------------------------

# 2: lme4 version
bgden2a <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + 
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                  Change_HerbCover_scaled + 
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  (1 | Site),
                data = plot.change)
summary(bgden2a)
r2(bgden2a) # marginal: 0.392; conditional: 0.431
check_model(bgden2a)

# 2: glmmTMB version
bgden2b <- glmmTMB(Change_BGDensity ~ Prev_year_precip_scaled +
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                     Change_HerbCover_scaled + 
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     (1 | Site),
                   data = plot.change,
                   family = gaussian)
summary(bgden2b)
r2(bgden2b) # marginal: 0.421; conditional: 0.436
res.bgden2b <- simulateResiduals(bgden2b)
plotQQunif(res.bgden2b)
plotResiduals(res.bgden2b)
check_model(bgden2b)


### 2: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgden2a_set <- dredge(bgden2a) 
bgden2b_set <- dredge(bgden2b)


# Examine best model
bgden2a_best.model <- get.models(bgden2a_set, 1)[[1]]
summary(bgden2a_best.model)
r2(bgden2a_best.model) # marginal: 0.390; conditional: 0.427
check_model(bgden2a_best.model) # posterior prediction is poor around 0-1

bgden2b_best.model <- get.models(bgden2b_set, 1)[[1]]
summary(bgden2b_best.model)
r2(bgden2b_best.model) # marginal: 0.428; conditional: 0.438
res.bgden2b_best.model <- simulateResiduals(bgden2b_best.model)
plotQQunif(res.bgden2b_best.model)
plotResiduals(res.bgden2b_best.model)
check_model(bgden2b_best.model) # posterior prediction is poor around 0-1


# Examine models within 2 AICc units of best and assign each top model to separate object
bgden2a_top <- subset(bgden2a_set, delta <= 2) # 6 models
for (i in 1:nrow(bgden2a_top)) {
  assign(paste0("bgden2a_model", i), get.models(bgden2a_top, subset = i)[[1]])
} 

bgden2b_top <- subset(bgden2b_set, delta <= 2) # 3 models
for (i in 1:nrow(bgden2b_top)) {
  assign(paste0("bgden2b_model", i), get.models(bgden2b_top, subset = i)[[1]])
} 


# R^2 of top models
r2(bgden2a_model1) # marginal: 0.390; conditional: 0.427
r2(bgden2a_model2) # marginal: 0.402; conditional: 0.431
r2(bgden2a_model3) # marginal: 0.378; conditional: 0.424
r2(bgden2a_model4) # marginal: 0.380; conditional: 0.429
r2(bgden2a_model5) # marginal: 0.392; conditional: 0.431
r2(bgden2a_model6) # marginal: 0.368; conditional: 0.428

r2(bgden2b_model1) # marginal: 0.428; conditional: 0.438
r2(bgden2b_model2) # marginal: 0.411; conditional: 0.429


# Model averaging of top models
bgden2a_avg <- model.avg(bgden2a_set, subset = delta <= 2)
summary(bgden2a_avg)

bgden2b_avg <- model.avg(bgden2b_set, subset = delta <= 2) 
summary(bgden2b_avg)




# Buffelgrass cover -------------------------------------------------------

## BG cover 1: Add (1 | Site / Transect) ----------------------------------

# 1: lme4 version
bgcov1a <- lmer(Change_BGCover ~ Prev_year_precip_scaled + 
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                  Change_HerbCover_scaled + 
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  (1 | Site / Transect),
                data = plot.change)
summary(bgcov1a)
r2(bgcov1a) # marginal: 0.239; conditional: 0.393
check_model(bgcov1a)

# 1: glmmTMB version
bgcov1b <- glmmTMB(Change_BGCover ~ Prev_year_precip_scaled +
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                     Change_HerbCover_scaled + 
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     (1 | Site / Transect),
                   data = plot.change,
                   family = gaussian)
summary(bgcov1b)
r2(bgcov1b) # marginal: 0.283; conditional: NA
res.bgcov1b <- simulateResiduals(bgcov1b)
plotQQunif(res.bgcov1b)
plotResiduals(res.bgcov1b)
check_model(bgcov1b)


### 1: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgcov1a_set <- dredge(bgcov1a) 
bgcov1b_set <- dredge(bgcov1b)


# Examine best model
bgcov1a_best.model <- get.models(bgcov1a_set, 1)[[1]]
summary(bgcov1a_best.model)
r2(bgcov1a_best.model) # marginal: 0.240; conditional: 0.393
check_model(bgcov1a_best.model)

bgcov1b_best.model <- get.models(bgcov1b_set, 1)[[1]]
summary(bgcov1b_best.model)
r2(bgcov1b_best.model) # marginal: 0.254; conditional: 0.355
res.bgcov1b_best.model <- simulateResiduals(bgcov1b_best.model)
plotQQunif(res.bgcov1b_best.model)
plotResiduals(res.bgcov1b_best.model)
check_model(bgcov1b_best.model)


# Examine models within 2 AICc units of best and assign each top model to separate object
bgcov1a_top <- subset(bgcov1a_set, delta <= 2) # 3 models
for (i in 1:nrow(bgcov1a_top)) {
  assign(paste0("bgcov1a_model", i), get.models(bgcov1a_top, subset = i)[[1]])
} 

bgcov1b_top <- subset(bgcov1b_set, delta <= 2) # 9 models
for (i in 1:nrow(bgcov1b_top)) {
  assign(paste0("bgcov1b_model", i), get.models(bgcov1b_top, subset = i)[[1]])
} 


# R^2 of top models
r2(bgcov1a_model1) # marginal: 0.240; conditional: 0.393
r2(bgcov1a_model2) # marginal: 0.239; conditional: 0.393
r2(bgcov1a_model3) # marginal: 0.240; conditional: 0.391

r2(bgcov1b_model1) # marginal: 0.254; conditional: 0.355
r2(bgcov1b_model2) # marginal: 0.247; conditional: 0.369
r2(bgcov1b_model3) # marginal: 0.196; conditional: 0.394
r2(bgcov1b_model4) # marginal: 0.234; conditional: 0.353
r2(bgcov1b_model5) # marginal: 0.269; conditional: NA
r2(bgcov1b_model6) # marginal: 0.187; conditional: 0.384
r2(bgcov1b_model7) # marginal: 0.194; conditional: 0.378
r2(bgcov1b_model8) # marginal: 0.255; conditional: 0.356
r2(bgcov1b_model9) # marginal: 0.248; conditional: 0.371


# Model averaging of top models
bgcov1a_avg <- model.avg(bgcov1a_set, subset = delta <= 2)
summary(bgcov1a_avg)

bgcov1b_avg <- model.avg(bgcov1b_set, subset = delta <= 2) 
summary(bgcov1b_avg)




# BG cover 2: Add (1 | Site) ----------------------------------------------

# 2: lme4 version
bgcov2a <- lmer(Change_BGCover ~ Prev_year_precip_scaled + 
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                  Change_HerbCover_scaled + 
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  (1 | Site),
                data = plot.change)
summary(bgcov2a)
r2(bgcov2a) # marginal: 0.251; conditional: 0.306
check_model(bgcov2a)

# 2: glmmTMB version
bgcov2b <- glmmTMB(Change_BGCover ~ Prev_year_precip_scaled +
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                     Change_HerbCover_scaled + 
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     (1 | Site),
                   data = plot.change,
                   family = gaussian) 
summary(bgcov2b)
r2(bgcov2b) # marginal: 0.260; conditional: 0.289
res.bgcov2b <- simulateResiduals(bgcov2b)
plotQQunif(res.bgcov2b)
plotResiduals(res.bgcov2b)
check_model(bgcov2b)


### 2: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgcov2a_set <- dredge(bgcov2a) 
bgcov2b_set <- dredge(bgcov2b) # convergence problems


# Examine best model
bgcov2a_best.model <- get.models(bgcov2a_set, 1)[[1]]
summary(bgcov2a_best.model)
r2(bgcov2a_best.model) # marginal: 0.252; conditional: 0.310
check_model(bgcov2a_best.model)

bgcov2b_best.model <- get.models(bgcov2b_set, 1)[[1]]
summary(bgcov2b_best.model)
r2(bgcov2b_best.model) # marginal: 0.263; conditional: 0.310
res.bgcov2b_best.model <- simulateResiduals(bgcov2b_best.model)
plotQQunif(res.bgcov2b_best.model)
plotResiduals(res.bgcov2b_best.model)
check_model(bgcov2b_best.model)


# Examine models within 2 AICc units of best and assign each top model to separate object
bgcov2a_top <- subset(bgcov2a_set, delta <= 2) # 3 models
for (i in 1:nrow(bgcov2a_top)) {
  assign(paste0("bgcov2a_model", i), get.models(bgcov2a_top, subset = i)[[1]])
} 

bgcov2b_top <- subset(bgcov2b_set, delta <= 2) %>% 
  filter(!is.na(df)) # 6 models; not all models converged
for (i in 1:nrow(bgcov2b_top)) {
  assign(paste0("bgcov2b_model", i), get.models(bgcov2b_top, subset = i)[[1]])
}


# R^2 of top models
r2(bgcov2a_model1) # marginal: 0.252; conditional: 0.310
r2(bgcov2a_model2) # marginal: 0.251; conditional: 0.306
r2(bgcov2a_model3) # marginal: 0.251; conditional: 0.306

r2(bgcov2b_model1) # marginal: 0.263; conditional: 0.310
r2(bgcov2b_model2) # marginal: 0.247; conditional: 0.281
r2(bgcov2b_model3) # marginal: 0.240; conditional: 0.259
r2(bgcov2b_model4) # marginal: 0.265; conditional: 0.314
r2(bgcov2b_model5) # marginal: 0.248; conditional: 0.284
r2(bgcov2b_model6) # marginal: 0.243; conditional: 0.264


# Model averaging of top models
bgcov2a_avg <- model.avg(bgcov2a_set, subset = delta <= 2)
summary(bgcov2a_avg)

bgcov2b_set_with.delta <- bgcov2b_set %>% 
  filter(!is.na(delta))
bgcov2b_avg <- model.avg(bgcov2b_set_with.delta, subset = delta <= 2)
summary(bgcov2b_avg)





# Survival ----------------------------------------------------------------

## Survival 1: Tweedie GLM, add (1 | Site / Transect) ---------------------

# 1: glmmTMB version
survival1 <- glmmTMB(survival_perc ~ Prev_year_precip_scaled +
                       Aspect + PlotSlope_scaled + ShrubCover_scaled +
                       HerbCover_scaled + BGDensity_scaled +
                       Prev_year_precip_scaled * ShrubCover_scaled +
                       Prev_year_precip_scaled * HerbCover_scaled +
                       Prev_year_precip_scaled * BGDensity_scaled +
                       (1 | Site / Transect),
                     data = dat.survival,
                     family = tweedie(link = "log"))
summary(survival1)
r2(survival1) # can't compute
res.survival1 <- simulateResiduals(survival1)
plotQQunif(res.survival1)
plotResiduals(res.survival1) # looks kind of janky
check_collinearity(survival1) 
check_model(survival1) # posterior prediction looks weird; homogeneity of variance not shown
check_overdispersion(survival1)


### 1: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
survival1_set <- dredge(survival1) # not all converged

# Examine best model
survival1_best.model <- get.models(survival1_set, 1)[[1]]
summary(survival1_best.model)
r2(survival1_best.model) # marginal: 0.362; conditional: 0.908
res.survival1_best.model <- simulateResiduals(survival1_best.model)
plotQQunif(res.survival1_best.model)
plotResiduals(res.survival1_best.model) # really does not look great
check_model(survival1_best.model) # posterior prediction looks weird

# Examine models within 2 AICc units of best and assign each top model to separate object
survival1_top <- subset(survival1_set, delta <= 2) %>% 
  filter(!is.na(df)) # not all models converged; 17 top models
for (i in 1:nrow(survival1_top)) {
  assign(paste0("survival1_model", i), get.models(survival1_top, subset = i)[[1]])
} 

# R^2 of top models
r2(survival1_model1) # marginal: 0.362; conditional: 0.908
r2(survival1_model2) # marginal: 0.349; conditional: 0.915
r2(survival1_model3) # marginal: 0.358; conditional: 0.905
r2(survival1_model4) # can't compute
r2(survival1_model5) # marginal: 0.329; conditional: 0.919
r2(survival1_model6) # marginal: 0.341; conditional: 0.917
r2(survival1_model7) # marginal: 0.337; conditional: 0.914
r2(survival1_model8) # marginal: 0.345; conditional: 0.912
r2(survival1_model9) # marginal: 0.355; conditional: 0.908
r2(survival1_model10) # marginal: 0.350; conditional: 0.906
r2(survival1_model11) # marginal: 0.341; conditional: 0.913
r2(survival1_model12) # can't compute
r2(survival1_model13) # marginal: 0.353; conditional: 0.911
r2(survival1_model14) # can't compute
r2(survival1_model15) # marginal: 0.342; conditional: 0.917
r2(survival1_model16) # marginal: 0.353; conditional: 0.911
r2(survival1_model17) # marginal: 0.330; conditional: 0.920


# Model averaging of top models
survival1_set_with.delta <- survival1_set %>% 
  filter(!is.na(delta))
survival1_avg <- model.avg(survival1_set_with.delta, subset = delta <= 2)
summary(survival1_avg)



## Survival 2: Tweedie GLM, add (1 | Site) --------------------------------

# 2: glmmTMB version
survival2 <- glmmTMB(survival_perc ~ Prev_year_precip_scaled +
                       Aspect + PlotSlope_scaled + ShrubCover_scaled +
                       HerbCover_scaled + BGDensity_scaled +
                       Prev_year_precip_scaled * ShrubCover_scaled +
                       Prev_year_precip_scaled * HerbCover_scaled +
                       Prev_year_precip_scaled * BGDensity_scaled +
                       (1 | Site),
                     data = dat.survival,
                     family = tweedie(link = "log"))
summary(survival2)
r2(survival2) # marginal: 0.333; conditional: 0.919
res.survival2 <- simulateResiduals(survival2)
plotQQunif(res.survival2)
plotResiduals(res.survival2) # looks a bit funky, I think due to uneven sample size
check_collinearity(survival2) 
check_model(survival2)
check_overdispersion(survival2)


### 2: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
survival2_set <- dredge(survival2)

# Examine best model
survival2_best.model <- get.models(survival2_set, 1)[[1]]
summary(survival2_best.model)
r2(survival2_best.model) # marginal: 0.340; conditional: 0.913
res.survival2_best.model <- simulateResiduals(survival2_best.model)
plotQQunif(res.survival2_best.model)
plotResiduals(res.survival2_best.model) # kind of janky
check_model(survival2_best.model) # posterior prediction looks weird

# Examine models within 2 AICc units of best and assign each top model to separate object
survival2_top <- subset(survival2_set, delta <= 2) # 11 models
for (i in 1:nrow(survival2_top)) {
  assign(paste0("survival2_model", i), get.models(survival2_top, subset = i)[[1]])
} 

# R^2 of top models
r2(survival2_model1) # marginal: 0.340; conditional: 0.913
r2(survival2_model2) # marginal: 0.329; conditional: 0.918
r2(survival2_model3) # marginal: 0.341; conditional: 0.913
r2(survival2_model4) # marginal: 0.353; conditional: 0.910
r2(survival2_model5) # marginal: 0.358; conditional: 0.909
r2(survival2_model6) # marginal: 0.342; conditional: 0.916
r2(survival2_model7) # marginal: 0.331; conditional: 0.918
r2(survival2_model8) # marginal: 0.334; conditional: 0.917
r2(survival2_model9) # marginal: 0.348; conditional: 0.914
r2(survival2_model10) # marginal: 0.357; conditional: 0.902
r2(survival2_model11) # marginal: 0.326; conditional: 0.921


# Model averaging of top models
survival2_avg <- model.avg(survival2_set, subset = delta <= 2)
summary(survival2_avg)




save.image("RData/06.4_linear-models-5.0.RData")
