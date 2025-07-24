# Created: 2025-07-09
# Updated: 2025-07-24

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
#   Also tried models with Site as fixed effect and Transect as random, and without Site at all,
#     because Site only has 4 levels, which is low for a random effect (vs. 27 transects).

# Density, cover, and survival models did not change that much from 06.3.R because they never included 
#   Aspect*PlotSlope and did not include Precip*PlotSlope after model selection.
# Total & repro culm count models had Precip*PlotSlope as significant previously, but I think this adds too much
#   complication to the model and isn't really what we are interested in, so that is why it is dropped here.

# Posterior predictions only look good for cover models; for all others, it is very wrong around 0-1.

# Changing random effects doesn't really help with model fit/posterior predictions or residuals plots, but
#   having Site as fixed effect does lower AICc and increase marginal R^2.

# Conclusions:
#   Should go with lme4 versions where possible because REML better accounts for unequal group sizes.

#   In defense of version 3 (7/11):
#     Should use model 3 (Site as fixed, Transect as random) because this accounts for differences between
#       sites by including it, spatial & temporal autocorrelation by including a random effect, and Transect has many  
#       more levels than Site, so it is better as a random effect.
#     Including Site as fixed implies these sites are of special interest vs. just a random sample; can't make
#       broader conclusions about buffelgrass in other circumstances.
#     Survival model cannot include Site as fixed effect because of high VIF; Site is altogether excluded.

#   In defense of version 1 (7/15):
#     Should use model 1 because most of the models do run and this fits the experimental
#       design best. But R ^2 are very low and not all models converge.

# Update from 7/24:
#   Tweedie GLM actually isn't good for survival data because it does not consider an upper bound
#     (data can be 0 - 1, inclusive of bounds). Beta-inflated (BEINF) needed instead, and have to
#     transform 0 and 1.

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

# Transform 0s and 1s
dat.survival <- dat.survival %>% 
  mutate(survival_transf = pmin(pmax(survival_perc, 1e-6), 1 - 1e-6))



# Total culms -------------------------------------------------------------

## Total change 1: (1 | Site / Transect) ----------------------------------

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



## Total change 3: Site as fixed, Transect as random ----------------------

# 3: lme4 version
total3a <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                  Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  Site + (1 | Transect),
                data = culm.change.flat.rm)
summary(total3a)
r2(total3a) # marginal: 0.359; conditional: 0.408
res.total3a <- simulateResiduals(total3a)
plotQQunif(res.total3a)
plotResiduals(res.total3a)
check_model(total3a)

# 3: glmmTMB version
total3b <- glmmTMB(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                     Change_BGDensity_scaled +
                     Prev_year_precip_scaled * Change_BGDensity_scaled +
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     Site + (1 | Transect),
                   data = culm.change.flat.rm,
                   family = gaussian) 
summary(total3b)
r2(total3b) # marginal: 0.369; conditional: 0.395
res.total3b <- simulateResiduals(total3b)
plotQQunif(res.total3b)
plotResiduals(res.total3b)
check_model(total3b)


### 3: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
total3a_set <- dredge(total3a) 
total3b_set <- dredge(total3b)

# Examine best model
total3a_best.model <- get.models(total3a_set, 1)[[1]]
summary(total3a_best.model)
r2(total3a_best.model) # marginal: 0.359; conditional: 0.408
res.total3a_best.model <- simulateResiduals(total3a_best.model)
plotQQunif(res.total3a_best.model)
plotResiduals(res.total3a_best.model)
check_model(total3a_best.model) # posterior prediction poor at 0

total3b_best.model <- get.models(total3b_set, 1)[[1]]
summary(total3b_best.model)
r2(total3b_best.model) # marginal: 0.370; conditional: 0.392
res.total3b_best.model <- simulateResiduals(total3b_best.model)
plotQQunif(res.total3b_best.model)
plotResiduals(res.total3b_best.model)
check_model(total3b_best.model) # posterior prediction poor at 0


# Examine models within 2 AICc units of best and assign each top model to separate object
total3a_top <- subset(total3a_set, delta <= 2) # 1 model

total3b_top <- subset(total3b_set, delta <= 2) # 7 models
for (i in 1:nrow(total3b_top)) {
  assign(paste0("total3b_model", i), get.models(total3b_top, subset = i)[[1]])
} 


# R^2 of top models
r2(total3a_best.model) # marginal: 0.359; conditional: 0.408

r2(total3b_model1) # marginal: 0.370; conditional: 0.392
r2(total3b_model2) # marginal: 0.369; conditional: 0.395
r2(total3b_model3) # marginal: 0.355; conditional: 0.392
r2(total3b_model4) # marginal: 0.370; conditional: 0.392
r2(total3b_model5) # marginal: 0.369; conditional: 0.396
r2(total3b_model6) # marginal: 0.355; conditional: 0.392
r2(total3b_model7) # marginal: 0.353; conditional: 0.395


# Model averaging of top models
summary(total3a_best.model)

total3b_avg <- model.avg(total3b_set, subset = delta <= 2)
summary(total3b_avg)



## Total change 4: Transect as random, no Site ----------------------------

# 4: lme4 version
total4a <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                  Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  (1 | Transect),
                data = culm.change.flat.rm)
summary(total4a)
r2(total4a) # marginal: 0.144; conditional: 0.391
res.total4a <- simulateResiduals(total4a)
plotQQunif(res.total4a)
plotResiduals(res.total4a)
check_model(total4a)

# 4: glmmTMB version
total4b <- glmmTMB(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                     Change_BGDensity_scaled +
                     Prev_year_precip_scaled * Change_BGDensity_scaled +
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     (1 | Transect),
                   data = culm.change.flat.rm,
                   family = gaussian) 
summary(total4b)
r2(total4b) # marginal: 0.156; conditional: 0.356
res.total4b <- simulateResiduals(total4b)
plotQQunif(res.total4b)
plotResiduals(res.total4b)
check_model(total4b)


### 4: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
total4a_set <- dredge(total4a) 
total4b_set <- dredge(total4b)

# Examine best model
total4a_best.model <- get.models(total4a_set, 1)[[1]]
summary(total4a_best.model)
r2(total4a_best.model) # marginal: 0.144; conditional: 0.391
res.total4a_best.model <- simulateResiduals(total4a_best.model)
plotQQunif(res.total4a_best.model)
plotResiduals(res.total4a_best.model)
check_model(total4a_best.model) # posterior prediction poor at 0

total4b_best.model <- get.models(total4b_set, 1)[[1]]
summary(total4b_best.model)
r2(total4b_best.model) # marginal: 0.119; conditional: 0.355
res.total4b_best.model <- simulateResiduals(total4b_best.model)
plotQQunif(res.total4b_best.model)
plotResiduals(res.total4b_best.model)
check_model(total4b_best.model) # posterior prediction poor at 0


# Examine models within 2 AICc units of best and assign each top model to separate object
total4a_top <- subset(total4a_set, delta <= 2) # 2 models
for (i in 1:nrow(total4a_top)) {
  assign(paste0("total4a_model", i), get.models(total4a_top, subset = i)[[1]])
} 

total4b_top <- subset(total4b_set, delta <= 2) # 4 models
for (i in 1:nrow(total4b_top)) {
  assign(paste0("total4b_model", i), get.models(total4b_top, subset = i)[[1]])
} 


# R^2 of top models
r2(total4a_model1) # marginal: 0.144; conditional: 0.391
r2(total4a_model2) # marginal: 0.147; conditional: 0.386

r2(total4b_model1) # marginal: 0.119; conditional: 0.355
r2(total4b_model2) # marginal: 0.125; conditional: 0.338
r2(total4b_model3) # marginal: 0.117; conditional: 0.359
r2(total4b_model4) # marginal: 0.124; conditional: 0.342


# Model averaging of top models
total4a_avg <- model.avg(total4a_set, subset = delta <= 2)
summary(total4a_avg)

total4b_avg <- model.avg(total4b_set, subset = delta <= 2)
summary(total4b_avg)




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



## Repro change 3: Site as fixed, Transect as random ----------------------

# 3: lme4 version
repro3a <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                  Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  Site + (1 | Transect),
                data = culm.change.flat.rm)
summary(repro3a)
r2(repro3a) # marginal: 0.293; conditional: 0.341
res.repro3a <- simulateResiduals(repro3a)
plotQQunif(res.repro3a)
plotResiduals(res.repro3a)
check_model(repro3a)

# 3: glmmTMB version
repro3b <- glmmTMB(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                     Change_BGDensity_scaled +
                     Prev_year_precip_scaled * Change_BGDensity_scaled +
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     Site + (1 | Transect),
                   data = culm.change.flat.rm,
                   family = gaussian) 
summary(repro3b)
r2(repro3b) # marginal: 0.300; conditional: 0.323
res.repro3b <- simulateResiduals(repro3b)
plotQQunif(res.repro3b)
plotResiduals(res.repro3b)
check_model(repro3b)


### 3: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
repro3a_set <- dredge(repro3a) 
repro3b_set <- dredge(repro3b)

# Examine best model
repro3a_best.model <- get.models(repro3a_set, 1)[[1]]
summary(repro3a_best.model)
r2(repro3a_best.model) # marginal: 0.292; conditional: 0.338
res.repro3a_best.model <- simulateResiduals(repro3a_best.model)
plotQQunif(res.repro3a_best.model)
plotResiduals(res.repro3a_best.model)
check_model(repro3a_best.model) # posterior prediction is poor at 0

repro3b_best.model <- get.models(repro3b_set, 1)[[1]]
summary(repro3b_best.model)
r2(repro3b_best.model) # marginal: 0.298; conditional: 0.323
res.repro3b_best.model <- simulateResiduals(repro3b_best.model)
plotQQunif(res.repro3b_best.model)
plotResiduals(res.repro3b_best.model)
check_model(repro3b_best.model) # posterior prediction is poor at 0


# Examine models within 2 AICc units of best and assign each top model to separate object
repro3a_top <- subset(repro3a_set, delta <= 2) # 8 models
for (i in 1:nrow(repro3a_top)) {
  assign(paste0("repro3a_model", i), get.models(repro3a_top, subset = i)[[1]])
} 

repro3b_top <- subset(repro3b_set, delta <= 2) # 6 models
for (i in 1:nrow(repro3b_top)) {
  assign(paste0("repro3b_model", i), get.models(repro3b_top, subset = i)[[1]])
} 


# R^2 of top models
r2(repro3a_model1) # marginal: 0.292; conditional: 0.338
r2(repro3a_model2) # marginal: 0.291; conditional: 0.339
r2(repro3a_model3) # marginal: 0.292; conditional: 0.343
r2(repro3a_model4) # marginal: 0.293; conditional: 0.341
r2(repro3a_model5) # marginal: 0.292; conditional: 0.337
r2(repro3a_model6) # marginal: 0.293; conditional: 0.341
r2(repro3a_model7) # marginal: 0.292; conditional: 0.343
r2(repro3a_model8) # marginal: 0.291; conditional: 0.339

r2(repro3b_model1) # marginal: 0.298; conditional: 0.323
r2(repro3b_model2) # marginal: 0.299; conditional: 0.322
r2(repro3b_model3) # marginal: 0.294; conditional: 0.330
r2(repro3b_model4) # marginal: 0.298; conditional: 0.323
r2(repro3b_model5) # marginal: 0.299; conditional: 0.322
r2(repro3b_model6) # marginal: 0.298; conditional: 0.325


# Model averaging of top models
repro3a_avg <- model.avg(repro3a_set, subset = delta <= 2)
summary(repro3a_avg)

repro3b_avg <- model.avg(repro3b_set, subset = delta <= 2)
summary(repro3b_avg)



## Repro change 4: Transect as random, no Site ----------------------------

# 4: lme4 version
repro4a <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                  Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  (1 | Transect),
                data = culm.change.flat.rm)
summary(repro4a)
r2(repro4a) # marginal: 0.155; conditional: 0.318
res.repro4a <- simulateResiduals(repro4a)
plotQQunif(res.repro4a)
plotResiduals(res.repro4a)
check_model(repro4a)

# 4: glmmTMB version
repro4b <- glmmTMB(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                     Change_BGDensity_scaled +
                     Prev_year_precip_scaled * Change_BGDensity_scaled +
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     (1 | Transect),
                   data = culm.change.flat.rm,
                   family = gaussian) 
summary(repro4b)
r2(repro4b) # marginal: 0.169; conditional: 0.290
res.repro4b <- simulateResiduals(repro4b)
plotQQunif(res.repro4b)
plotResiduals(res.repro4b)
check_model(repro4b)


### 4: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
repro4a_set <- dredge(repro4a) 
repro4b_set <- dredge(repro4b)

# Examine best model
repro4a_best.model <- get.models(repro4a_set, 1)[[1]]
summary(repro4a_best.model)
r2(repro4a_best.model) # marginal: 0.155; conditional: 0.320
res.repro4a_best.model <- simulateResiduals(repro4a_best.model)
plotQQunif(res.repro4a_best.model)
plotResiduals(res.repro4a_best.model)
check_model(repro4a_best.model) # posterior prediction is poor at 0

repro4b_best.model <- get.models(repro4b_set, 1)[[1]]
summary(repro4b_best.model)
r2(repro4b_best.model) # marginal: 0.163; conditional: 0.293
res.repro4b_best.model <- simulateResiduals(repro4b_best.model)
plotQQunif(res.repro4b_best.model)
plotResiduals(res.repro4b_best.model)
check_model(repro4b_best.model) # posterior prediction is poor at 0


# Examine models within 2 AICc units of best and assign each top model to separate object
repro4a_top <- subset(repro4a_set, delta <= 2) # 5 models
for (i in 1:nrow(repro4a_top)) {
  assign(paste0("repro4a_model", i), get.models(repro4a_top, subset = i)[[1]])
} 

repro4b_top <- subset(repro4b_set, delta <= 2) # 10 models
for (i in 1:nrow(repro4b_top)) {
  assign(paste0("repro4b_model", i), get.models(repro4b_top, subset = i)[[1]])
} 


# R^2 of top models
r2(repro4a_model1) # marginal: 0.155; conditional: 0.320
r2(repro4a_model2) # marginal: 0.151; conditional: 0.322
r2(repro4a_model3) # marginal: 0.155; conditional: 0.318
r2(repro4a_model4) # marginal: 0.151; conditional: 0.321
r2(repro4a_model5) # marginal: 0.147; conditional: 0.333

r2(repro4b_model1) # marginal: 0.163; conditional: 0.293
r2(repro4b_model2) # marginal: 0.167; conditional: 0.291
r2(repro4b_model3) # marginal: 0.152; conditional: 0.292
r2(repro4b_model4) # marginal: 0.146; conditional: 0.315
r2(repro4b_model5) # marginal: 0.157; conditional: 0.303
r2(repro4b_model6) # marginal: 0.150; conditional: 0.308
r2(repro4b_model7) # marginal: 0.164; conditional: 0.291
r2(repro4b_model8) # marginal: 0.155; conditional: 0.291
r2(repro4b_model9) # marginal: 0.135; conditional: 0.315
r2(repro4b_model10) # marginal: 0.155; conditional: 0.290


# Model averaging of top models
repro4a_avg <- model.avg(repro4a_set, subset = delta <= 2)
summary(repro4a_avg)

repro4b_avg <- model.avg(repro4b_set, subset = delta <= 2)
summary(repro4b_avg)




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
res.bgden1a_best.model <- simulateResiduals(bgden1a_best.model)
plotQQunif(res.bgden1a_best.model)
plotResiduals(res.bgden1a_best.model)
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




## BG density 2: Add (1 | Site) -------------------------------------------

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
res.bgden2a_best.model <- simulateResiduals(bgden2a_best.model)
plotQQunif(res.bgden2a_best.model)
plotResiduals(res.bgden2a_best.model)
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



## BG density 3: Site as fixed, Transect as random ------------------------

# 3: lme4 version
bgden3a <- lmer(Change_BGDensity ~ Prev_year_precip_scaled +
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                     Change_HerbCover_scaled + 
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     Site + (1 | Transect),
                   data = plot.change)
summary(bgden3a)
r2(bgden3a) # marginal: 0.411; conditional: 0.599
res.bgden3a <- simulateResiduals(bgden3a)
plotQQunif(res.bgden3a)
plotResiduals(res.bgden3a)
check_model(bgden3a)

# 3: glmmTMB version
bgden3b <- glmmTMB(Change_BGDensity ~ Prev_year_precip_scaled +
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                     Change_HerbCover_scaled + 
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     Site + (1 | Transect),
                   data = plot.change,
                   family = gaussian)
summary(bgden3b)
r2(bgden3b) # marginal: 0.453; conditional: 0.569
res.bgden3b <- simulateResiduals(bgden3b)
plotQQunif(res.bgden3b)
plotResiduals(res.bgden3b)
check_model(bgden3b)


### 3: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgden3a_set <- dredge(bgden3a) 
bgden3b_set <- dredge(bgden3b)


# Examine best model
bgden3a_best.model <- get.models(bgden3a_set, 1)[[1]]
summary(bgden3a_best.model)
r2(bgden3a_best.model) # marginal: 0.411; conditional: 0.598
res.bgden3a_best.model <- simulateResiduals(bgden3a_best.model)
plotQQunif(res.bgden3a_best.model)
plotResiduals(res.bgden3a_best.model) # looks a bit janky
check_model(bgden3a_best.model) # posterior prediction is poor around 0-1

bgden3b_best.model <- get.models(bgden3b_set, 1)[[1]]
summary(bgden3b_best.model)
r2(bgden3b_best.model) # marginal: 0.446; conditional: 0.570
res.bgden3b_best.model <- simulateResiduals(bgden3b_best.model)
plotQQunif(res.bgden3b_best.model)
plotResiduals(res.bgden3b_best.model) # looks a bit janky
check_model(bgden3b_best.model) # posterior prediction is poor around 0-1


# Examine models within 2 AICc units of best and assign each top model to separate object
bgden3a_top <- subset(bgden3a_set, delta <= 2) # 2 models
for (i in 1:nrow(bgden3a_top)) {
  assign(paste0("bgden3a_model", i), get.models(bgden3a_top, subset = i)[[1]])
} 

bgden3b_top <- subset(bgden3b_set, delta <= 2) # 3 models
for (i in 1:nrow(bgden3b_top)) {
  assign(paste0("bgden3b_model", i), get.models(bgden3b_top, subset = i)[[1]])
} 


# R^2 of top models
r2(bgden3a_model1) # marginal: 0.411; conditional: 0.598
r2(bgden3a_model2) # marginal: 0.414; conditional: 0.598

r2(bgden3b_model1) # marginal: 0.446; conditional: 0.570
r2(bgden3b_model2) # marginal: 0.350; conditional: 0.532
r2(bgden3b_model3) # marginal: 0.451; conditional: 0.570


# Model averaging of top models
bgden3a_avg <- model.avg(bgden3a_set, subset = delta <= 2)
summary(bgden3a_avg)

bgden3b_avg <- model.avg(bgden3b_set, subset = delta <= 2) 
summary(bgden3b_avg)



## BG density 4: Transect as random, no Site ------------------------------

# 4: lme4 version
bgden4a <- lmer(Change_BGDensity ~ Prev_year_precip_scaled +
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                  Change_HerbCover_scaled + 
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  (1 | Transect),
                data = plot.change)
summary(bgden4a)
r2(bgden4a) # marginal: 0.315; conditional: 0.552
res.bgden4a <- simulateResiduals(bgden4a)
plotQQunif(res.bgden4a)
plotResiduals(res.bgden4a)
check_model(bgden4a)

# 3: glmmTMB version
bgden4b <- glmmTMB(Change_BGDensity ~ Prev_year_precip_scaled +
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                     Change_HerbCover_scaled + 
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     (1 | Transect),
                   data = plot.change,
                   family = gaussian)
summary(bgden4b)
r2(bgden4b) # marginal: 0.356; conditional: 0.527
res.bgden4b <- simulateResiduals(bgden4b)
plotQQunif(res.bgden4b)
plotResiduals(res.bgden4b)
check_model(bgden4b)


### 4: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgden4a_set <- dredge(bgden4a) 
bgden4b_set <- dredge(bgden4b)


# Examine best model
bgden4a_best.model <- get.models(bgden4a_set, 1)[[1]]
summary(bgden4a_best.model)
r2(bgden4a_best.model) # marginal: 0.319; conditional: 0.553
res.bgden4a_best.model <- simulateResiduals(bgden4a_best.model)
plotQQunif(res.bgden4a_best.model)
plotResiduals(res.bgden4a_best.model) # looks kind of janky
check_model(bgden4a_best.model) # posterior prediction is poor around 0-1

bgden4b_best.model <- get.models(bgden4b_set, 1)[[1]]
summary(bgden4b_best.model)
r2(bgden4b_best.model) # marginal: 0.350; conditional: 0.532
res.bgden4b_best.model <- simulateResiduals(bgden4b_best.model)
plotQQunif(res.bgden4b_best.model)
plotResiduals(res.bgden4b_best.model) # looks kind of janky
check_model(bgden4b_best.model) # posterior prediction is poor around 0-1


# Examine models within 2 AICc units of best and assign each top model to separate object
bgden4a_top <- subset(bgden4a_set, delta <= 2) # 2 models
for (i in 1:nrow(bgden4a_top)) {
  assign(paste0("bgden4a_model", i), get.models(bgden4a_top, subset = i)[[1]])
} 

bgden4b_top <- subset(bgden4b_set, delta <= 2) # 1 model


# R^2 of top models
r2(bgden4a_model1) # marginal: 0.319; conditional: 0.553
r2(bgden4a_model2) # marginal: 0.315; conditional: 0.555

r2(bgden4b_best.model) # marginal: 0.350; conditional: 0.532


# Model averaging of top models
bgden4a_avg <- model.avg(bgden4a_set, subset = delta <= 2)
summary(bgden4a_avg)

summary(bgden4b_best.model)




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



## BG cover 2: Add (1 | Site) ---------------------------------------------

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



## BG cover 3: Site as fixed, Transect as random --------------------------

# 3: lme4 version
bgcov3a <- lmer(Change_BGCover ~ Prev_year_precip_scaled +
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                  Change_HerbCover_scaled + 
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  Site + (1 | Transect),
                data = plot.change)
summary(bgcov3a)
r2(bgcov3a) # marginal: 0.248; conditional: 0.377
res.bgcov3a <- simulateResiduals(bgcov3a)
plotQQunif(res.bgcov3a)
plotResiduals(res.bgcov3a)
check_model(bgcov3a)

# 3: glmmTMB version
bgcov3b <- glmmTMB(Change_BGCover ~ Prev_year_precip_scaled +
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                     Change_HerbCover_scaled + 
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     Site + (1 | Transect),
                   data = plot.change,
                   family = gaussian)
summary(bgcov3b)
r2(bgcov3b) # marginal: 0.274; conditional: 0.341
res.bgcov3b <- simulateResiduals(bgcov3b)
plotQQunif(res.bgcov3b)
plotResiduals(res.bgcov3b)
check_model(bgcov3b)


### 3: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgcov3a_set <- dredge(bgcov3a) 
bgcov3b_set <- dredge(bgcov3b)


# Examine best model
bgcov3a_best.model <- get.models(bgcov3a_set, 1)[[1]]
summary(bgcov3a_best.model)
r2(bgcov3a_best.model) # marginal: 0.248; conditional: 0.377
res.bgcov3a_best.model <- simulateResiduals(bgcov3a_best.model)
plotQQunif(res.bgcov3a_best.model)
plotResiduals(res.bgcov3a_best.model)
check_model(bgcov3a_best.model) 

bgcov3b_best.model <- get.models(bgcov3b_set, 1)[[1]]
summary(bgcov3b_best.model)
r2(bgcov3b_best.model) # marginal: 0.254; conditional: 0.355
res.bgcov3b_best.model <- simulateResiduals(bgcov3b_best.model)
plotQQunif(res.bgcov3b_best.model)
plotResiduals(res.bgcov3b_best.model) 
check_model(bgcov3b_best.model)


# Examine models within 2 AICc units of best and assign each top model to separate object
bgcov3a_top <- subset(bgcov3a_set, delta <= 2) # 5 models
for (i in 1:nrow(bgcov3a_top)) {
  assign(paste0("bgcov3a_model", i), get.models(bgcov3a_top, subset = i)[[1]])
} 

bgcov3b_top <- subset(bgcov3b_set, delta <= 2) # 14 models
for (i in 1:nrow(bgcov3b_top)) {
  assign(paste0("bgcov3b_model", i), get.models(bgcov3b_top, subset = i)[[1]])
} 


# R^2 of top models
r2(bgcov3a_model1) # marginal: 0.248; conditional: 0.377
r2(bgcov3a_model2) # marginal: 0.248; conditional: 0.377
r2(bgcov3a_model3) # marginal: 0.248; conditional: 0.377
r2(bgcov3a_model4) # marginal: 0.238; conditional: 0.375
r2(bgcov3a_model5) # marginal: 0.238; conditional: 0.376


r2(bgcov3b_model1) # marginal: 0.254; conditional: 0.355
r2(bgcov3b_model2) # marginal: 0.261; conditional: 0.335
r2(bgcov3b_model3) # marginal: 0.242; conditional: 0.365
r2(bgcov3b_model4) # marginal: 0.234; conditional: 0.353
r2(bgcov3b_model5) # marginal: 0.188; conditional: 0.388
r2(bgcov3b_model6) # marginal: 0.194; conditional: 0.339
r2(bgcov3b_model7) # marginal: 0.242; conditional: 0.343
r2(bgcov3b_model8) # marginal: 0.272; conditional: 0.341
r2(bgcov3b_model9) # marginal: 0.181; conditional: 0.380
r2(bgcov3b_model10) # marginal: 0.190; conditional: 0.374
r2(bgcov3b_model11) # marginal: 0.249; conditional: 0.329
r2(bgcov3b_model12) # marginal: 0.255; conditional: 0.356
r2(bgcov3b_model13) # marginal: 0.262; conditional: 0.336
r2(bgcov3b_model14) # marginal: 0.243; conditional: 0.367


# Model averaging of top models
bgcov3a_avg <- model.avg(bgcov3a_set, subset = delta <= 2)
summary(bgcov3a_avg)

bgcov3b_avg <- model.avg(bgcov3b_set, subset = delta <= 2) 
summary(bgcov3b_avg)



## BG cover 4: Transect as random, no Site --------------------------------

# 4: lme4 version
bgcov4a <- lmer(Change_BGCover ~ Prev_year_precip_scaled +
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                  Change_HerbCover_scaled + 
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  (1 | Transect),
                data = plot.change)
summary(bgcov4a)
r2(bgcov4a) # marginal: 0.239; conditional: 0.383
res.bgcov4a <- simulateResiduals(bgcov4a)
plotQQunif(res.bgcov4a)
plotResiduals(res.bgcov4a)
check_model(bgcov4a)

# 3: glmmTMB version
bgcov4b <- glmmTMB(Change_BGCover ~ Prev_year_precip_scaled +
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                     Change_HerbCover_scaled + 
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     (1 | Transect),
                   data = plot.change,
                   family = gaussian)
summary(bgcov4b)
r2(bgcov4b) # marginal: 0.255; conditional: 0.354
res.bgcov4b <- simulateResiduals(bgcov4b)
plotQQunif(res.bgcov4b)
plotResiduals(res.bgcov4b)
check_model(bgcov4b)


### 4: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgcov4a_set <- dredge(bgcov4a) 
bgcov4b_set <- dredge(bgcov4b)


# Examine best model
bgcov4a_best.model <- get.models(bgcov4a_set, 1)[[1]]
summary(bgcov4a_best.model)
r2(bgcov4a_best.model) # marginal: 0.240; conditional: 0.384
res.bgcov4a_best.model <- simulateResiduals(bgcov4a_best.model)
plotQQunif(res.bgcov4a_best.model)
plotResiduals(res.bgcov4a_best.model) 
check_model(bgcov4a_best.model)

bgcov4b_best.model <- get.models(bgcov4b_set, 1)[[1]]
summary(bgcov4b_best.model)
r2(bgcov4b_best.model) # marginal: 0.254; conditional: 0.355
res.bgcov4b_best.model <- simulateResiduals(bgcov4b_best.model)
plotQQunif(res.bgcov4b_best.model)
plotResiduals(res.bgcov4b_best.model)
check_model(bgcov4b_best.model) 


# Examine models within 2 AICc units of best and assign each top model to separate object
bgcov4a_top <- subset(bgcov4a_set, delta <= 2) # 3 models
for (i in 1:nrow(bgcov4a_top)) {
  assign(paste0("bgcov4a_model", i), get.models(bgcov4a_top, subset = i)[[1]])
} 

bgcov4b_top <- subset(bgcov4b_set, delta <= 2) # 9 models
for (i in 1:nrow(bgcov4b_top)) {
  assign(paste0("bgcov4b_model", i), get.models(bgcov4b_top, subset = i)[[1]])
}


# R^2 of top models
r2(bgcov4a_model1) # marginal: 0.240; conditional: 0.384
r2(bgcov4a_model2) # marginal: 0.239; conditional: 0.383
r2(bgcov4a_model3) # marginal: 0.240; conditional: 0.383

r2(bgcov4b_model1) # marginal: 0.254; conditional: 0.355
r2(bgcov4b_model2) # marginal: 0.242; conditional: 0.365
r2(bgcov4b_model3) # marginal: 0.234; conditional: 0.353
r2(bgcov4b_model4) # marginal: 0.188; conditional: 0.388
r2(bgcov4b_model5) # marginal: 0.242; conditional: 0.343
r2(bgcov4b_model6) # marginal: 0.181; conditional: 0.380
r2(bgcov4b_model7) # marginal: 0.190; conditional: 0.374
r2(bgcov4b_model8) # marginal: 0.255; conditional: 0.356
r2(bgcov4b_model9) # marginal: 0.243; conditional: 0.367


# Model averaging of top models
bgcov4a_avg <- model.avg(bgcov4a_set, subset = delta <= 2)
summary(bgcov4a_avg)

bgcov4b_avg <- model.avg(bgcov4b_set, subset = delta <= 2)
summary(bgcov4b_avg)




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



## Survival 3: Site as fixed, Transect as random --------------------------

# 3: glmmTMB version
survival3 <- glmmTMB(survival_perc ~ Prev_year_precip_scaled +
                       Aspect + PlotSlope_scaled + ShrubCover_scaled +
                       HerbCover_scaled + BGDensity_scaled +
                       Prev_year_precip_scaled * ShrubCover_scaled +
                       Prev_year_precip_scaled * HerbCover_scaled +
                       Prev_year_precip_scaled * BGDensity_scaled +
                       Site + (1 | Transect),
                     data = dat.survival,
                     family = tweedie(link = "log"))
summary(survival3)
r2(survival3) # can't compute
res.survival3 <- simulateResiduals(survival3)
plotQQunif(res.survival3)
plotResiduals(res.survival3) 
check_collinearity(survival3) # Site has high VIF 
check_model(survival3) 
check_overdispersion(survival3) # underdispersion detected

# did not proceed with model selection because of high VIF for site



## Survival 4: Transect as fixed, no Site ---------------------------------

# 4: glmmTMB version
survival4 <- glmmTMB(survival_perc ~ Prev_year_precip_scaled +
                       Aspect + PlotSlope_scaled + ShrubCover_scaled +
                       HerbCover_scaled + BGDensity_scaled +
                       Prev_year_precip_scaled * ShrubCover_scaled +
                       Prev_year_precip_scaled * HerbCover_scaled +
                       Prev_year_precip_scaled * BGDensity_scaled +
                       (1 | Transect),
                     data = dat.survival,
                     family = tweedie(link = "log"))
summary(survival4)
r2(survival4) # marginal: 0.593; conditional: 0.804
res.survival4 <- simulateResiduals(survival4)
plotQQunif(res.survival4)
plotResiduals(res.survival4) 
check_model(survival4)
check_overdispersion(survival4)


### 4: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
survival4_set <- dredge(survival4)

# Examine best model
survival4_best.model <- get.models(survival4_set, 1)[[1]]
summary(survival4_best.model)
r2(survival4_best.model) # marginal: 0.554; conditional: 0.833
res.survival4_best.model <- simulateResiduals(survival4_best.model)
plotQQunif(res.survival4_best.model)
plotResiduals(res.survival4_best.model) # kind of janky
check_model(survival4_best.model) # posterior prediction looks weird at 0

# Examine models within 2 AICc units of best and assign each top model to separate object
survival4_top <- subset(survival4_set, delta <= 2) # 8 models
for (i in 1:nrow(survival4_top)) {
  assign(paste0("survival4_model", i), get.models(survival4_top, subset = i)[[1]])
} 

# R^2 of top models
r2(survival4_model1) # marginal: 0.554; conditional: 0.833
r2(survival4_model2) # marginal: 0.571; conditional: 0.824
r2(survival4_model3) # marginal: 0.565; conditional: 0.820
r2(survival4_model4) # marginal: 0.551; conditional: 0.842
r2(survival4_model5) # marginal: 0.558; conditional: 0.832
r2(survival4_model6) # marginal: 0.559; conditional: 0.826
r2(survival4_model7) # marginal: 0.563; conditional: 0.829
r2(survival4_model8) # marginal: 0.544; conditional: 0.831


# Model averaging of top models
survival4_avg <- model.avg(survival4_set, subset = delta <= 2)
summary(survival4_avg)



## Survival 5: BEINF -----------------------------------------------

# 5: glmmTMB version
survival5 <- glmmTMB(survival_transf ~ Prev_year_precip_scaled +
                       Aspect + PlotSlope_scaled + ShrubCover_scaled +
                       HerbCover_scaled + BGDensity_scaled +
                       Prev_year_precip_scaled * ShrubCover_scaled +
                       Prev_year_precip_scaled * HerbCover_scaled +
                       Prev_year_precip_scaled * BGDensity_scaled +
                       (1 | Site / Transect),
                     data = dat.survival,
                     family = beta_family(link = "logit"),
                     ziformula = ~1,
                     dispformula = ~1)
summary(survival5)
r2(survival5) # marginal: 0.333; conditional: 0.521
res.survival5 <- simulateResiduals(survival5)
plotQQunif(res.survival5)
plotResiduals(res.survival5) # yikes
check_collinearity(survival5) 
check_model(survival5)
check_overdispersion(survival5)


### 5: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
survival5_set <- dredge(survival5) 

# Examine best model
survival5_best.model <- get.models(survival5_set, 1)[[1]]
summary(survival5_best.model)
r2(survival5_best.model) # marginal: 0.306; conditional: 0.591
res.survival5_best.model <- simulateResiduals(survival5_best.model)
plotQQunif(res.survival5_best.model)
plotResiduals(res.survival5_best.model) # really does not look great
check_model(survival5_best.model) 

# Examine models within 2 AICc units of best and assign each top model to separate object
survival5_top <- subset(survival5_set, delta <= 2) 
for (i in 1:nrow(survival5_top)) {
  assign(paste0("survival5_model", i), get.models(survival5_top, subset = i)[[1]])
} 

# R^2 of top models
r2(survival5_model1) # marginal: 0.306; conditional: 0.591
r2(survival5_model2) # marginal: 0.302; conditional: 0.590
r2(survival5_model3) # marginal: 0.311; conditional: 0.565
r2(survival5_model4) # marginal: 0.310; conditional: 0.597
r2(survival5_model5) # marginal: 0.307; conditional: 0.565
r2(survival5_model6) # marginal: 0.322; conditional: 0.563
r2(survival5_model7) # marginal: 0.324; conditional: 0.540
r2(survival5_model8) # marginal: 0.329; conditional: 0.533
r2(survival5_model9) # marginal: 0.3131; conditional: 0.569
r2(survival5_model10) # marginal: 0.314; conditional: 0.578
r2(survival5_model11) # marginal: 0.324; conditional: 0.574
r2(survival5_model12) # marginal: 0.334; conditional: 0.540
r2(survival5_model13) # marginal: 0.310; conditional: 0.583
r2(survival5_model14) # marginal: 0.33``; conditional: 0.545


# Model averaging of top models
survival5_avg <- model.avg(survival5_set, subset = delta <= 2)
summary(survival5_avg)



save.image("RData/06.4_linear-models-5.0.RData")
