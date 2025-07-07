# Created: 2025-07-03
# Updated: 2025-07-07

# Purpose: Run linear models with Change_Reproductive_culms, Change_Total_Live_Culms, 
#   Change_BGDensity, and Change_BGCover as response variable.

# Continuous explanatory variables are centered and scaled. 

# Updates from 06.2_linear-models-3.0.R script:
#   Mostly this is to restart model numbering now that I have explanatory variables confirmed,
#     and am looking at random effects and glmmTMB (b) vs. lme4 (a).
#   Also no longer modeling survival as linear model, instead using Tweedie GLM.

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

## Total change 1: model 8 from 3.0 ---------------------------------------

# 1: lme4 version
total1a <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                  Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_BGDensity_scaled +
                  Prev_year_precip_scaled * PlotSlope_scaled + 
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  Aspect * PlotSlope_scaled + (1 | Site / Transect),
                data = culm.change.flat.rm)
summary(total1a)
r2(total1a) # marginal: 0.132; conditional: 0.411
check_model(total1a)

# 1: glmmTMB version
total1b <- glmmTMB(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                    Change_BGDensity_scaled +
                    Prev_year_precip_scaled * Change_BGDensity_scaled +
                    Prev_year_precip_scaled * PlotSlope_scaled + 
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Prev_year_precip_scaled * Change_HerbCover_scaled +
                    Aspect * PlotSlope_scaled + (1 | Site / Transect),
                  data = culm.change.flat.rm,
                  family = gaussian) 
summary(total1b)
r2(total1b) # marginal: 0.149; conditional: 0.359
res.total1b <- simulateResiduals(total1b)
plotQQunif(res.total1b)
plotResiduals(res.total1b)
check_model(total1b)


### 8: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
total1a_set <- dredge(total1a) # got warning about convergence, but table looks complete 
total1b_set <- dredge(total1b)

# Examine best model
total1a_best.model <- get.models(total1a_set, 1)[[1]]
summary(total1a_best.model)
r2(total1a_best.model) # marginal: 0.132; conditional: 0.411
check_model(total1a_best.model)

total1b_best.model <- get.models(total1b_set, 1)[[1]]
summary(total1b_best.model)
r2(total1b_best.model) # marginal: 0.147; conditional: 0.356
res.total1b_best.model <- simulateResiduals(total1b_best.model)
plotQQunif(res.total1b_best.model)
plotResiduals(res.total1b_best.model)
check_model(total1b_best.model)


# Examine models within 2 AICc units of best and assign each top model to separate object
total1a_top <- subset(total1a_set, delta <= 2) # 1 model

total1b_top <- subset(total1b_set, delta <= 2) # 4 models
for (i in 1:nrow(total1b_top)) {
  assign(paste0("total1b_model", i), get.models(total1b_top, subset = i)[[1]])
} 


# R^2 of top models
r2(total1a_model1) # marginal: 0.132; conditional: 0.411

r2(total1b_model1) # marginal: 0.147; conditional: 0.356
r2(total1b_model2) # marginal: 0.131; conditional: 0.367
r2(total1b_model3) # marginal: 0.143; conditional: 0.359
r2(total1b_model4) # marginal: 0.127; conditional: 0.369


# Model averaging of top models
#   model averaging not needed for lme4 version; only 1 top model
summary(total1a_best.model)

total1b_avg <- model.avg(total1b_set, subset = delta <= 2)
summary(total1b_avg)




# Reproductive culms ------------------------------------------------------

## Repro change 1: model 8 from 3.0 ---------------------------------------

# 1: lme4 version
repro1a <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                  Change_BGDensity_scaled +
                  Prev_year_precip_scaled * Change_BGDensity_scaled +
                  Prev_year_precip_scaled * PlotSlope_scaled + 
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  Aspect * PlotSlope_scaled + (1 | Site / Transect),
                data = culm.change.flat.rm)
summary(repro1a)
r2(repro1a) # marginal: 0.117; conditional: 0.286
check_model(repro1a)

# 1: glmmTMB version
repro1b <- glmmTMB(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                     Change_BGDensity_scaled +
                     Prev_year_precip_scaled * Change_BGDensity_scaled +
                     Prev_year_precip_scaled * PlotSlope_scaled + 
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     Aspect * PlotSlope_scaled + (1 | Site / Transect),
                   data = culm.change.flat.rm,
                   family = gaussian) 
summary(repro1b)
r2(repro1b) # marginal: 0.130; conditional: 0.243
res.repro1b <- simulateResiduals(repro1b)
plotQQunif(res.repro1b)
plotResiduals(res.repro1b)
check_model(repro1b)


### 8: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
repro1a_set <- dredge(repro1a) # got warning about convergence, but table looks complete 
repro1b_set <- dredge(repro1b)

# Examine best model
repro1a_best.model <- get.models(repro1a_set, 1)[[1]]
summary(repro1a_best.model)
r2(repro1a_best.model) # marginal: 0.115; conditional: 0.287
check_model(repro1a_best.model)

repro1b_best.model <- get.models(repro1b_set, 1)[[1]]
summary(repro1b_best.model)
r2(repro1b_best.model) # marginal: 0.123; conditional: 0.239
res.repro1b_best.model <- simulateResiduals(repro1b_best.model)
plotQQunif(res.repro1b_best.model)
plotResiduals(res.repro1b_best.model)
check_model(repro1b_best.model)


# Examine models within 2 AICc units of best and assign each top model to separate object
repro1a_top <- subset(repro1a_set, delta <= 2) # 4 models
for (i in 1:nrow(repro1a_top)) {
  assign(paste0("repro1a_model", i), get.models(repro1a_top, subset = i)[[1]])
} 

repro1b_top <- subset(repro1b_set, delta <= 2) # 4 models
for (i in 1:nrow(repro1b_top)) {
  assign(paste0("repro1b_model", i), get.models(repro1b_top, subset = i)[[1]])
} 


# R^2 of top models
r2(repro1a_model1) # marginal: 0.115; conditional: 0.287
r2(repro1a_model2) # marginal: 0.113; conditional: 0.290
r2(repro1a_model3) # marginal: 0.117; conditional: 0.286
r2(repro1a_model4) # marginal: 0.115; conditional: 0.289

r2(repro1b_model1) # marginal: 0.123; conditional: 0.239
r2(repro1b_model2) # marginal: 0.125; conditional: 0.238
r2(repro1b_model3) # marginal: 0.126; conditional: 0.237
r2(repro1b_model4) # marginal: 0.128; conditional: 0.236


# Model averaging of top models
repro1a_avg <- model.avg(repro1a_set, subset = delta <= 2)
summary(repro1a_avg)

repro1b_avg <- model.avg(repro1b_set, subset = delta <= 2)
summary(repro1b_avg)



# Buffelgrass density -----------------------------------------------------

## BG density 1: Add (1 | Site / Transect) --------------------------------

# 1: lme4 version
bgden1a <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + 
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                  Change_HerbCover_scaled + 
                  Prev_year_precip_scaled * PlotSlope_scaled +
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  (1 | Site / Transect),
                data = plot.change)
summary(bgden1a)
r2(bgden1a) # marginal: 0.308; conditional: 0.572
check_model(bgden1a)

# 1: glmmTMB version
bgden1b <- glmmTMB(Change_BGDensity ~ Prev_year_precip_scaled +
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                     Change_HerbCover_scaled + 
                     Prev_year_precip_scaled * PlotSlope_scaled +
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     (1 | Site / Transect),
                   data = plot.change,
                   family = gaussian)
summary(bgden1b)
r2(bgden1b) # marginal: 0.351; conditional: 0.540
res.bgden1b <- simulateResiduals(bgden1b)
plotQQunif(res.bgden1b)
plotResiduals(res.bgden1b)
check_model(bgden1b)


### 1: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgden1a_set <- dredge(bgden1a) # got warning about convergence, but table looks complete
bgden1b_set <- dredge(bgden1b)


# Examine best model
bgden1a_best.model <- get.models(bgden1a_set, 1)[[1]]
summary(bgden1a_best.model)
r2(bgden1a_best.model) # marginal: 0.305; conditional: 0.562
check_model(bgden1a_best.model)

bgden1b_best.model <- get.models(bgden1b_set, 1)[[1]]
summary(bgden1b_best.model)
r2(bgden1b_best.model) # marginal: 0.339; conditional: 0.532
res.bgden1b_best.model <- simulateResiduals(bgden1b_best.model)
plotQQunif(res.bgden1b_best.model)
plotResiduals(res.bgden1b_best.model)
check_model(bgden1b_best.model)


# Examine models within 2 AICc units of best and assign each top model to separate object
bgden1a_top <- subset(bgden1a_set, delta <= 2) # 3 models
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
r2(bgden1a_model3) # marginal: 0.311; conditional: 0.572

r2(bgden1b_model1) # marginal: 0.339; conditional: 0.532
r2(bgden1b_model2) # marginal: 0.336; conditional: 0.535


# Model averaging of top models
bgden1a_avg <- model.avg(bgden1a_set, subset = delta <= 2)
summary(bgden1a_avg)

bgden1b_avg <- model.avg(bgden1b_set, subset = delta <= 2) 
summary(bgden1b_avg)



# BG density 2: Add (1 | Site) --------------------------------------------

# 2: lme4 version
bgden2a <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + 
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                  Change_HerbCover_scaled + 
                  Prev_year_precip_scaled * PlotSlope_scaled +
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  (1 | Site),
                data = plot.change)
summary(bgden2a)
r2(bgden2a) # marginal: 0.391; conditional: 0.430
check_model(bgden2a)

# 2: glmmTMB version
bgden2b <- glmmTMB(Change_BGDensity ~ Prev_year_precip_scaled +
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                     Change_HerbCover_scaled + 
                     Prev_year_precip_scaled * PlotSlope_scaled +
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     (1 | Site),
                   data = plot.change,
                   family = gaussian)
summary(bgden2b)
r2(bgden2b) # marginal: 0.423; conditional: 0.437
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
check_model(bgden2a_best.model)

bgden2b_best.model <- get.models(bgden2b_set, 1)[[1]]
summary(bgden2b_best.model)
r2(bgden2b_best.model) # marginal: 0.428; conditional: 0.438
res.bgden2b_best.model <- simulateResiduals(bgden2b_best.model)
plotQQunif(res.bgden2b_best.model)
plotResiduals(res.bgden2b_best.model)
check_model(bgden2b_best.model)


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



## BG density 3: no random effects ----------------------------------------

# 3: lm version
bgden3a <- lm(Change_BGDensity ~ Prev_year_precip_scaled + 
                Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                Change_HerbCover_scaled + 
                Prev_year_precip_scaled * PlotSlope_scaled +
                Prev_year_precip_scaled * Change_ShrubCover_scaled +
                Prev_year_precip_scaled * Change_HerbCover_scaled,
              data = plot.change)
summary(bgden3a)
r2(bgden3a) # adjusted: 0.420
check_model(bgden3a)

# 3: glmmTMB version
bgden3b <- glmmTMB(Change_BGDensity ~ Prev_year_precip_scaled +
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                     Change_HerbCover_scaled + 
                     Prev_year_precip_scaled * PlotSlope_scaled +
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled,
                   data = plot.change,
                   family = gaussian)
summary(bgden3b)
r2(bgden3b) # adjusted: 0.417
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
r2(bgden3a_best.model) # 0.424
check_model(bgden3a_best.model)

bgden3b_best.model <- get.models(bgden3b_set, 1)[[1]]
summary(bgden3b_best.model)
r2(bgden3b_best.model) # adjusted: 0.422
res.bgden3b_best.model <- simulateResiduals(bgden3b_best.model)
plotQQunif(res.bgden3b_best.model)
plotResiduals(res.bgden3b_best.model)
check_model(bgden3b_best.model)


# Examine models within 2 AICc units of best and assign each top model to separate object
bgden3a_top <- subset(bgden3a_set, delta <= 2) # 2 models
for (i in 1:nrow(bgden3a_top)) {
  assign(paste0("bgden3a_model", i), get.models(bgden3a_top, subset = i)[[1]])
} 

bgden3b_top <- subset(bgden3b_set, delta <= 2) # 2 models
for (i in 1:nrow(bgden3b_top)) {
  assign(paste0("bgden3b_model", i), get.models(bgden3b_top, subset = i)[[1]])
} 


# R^2 of top models
r2(bgden3a_model1) # adjusted: 0.424
r2(bgden3a_model2) # adjusted: 0.418

r2(bgden3b_model1) # adjusted: 0.422
r2(bgden3b_model2) # adjusted: 0.415


# Model averaging of top models
bgden3a_avg <- model.avg(bgden3a_set, subset = delta <= 2)
summary(bgden3a_avg)

bgden3b_avg <- model.avg(bgden3b_set, subset = delta <= 2) 
summary(bgden3b_avg)




# Buffelgrass cover -------------------------------------------------------

## BG cover 1: Add (1 | Site / Transect) ----------------------------------

# 1: lme4 version
bgcov1a <- lmer(Change_BGCover ~ Prev_year_precip_scaled + 
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                  Change_HerbCover_scaled + 
                  Prev_year_precip_scaled * PlotSlope_scaled +
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  (1 | Site / Transect),
                data = plot.change)
summary(bgcov1a)
r2(bgcov1a) # marginal: 0.238; conditional: 0.396
check_model(bgcov1a)

# 1: glmmTMB version
bgcov1b <- glmmTMB(Change_BGCover ~ Prev_year_precip_scaled +
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                     Change_HerbCover_scaled + 
                     Prev_year_precip_scaled * PlotSlope_scaled +
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     (1 | Site / Transect),
                   data = plot.change,
                   family = gaussian)
summary(bgcov1b)
r2(bgcov1b) # marginal: 0.255; conditional: 0.354
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
bgcov1a_top <- subset(bgcov1a_set, delta <= 2) # 6 models
for (i in 1:nrow(bgcov1a_top)) {
  assign(paste0("bgcov1a_model", i), get.models(bgcov1a_top, subset = i)[[1]])
} 

bgcov1b_top <- subset(bgcov1b_set, delta <= 2) # 9 models
for (i in 1:nrow(bgcov1b_top)) {
  assign(paste0("bgcov1b_model", i), get.models(bgcov1b_top, subset = i)[[1]])
} 


# R^2 of top models
r2(bgcov1a_model1) # marginal: 0.240; conditional: 0.393
r2(bgcov1a_model2) # marginal: 0.239; conditional: 0.395
r2(bgcov1a_model3) # marginal: 0.239; conditional: 0.393
r2(bgcov1a_model4) # marginal: 0.238; conditional: 0.396
r2(bgcov1a_model5) # marginal: 0.240; conditional: 0.391
r2(bgcov1a_model6) # marginal: 0.240; conditional: 0.393

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
                  Prev_year_precip_scaled * PlotSlope_scaled +
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled +
                  (1 | Site),
                data = plot.change)
summary(bgcov2a)
r2(bgcov2a) # marginal: 0.249; conditional: 0.303
check_model(bgcov2a)

# 2: glmmTMB version
bgcov2b <- glmmTMB(Change_BGCover ~ Prev_year_precip_scaled +
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                     Change_HerbCover_scaled + 
                     Prev_year_precip_scaled * PlotSlope_scaled +
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled +
                     (1 | Site),
                   data = plot.change,
                   family = gaussian) # model convergence problem? didn't have problem in 06.2.R script
summary(bgcov2b)
r2(bgcov2b) # 
res.bgcov2b <- simulateResiduals(bgcov2b)
plotQQunif(res.bgcov2b)
plotResiduals(res.bgcov2b)
check_model(bgcov2b)


### 2: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgcov2a_set <- dredge(bgcov2a) 
bgcov2b_set <- dredge(bgcov2b)


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
bgcov2a_top <- subset(bgcov2a_set, delta <= 2) # 6 models
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
r2(bgcov2a_model2) # marginal: 0.251; conditional: 0.307
r2(bgcov2a_model3) # marginal: 0.251; conditional: 0.306
r2(bgcov2a_model4) # marginal: 0.249; conditional: 0.303
r2(bgcov2a_model5) # marginal: 0.251; conditional: 0.306
r2(bgcov2a_model6) # marginal: 0.249; conditional: 0.302

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



## BG cover 3: no random effects ------------------------------------------

# 3: lm version
bgcov3a <- lm(Change_BGCover ~ Prev_year_precip_scaled + 
                Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                Change_HerbCover_scaled + 
                Prev_year_precip_scaled * PlotSlope_scaled +
                Prev_year_precip_scaled * Change_ShrubCover_scaled +
                Prev_year_precip_scaled * Change_HerbCover_scaled,
              data = plot.change)
summary(bgcov3a)
r2(bgcov3a) # adjusted: 0.210
check_model(bgcov3a)

# 3: glmmTMB version
bgcov3b <- glmmTMB(Change_BGCover ~ Prev_year_precip_scaled +
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                     Change_HerbCover_scaled + 
                     Prev_year_precip_scaled * PlotSlope_scaled +
                     Prev_year_precip_scaled * Change_ShrubCover_scaled +
                     Prev_year_precip_scaled * Change_HerbCover_scaled,
                   data = plot.change,
                   family = gaussian)
summary(bgcov3b)
r2(bgcov3b) # adjusted: 0.206
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
r2(bgcov3a_best.model) # 0.206
check_model(bgcov3a_best.model)

bgcov3b_best.model <- get.models(bgcov3b_set, 1)[[1]]
summary(bgcov3b_best.model)
r2(bgcov3b_best.model) # adjusted: 0.203
res.bgcov3b_best.model <- simulateResiduals(bgcov3b_best.model)
plotQQunif(res.bgcov3b_best.model)
plotResiduals(res.bgcov3b_best.model)
check_model(bgcov3b_best.model)


# Examine models within 2 AICc units of best and assign each top model to separate object
bgcov3a_top <- subset(bgcov3a_set, delta <= 2) # 7 models
for (i in 1:nrow(bgcov3a_top)) {
  assign(paste0("bgcov3a_model", i), get.models(bgcov3a_top, subset = i)[[1]])
} 

bgcov3b_top <- subset(bgcov3b_set, delta <= 2) # 7 models
for (i in 1:nrow(bgcov3b_top)) {
  assign(paste0("bgcov3b_model", i), get.models(bgcov3b_top, subset = i)[[1]])
} 


# R^2 of top models
r2(bgcov3a_model1) # adjusted: 0.206
r2(bgcov3a_model2) # adjusted: 0.213
r2(bgcov3a_model3) # adjusted: 0.206
r2(bgcov3a_model4) # adjusted: 0.204
r2(bgcov3a_model5) # adjusted: 0.212
r2(bgcov3a_model6) # adjusted: 0.204
r2(bgcov3a_model7) # adjusted: 0.212

r2(bgcov3b_model1) # adjusted: 0.203
r2(bgcov3b_model2) # adjusted: 0.209
r2(bgcov3b_model3) # adjusted: 0.202
r2(bgcov3b_model4) # adjusted: 0.201
r2(bgcov3b_model5) # adjusted: 0.208
r2(bgcov3b_model6) # adjusted: 0.201
r2(bgcov3b_model7) # adjusted: 0.208


# Model averaging of top models
bgcov3a_avg <- model.avg(bgcov3a_set, subset = delta <= 2)
summary(bgcov3a_avg)

bgcov3b_avg <- model.avg(bgcov3b_set, subset = delta <= 2) 
summary(bgcov3b_avg)




# Survival ----------------------------------------------------------------

## Survival 1: Tweedie GLM, add (1 | Site / Transect) ---------------------

# 1: glmmTMB version
survival1 <- glmmTMB(survival_perc ~ Prev_year_precip_scaled +
                       Aspect + PlotSlope_scaled + ShrubCover_scaled +
                       HerbCover_scaled + BGDensity_scaled +
                       Prev_year_precip_scaled * PlotSlope_scaled +
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
check_model(survival1) # posterior looks weird
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
plotResiduals(res.survival1_best.model)
check_model(survival1_best.model) # posterior looks weird

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
                       Prev_year_precip_scaled * PlotSlope_scaled +
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
plotResiduals(res.survival2) # starts to look more funky, I think due to uneven sample size
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
plotResiduals(res.survival2_best.model)
check_model(survival2_best.model) # posterior looks weird

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




## Survival 3: Tweedie GLM, no random effects -----------------------------

# 3: glmmTMB version
survival3 <- glmmTMB(survival_perc ~ Prev_year_precip_scaled +
                       Aspect + PlotSlope_scaled + ShrubCover_scaled +
                       HerbCover_scaled + BGDensity_scaled +
                       Prev_year_precip_scaled * PlotSlope_scaled +
                       Prev_year_precip_scaled * ShrubCover_scaled +
                       Prev_year_precip_scaled * HerbCover_scaled +
                       Prev_year_precip_scaled * BGDensity_scaled,
                     data = dat.survival,
                     family = tweedie(link = "log"))
summary(survival3)
res.survival3 <- simulateResiduals(survival3)
plotQQunif(res.survival3)
plotResiduals(res.survival3) 
check_collinearity(survival3) 
check_model(survival3)
check_overdispersion(survival3)


### 3: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
survival3_set <- dredge(survival3)

# Examine best model
survival3_best.model <- get.models(survival3_set, 1)[[1]]
summary(survival3_best.model)
res.survival3_best.model <- simulateResiduals(survival3_best.model)
plotQQunif(res.survival3_best.model)
plotResiduals(res.survival3_best.model)
check_model(survival3_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
survival3_top <- subset(survival3_set, delta <= 2) # 5 models
for (i in 1:nrow(survival3_top)) {
  assign(paste0("survival3_model", i), get.models(survival3_top, subset = i)[[1]])
} 

# Model averaging of top models
survival3_avg <- model.avg(survival3_set, subset = delta <= 2)
summary(survival3_avg)



save.image("RData/06.3_linear-models-4.0.RData")
