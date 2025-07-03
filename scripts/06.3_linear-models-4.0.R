# Created: 2025-07-03
# Updated: 2025-07-03

# Purpose: Run linear models with Change_Reproductive_culms, Change_Total_Live_Culms, 
#   Change_BGDensity, and Change_BGCover as response variable.

# Continuous explanatory variables are centered and scaled. 

# Updates from 06.2_linear-models-3.0.R script:
#   Mostly this is to restart model numbering now that I have explanatory variables confirmed,
#     and am looking at random effects and glmmTMB (b) vs. lme4 (a).

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

#   Model averaging not possible for glmmTMB version because not all models converged


# R^2 of top models
r2(bgcov2a_model1) # marginal: 0.252; conditional: 0.310
r2(bgcov2a_model2) # marginal: 0.251; conditional: 0.307
r2(bgcov2a_model3) # marginal: 0.251; conditional: 0.306
r2(bgcov2a_model4) # marginal: 0.249; conditional: 0.303
r2(bgcov2a_model5) # marginal: 0.251; conditional: 0.306
r2(bgcov2a_model6) # marginal: 0.249; conditional: 0.302


# Model averaging of top models
bgcov2a_avg <- model.avg(bgcov2a_set, subset = delta <= 2)
summary(bgcov2a_avg)

#   Model averaging not possible for glmmTMB version because not all models converged



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


save.image("RData/06.3_linear-models-4.0.RData")
