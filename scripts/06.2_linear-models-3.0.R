# Created: 2025-03-24
# Updated: 2025-04-29

# Purpose: Run linear models with Change_Reproductive_culms, Change_Total_Live_Culms, 
#   Change_BGDensity, and Change_BGCover as response variable.

# Continuous explanatory variables are centered and scaled. 
# Use (1 | Site / Transect) for culm counts. 

# Updates from 06.1.R script:
#   Response variables refer to change in culm count, density, or cover to better
#     pinpoint precip effects (culm counts are specific to individual).
#   Used model selection to identify important explanatory variables (HPC not needed for
#     these models; they run in a couple of minutes on local machine NMSU-DHJYFZ3).

# Dropping observations from "flat" aspect (20 obs total) allows for full model convergence during
#   model selection, which then allows for model averaging for total culms.

# Dropping (1 | Site) random effect allows for full model convergence during model selection for
#   change in BG cover.
# Because conditional R^2 can't be calculated for bgden5, bgden6 (with random effect dropped) should be
#   used instead. Also, this allows for better comparison with BG cover models.

# Centering and scaling variables means that Elevation_ft and Elevation_m are basically the same,
#   but I ran new models anyway.

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
  mutate(Perc_dev_scaled = scale(Perc_dev, center = TRUE, scale = TRUE)[, ],
         Elevation_ft_scaled = scale(Elevation_ft, center = TRUE, scale = TRUE)[, 1],
         Elevation_m_scaled = scale(Elevation_m, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         Change_BGDensity_scaled = scale(Change_BGDensity, scale = TRUE)[, 1],
         Change_BGCover_scaled = scale(Change_BGCover, scale = TRUE)[, 1],
         Change_ShrubCover_scaled = scale(Change_ShrubCover, scale = TRUE)[, 1],
         Change_HerbCover_scaled = scale(Change_HerbCover, scale = TRUE)[, 1])

culm.change.flat.rm <- culm.change %>% 
  filter(Aspect != "flat")

# Separate out plot-level data
plot.change <- culm.change %>% 
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm,
         -Change_Reproductive_culms, -Change_Total_Live_Culms) %>% 
  distinct(.keep_all = TRUE)

plot.change <- plot.change %>% 
  mutate(Perc_dev_scaled = scale(Perc_dev,center = TRUE, scale = TRUE)[, 1],
         Elevation_ft_scaled = scale(Elevation_ft, center = TRUE, scale = TRUE)[, 1],
         Elevation_m_scaled = scale(Elevation_m, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1]) %>% 
  filter(Aspect != "flat")


# Prepare survival data
dat.survival <- dat %>% 
  filter(!is.na(survival_perc),
         Aspect != "flat") %>% 
  mutate(Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         ShrubCover_scaled = scale(ShrubCover, center = TRUE, scale = TRUE)[, 1],
         HerbCover_scaled = scale(HerbCover, center = TRUE, scale = TRUE)[, 1],
         Elevation_m_scaled = scale(Elevation_m, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1])

dat.survival.plot <- dat.survival %>% 
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm) %>% 
  distinct(.keep_all = TRUE)



# Total culms -------------------------------------------------------------

## Total 1: All variables, no interactions --------------------------------

# 1: lme4 version
lme4.total1 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                      Change_HerbCover_scaled + Change_BGDensity_scaled + (1 | Site / Transect),
                    data = culm.change)
summary(lme4.total1)
r2(lme4.total1) # marginal: 0.149
check_model(lme4.total1)

# 1: glmmTMB version
total1 <- glmmTMB(Change_Total_Live_Culms ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                    Change_HerbCover_scaled + Change_BGDensity_scaled + (1 | Site / Transect),
                  data = culm.change,
                  family = gaussian)
summary(total1)
r2(total1) # marginal: 0.167
res.total1 <- simulateResiduals(total1)
plotQQunif(res.total1)
plotResiduals(res.total1)
check_collinearity(total1) 


### 1: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
total1_set <- dredge(total1)

# Examine best model
total1_best.model <- get.models(total1_set, 1)[[1]]
summary(total1_best.model)
r2(total1_best.model) # marginal: 0.091
check_model(total1_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
total1_top <- subset(total1_set, delta <= 2)
for (i in 1:nrow(total1_top)) {
  assign(paste0("total1_model", i), get.models(total1_top, subset = i)[[1]])
}

# Model averaging of top models
total1_avg <- model.avg(total1_set, delta <= 2)
summary(total1_avg)
total1_importance <- sw(total1_avg)
total1_importance.df <- data.frame(variable = names(total1_importance),
                                   importance = total1_importance)
total1_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
total1_avg.all <- model.avg(total1_set)
summary(total1_avg.all)
total1_importance.all <- sw(total1_avg.all)
total1_importance.all.df <- data.frame(variable = names(total1_importance.all),
                                       importance = total1_importance.all)
total1_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()



## Total 2: Add precip interactions ---------------------------------------

# 2: lme4 version
lme4.total2 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                      Change_BGDensity_scaled +
                      Prev_year_precip_scaled * Aspect +
                      Prev_year_precip_scaled * PlotSlope_scaled + 
                      Aspect * PlotSlope_scaled + (1 | Site / Transect),
                    data = culm.change) 
summary(lme4.total2)
r2(lme4.total2) # marginal: 0.312
check_model(lme4.total2) # collinearity issues

# 2: glmmTMB version
total2 <- glmmTMB(Change_Total_Live_Culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                    Change_BGDensity_scaled +
                    Prev_year_precip_scaled * Aspect +
                    Prev_year_precip_scaled * PlotSlope_scaled + 
                    Aspect * PlotSlope_scaled +
                    (1 | Site / Transect),
                  data = culm.change,
                  family = gaussian) 
summary(total2)
r2(total2) # marginal: 0.321
res.total2 <- simulateResiduals(total2)
plotQQunif(res.total2)
plotResiduals(res.total2)
check_collinearity(total2) # Prev_year_precip & Aspect highly correlated



## Total 3: Change precip*aspect to precip*density ------------------------

# 3: lme4 version
lme4.total3 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                      Change_BGDensity_scaled +
                      Prev_year_precip_scaled * Change_BGDensity_scaled +
                      Prev_year_precip_scaled * PlotSlope_scaled + 
                      Aspect * PlotSlope_scaled + (1 | Site / Transect),
                    data = culm.change)
summary(lme4.total3)
r2(lme4.total3) # marginal: 0.195
check_model(lme4.total3)

# 3: glmmTMB version
total3 <- glmmTMB(Change_Total_Live_Culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                    Change_BGDensity_scaled +
                    Prev_year_precip_scaled * Change_BGDensity_scaled +
                    Prev_year_precip_scaled * PlotSlope_scaled + 
                    Aspect * PlotSlope_scaled + (1 | Site / Transect),
                  data = culm.change,
                  family = gaussian) 
summary(total3)
r2(total3) # marginal: 0.217
res.total3 <- simulateResiduals(total3)
plotQQunif(res.total3)
plotResiduals(res.total3)
check_collinearity(total3)


### 3: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
total3_set <- dredge(total3)

# Examine best model
total3_best.model <- get.models(total3_set, 1)[[1]]
summary(total3_best.model)
r2(total3_best.model) # marginal: 0.195
res.total3_best.model <- simulateResiduals(total3_best.model)
plotQQunif(res.total3_best.model)
plotResiduals(res.total3_best.model)
check_model(total3_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
total3_top <- subset(total3_set, delta <= 2) %>% 
  filter(!is.na(df)) # not all models converged
for (i in 1:nrow(total3_top)) {
  assign(paste0("total3_model", i), get.models(total4_top, subset = i)[[1]])
} 

# Model averaging of top models - cannot average because some models did not converge



## Total 4: Add shrub*precip interaction ----------------------------------

# 4: lme4 version
lme4.total4 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                      Change_BGDensity_scaled +
                      Prev_year_precip_scaled * Change_BGDensity_scaled +
                      Prev_year_precip_scaled * PlotSlope_scaled + 
                      Prev_year_precip_scaled * Change_ShrubCover_scaled +
                      Aspect * PlotSlope_scaled + (1 | Site / Transect),
                    data = culm.change)
summary(lme4.total4)
r2(lme4.total4) # marginal: 0.224
check_model(lme4.total4)

# 4: glmmTMB version
total4 <- glmmTMB(Change_Total_Live_Culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                    Change_BGDensity_scaled +
                    Prev_year_precip_scaled * Change_BGDensity_scaled +
                    Prev_year_precip_scaled * PlotSlope_scaled + 
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Aspect * PlotSlope_scaled + (1 | Site / Transect),
                  data = culm.change,
                  family = gaussian) 
summary(total4)
r2(total4) # marginal: 0.250
res.total4 <- simulateResiduals(total4)
plotQQunif(res.total4)
plotResiduals(res.total4)
check_collinearity(total4)
check_model(total4)


### 4: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
total4_set <- dredge(total4)

# Examine best model
total4_best.model <- get.models(total4_set, 1)[[1]]
summary(total4_best.model)
r2(total4_best.model) # marginal: 0.233
res.total4_best.model <- simulateResiduals(total4_best.model)
plotQQunif(res.total4_best.model)
plotResiduals(res.total4_best.model)
check_model(total4_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
total4_top <- subset(total4_set, delta <= 2) %>% 
  filter(!is.na(df)) # not all models converged
for (i in 1:nrow(total4_top)) {
  assign(paste0("total4_model", i), get.models(total4_top, subset = i)[[1]])
} 

# Model averaging of top models - cannot average because some models did not converge



## Total 5: Add herb*precip interaction -----------------------------------

# 5: lme4 version
lme4.total5 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                      Change_BGDensity_scaled +
                      Prev_year_precip_scaled * Change_BGDensity_scaled +
                      Prev_year_precip_scaled * PlotSlope_scaled + 
                      Prev_year_precip_scaled * Change_ShrubCover_scaled +
                      Prev_year_precip_scaled * Change_HerbCover_scaled +
                      Aspect * PlotSlope_scaled + (1 | Site / Transect),
                    data = culm.change)
summary(lme4.total5)
r2(lme4.total5) # marginal: 0.210
check_model(lme4.total5)

# 5: glmmTMB version
total5 <- glmmTMB(Change_Total_Live_Culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                    Change_BGDensity_scaled +
                    Prev_year_precip_scaled * Change_BGDensity_scaled +
                    Prev_year_precip_scaled * PlotSlope_scaled + 
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Prev_year_precip_scaled * Change_HerbCover_scaled +
                    Aspect * PlotSlope_scaled + (1 | Site / Transect),
                  data = culm.change,
                  family = gaussian) 
summary(total5)
r2(total5) # marginal: 0.236
res.total5 <- simulateResiduals(total5)
plotQQunif(res.total5)
plotResiduals(res.total5)
check_collinearity(total5)
check_model(total5)


### 5: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
total5_set <- dredge(total5)

# Examine best model
total5_best.model <- get.models(total5_set, 1)[[1]]
summary(total5_best.model)
r2(total5_best.model) # marginal: 0.230
res.total5_best.model <- simulateResiduals(total5_best.model)
plotQQunif(res.total5_best.model)
plotResiduals(res.total5_best.model)
check_model(total5_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
total5_top <- subset(total5_set, delta <= 2) %>% 
  filter(!is.na(df)) # not all models converged
for (i in 1:nrow(total5_top)) {
  assign(paste0("total5_model", i), get.models(total5_top, subset = i)[[1]])
} 


# Model averaging of top models - cannot average because some models did not converge



## Total 6: Drop flat obs -------------------------------------------------

# 6: lme4 version
lme4.total6 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                      Change_BGDensity_scaled +
                      Prev_year_precip_scaled * Change_BGDensity_scaled +
                      Prev_year_precip_scaled * PlotSlope_scaled + 
                      Prev_year_precip_scaled * Change_ShrubCover_scaled +
                      Prev_year_precip_scaled * Change_HerbCover_scaled +
                      Aspect * PlotSlope_scaled + (1 | Site / Transect),
                    data = culm.change.flat.rm)
summary(lme4.total6)
r2(lme4.total6) # marginal: 0.210
check_model(lme4.total6)

# 6: glmmTMB version
total6 <- glmmTMB(Change_Total_Live_Culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                    Change_BGDensity_scaled +
                    Prev_year_precip_scaled * Change_BGDensity_scaled +
                    Prev_year_precip_scaled * PlotSlope_scaled + 
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Prev_year_precip_scaled * Change_HerbCover_scaled +
                    Aspect * PlotSlope_scaled + (1 | Site / Transect),
                  data = culm.change.flat.rm,
                  family = gaussian) 
summary(total6)
r2(total6) # marginal: 0.235
res.total6 <- simulateResiduals(total6)
plotQQunif(res.total6)
plotResiduals(res.total6)
check_collinearity(total6)
check_model(total6)


### 6: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
total6_set <- dredge(total6)

# Examine best model
total6_best.model <- get.models(total6_set, 1)[[1]]
summary(total6_best.model)
r2(total6_best.model) # marginal: 0.230
res.total6_best.model <- simulateResiduals(total6_best.model)
plotQQunif(res.total6_best.model)
plotResiduals(res.total6_best.model)
check_model(total6_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
total6_top <- subset(total6_set, delta <= 2) 
for (i in 1:nrow(total6_top)) {
  assign(paste0("total6_model", i), get.models(total6_top, subset = i)[[1]])
} 

# R^2 of top models
r2(total6_model1) # marginal: 0.230
r2(total6_model2) # marginal: 0.229
r2(total6_model3) # marginal: 0.230
r2(total6_model4) # marginal: 0.225

# Model averaging of top models
total6_avg <- model.avg(total6_set, subset = delta <= 2)
summary(total6_avg)
total6_importance <- sw(total6_avg)
total6_importance.df <- data.frame(variable = names(total6_importance),
                                   importance = total6_importance)
total6_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
total6_avg.all <- model.avg(total6_set)
summary(total6_avg.all)
total6_importance.all <- sw(total6_avg.all)
total6_importance.all.df <- data.frame(variable = names(total6_importance.all),
                                       importance = total6_importance.all)
total6_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()



# Total change 7: Switch elevation from ft to m ---------------------------

# 7: lme4 version
lme4.total7 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled + Elevation_m_scaled + 
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                      Change_BGDensity_scaled +
                      Prev_year_precip_scaled * Change_BGDensity_scaled +
                      Prev_year_precip_scaled * PlotSlope_scaled + 
                      Prev_year_precip_scaled * Change_ShrubCover_scaled +
                      Prev_year_precip_scaled * Change_HerbCover_scaled +
                      Aspect * PlotSlope_scaled + (1 | Site / Transect),
                    data = culm.change.flat.rm)
summary(lme4.total7)
r2(lme4.total7) # marginal: 0.210
check_model(lme4.total7)

# 7: glmmTMB version
total7 <- glmmTMB(Change_Total_Live_Culms ~ Prev_year_precip_scaled + Elevation_m_scaled + 
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                    Change_BGDensity_scaled +
                    Prev_year_precip_scaled * Change_BGDensity_scaled +
                    Prev_year_precip_scaled * PlotSlope_scaled + 
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Prev_year_precip_scaled * Change_HerbCover_scaled +
                    Aspect * PlotSlope_scaled + (1 | Site / Transect),
                  data = culm.change.flat.rm,
                  family = gaussian) 
summary(total7)
r2(total7) # marginal: 0.235
res.total7 <- simulateResiduals(total7)
plotQQunif(res.total7)
plotResiduals(res.total7)
check_collinearity(total7)
check_model(total7)


### 7: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
total7_set <- dredge(total7)

# Examine best model
total7_best.model <- get.models(total7_set, 1)[[1]]
summary(total7_best.model)
r2(total7_best.model) # marginal: 0.230
res.total7_best.model <- simulateResiduals(total7_best.model)
plotQQunif(res.total7_best.model)
plotResiduals(res.total7_best.model)
check_model(total7_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
total7_top <- subset(total7_set, delta <= 2) 
for (i in 1:nrow(total7_top)) {
  assign(paste0("total7_model", i), get.models(total7_top, subset = i)[[1]])
} 

# R^2 of top models
r2(total7_model1) # marginal: 0.230
r2(total7_model2) # marginal: 0.229
r2(total7_model3) # marginal: 0.230
r2(total7_model4) # marginal: 0.225

# Model averaging of top models
total7_avg <- model.avg(total7_set, subset = delta <= 2)
summary(total7_avg)
total7_importance <- sw(total7_avg)
total7_importance.df <- data.frame(variable = names(total7_importance),
                                   importance = total7_importance)
total7_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
total7_avg.all <- model.avg(total7_set)
summary(total7_avg.all)
total7_importance.all <- sw(total7_avg.all)
total7_importance.all.df <- data.frame(variable = names(total7_importance.all),
                                       importance = total7_importance.all)
total7_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()




# Reproductive culms ------------------------------------------------------

## Repro 1: All variables, no interactions --------------------------------

# 1: lme4 version
lme4.repro1 <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                     Change_HerbCover_scaled + Change_BGDensity_scaled + (1 | Site / Transect),
                   data = culm.change)
summary(lme4.repro1)
r2(lme4.repro1) # marginal: 0.086
check_model(lme4.repro1)

# 1: glmmTMB version
repro1 <- glmmTMB(Change_Reproductive_culms ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                    Change_HerbCover_scaled + Change_BGDensity_scaled + (1 | Site / Transect),
                  data = culm.change,
                  family = gaussian)
summary(repro1)
r2(repro1) # marginal: 0.095
res.repro1 <- simulateResiduals(repro1)
plotQQunif(res.repro1)
plotResiduals(res.repro1)
check_collinearity(repro1) 


### 1: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
repro1_set <- dredge(repro1)

# Examine best model
repro1_best.model <- get.models(repro1_set, 1)[[1]]
summary(repro1_best.model)
r2(repro1_best.model) # marginal: 0.091
check_model(repro1_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
repro1_top <- subset(repro1_set, delta <= 2)
for (i in 1:nrow(repro1_top)) {
  assign(paste0("repro1_model", i), get.models(repro1_top, subset = i)[[1]])
}

# Model averaging of top models
repro1_avg <- model.avg(repro1_set, delta <= 2)
summary(repro1_avg)
repro1_importance <- sw(repro1_avg)
repro1_importance.df <- data.frame(variable = names(repro1_importance),
                                   importance = repro1_importance)
repro1_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()



## Repro 2: Add precip interactions --------------------------------------

# 2: lme4 version
lme4.repro2 <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                      Change_BGDensity_scaled +
                      Prev_year_precip_scaled * Aspect +
                      Prev_year_precip_scaled * PlotSlope_scaled + 
                      Aspect * PlotSlope_scaled + (1 | Site / Transect),
                    data = culm.change) 
summary(lme4.repro2)
r2(lme4.repro2) # marginal: 0.178
check_model(lme4.repro2) # collinearity issues

# 2: glmmTMB version
repro2 <- glmmTMB(Change_Reproductive_culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                    Change_BGDensity_scaled +
                    Prev_year_precip_scaled * Aspect +
                    Prev_year_precip_scaled * PlotSlope_scaled + 
                    Aspect * PlotSlope_scaled +
                    (1 | Site / Transect),
                  data = culm.change,
                  family = gaussian) 
summary(repro2)
r2(repro2) # marginal: 0.217
res.repro2 <- simulateResiduals(repro2)
plotQQunif(res.repro2)
plotResiduals(res.repro2)
check_collinearity(repro2) # Prev_year_precip & Aspect highly correlated



## Repro 3: Change precip*aspect to precip*density -----------------------

# 3: lme4 version
lme4.repro3 <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                      Change_BGDensity_scaled +
                      Prev_year_precip_scaled * Change_BGDensity_scaled +
                      Prev_year_precip_scaled * PlotSlope_scaled + 
                      Aspect * PlotSlope_scaled + (1 | Site / Transect),
                   data = culm.change)
summary(lme4.repro3)
r2(lme4.repro3) # marginal: 0.133
check_model(lme4.repro3)

# 3: glmmTMB version
repro3 <- glmmTMB(Change_Reproductive_culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                        Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                        Change_BGDensity_scaled +
                        Prev_year_precip_scaled * Change_BGDensity_scaled +
                        Prev_year_precip_scaled * PlotSlope_scaled + 
                        Aspect * PlotSlope_scaled + (1 | Site / Transect),
                      data = culm.change,
                      family = gaussian) 
summary(repro3)
r2(repro3) # marginal: 0.152
res.repro3 <- simulateResiduals(repro3)
plotQQunif(res.repro3)
plotResiduals(res.repro3)
check_collinearity(repro3)


### 3: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
repro3_set <- dredge(repro3)

# Examine best model
repro3_best.model <- get.models(repro3_set, 1)[[1]]
summary(repro3_best.model)
r2(repro3_best.model) # marginal: 0.125
res.repro3_best.model <- simulateResiduals(repro3_best.model)
plotQQunif(res.repro3_best.model)
plotResiduals(res.repro3_best.model)
check_collinearity(repro3_best.model)
check_model(repro3_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
repro3_top <- subset(repro3_set, delta <= 2)
for (i in 1:nrow(repro3_top)) {
  assign(paste0("repro3_model", i), get.models(repro3_top, subset = i)[[1]])
}

# Model averaging of top models
repro3_avg <- model.avg(repro3_set, subset = delta <= 2)
summary(repro3_avg)
repro3_importance <- sw(repro3_avg)
repro3_importance.df <- data.frame(variable = names(repro3_importance),
                                   importance = repro3_importance)
repro3_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
repro3_avg.all <- model.avg(repro3_set)
summary(repro3_avg.all)
repro3_importance.all <- sw(repro3_avg.all)
repro3_importance.all.df <- data.frame(variable = names(repro3_importance.all),
                                   importance = repro3_importance.all)
repro3_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()



## Repro 4: Add shrub*precip interaction ----------------------------------

# 4: lme4 version
lme4.repro4 <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                      Change_BGDensity_scaled +
                      Prev_year_precip_scaled * Change_BGDensity_scaled +
                      Prev_year_precip_scaled * PlotSlope_scaled + 
                      Prev_year_precip_scaled * Change_ShrubCover_scaled +
                      Aspect * PlotSlope_scaled + (1 | Site / Transect),
                    data = culm.change)
summary(lme4.repro4)
r2(lme4.repro4) # marginal: 0.134
check_model(lme4.repro4)

# 4: glmmTMB version
repro4 <- glmmTMB(Change_Reproductive_culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                    Change_BGDensity_scaled +
                    Prev_year_precip_scaled * Change_BGDensity_scaled +
                    Prev_year_precip_scaled * PlotSlope_scaled + 
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Aspect * PlotSlope_scaled + (1 | Site / Transect),
                  data = culm.change,
                  family = gaussian) 
summary(repro4)
r2(repro4) # marginal: 0.154
res.repro4 <- simulateResiduals(repro4)
plotQQunif(res.repro4)
plotResiduals(res.repro4)
check_collinearity(repro4)
check_model(repro4)


### 4: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
repro4_set <- dredge(repro4)

# Examine best model
repro4_best.model <- get.models(repro4_set, 1)[[1]]
summary(repro4_best.model)
r2(repro4_best.model) # marginal: 0.125
res.repro4_best.model <- simulateResiduals(repro4_best.model)
plotQQunif(res.repro4_best.model)
plotResiduals(res.repro4_best.model)
check_model(repro4_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
repro4_top <- subset(repro4_set, delta <= 2)
for (i in 1:nrow(repro4_top)) {
  assign(paste0("repro4_model", i), get.models(repro4_top, subset = i)[[1]])
} 

# Model averaging of top models
repro4_avg <- model.avg(repro4_set, subset = delta <= 2)
summary(repro4_avg)
repro4_importance <- sw(repro4_avg)
repro4_importance.df <- data.frame(variable = names(repro4_importance),
                                   importance = repro4_importance)
repro4_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
repro4_avg.all <- model.avg(repro4_set)
summary(repro4_avg.all)
repro4_importance.all <- sw(repro4_avg.all)
repro4_importance.all.df <- data.frame(variable = names(repro4_importance.all),
                                       importance = repro4_importance.all)
repro4_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()



## Repro 5: Add herb*precip interaction -----------------------------------

# 5: lme4 version
lme4.repro5 <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                      Change_BGDensity_scaled +
                      Prev_year_precip_scaled * Change_BGDensity_scaled +
                      Prev_year_precip_scaled * PlotSlope_scaled + 
                      Prev_year_precip_scaled * Change_ShrubCover_scaled +
                      Prev_year_precip_scaled * Change_HerbCover_scaled +
                      Aspect * PlotSlope_scaled + (1 | Site / Transect),
                    data = culm.change)
summary(lme4.repro5)
r2(lme4.repro5) # marginal: 0.136
check_model(lme4.repro5)

# 5: glmmTMB version
repro5 <- glmmTMB(Change_Reproductive_culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                    Change_BGDensity_scaled +
                    Prev_year_precip_scaled * Change_BGDensity_scaled +
                    Prev_year_precip_scaled * PlotSlope_scaled + 
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Prev_year_precip_scaled * Change_HerbCover_scaled +
                    Aspect * PlotSlope_scaled + (1 | Site / Transect),
                  data = culm.change,
                  family = gaussian) 
summary(repro5)
r2(repro5) # marginal: 0.156
res.repro5 <- simulateResiduals(repro5)
plotQQunif(res.repro5)
plotResiduals(res.repro5)
check_collinearity(repro5)
check_model(repro5)


### 5: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
repro5_set <- dredge(repro5)

# Examine best model
repro5_best.model <- get.models(repro5_set, 1)[[1]]
summary(repro5_best.model)
r2(repro5_best.model) # marginal: 0.125
res.repro5_best.model <- simulateResiduals(repro5_best.model)
plotQQunif(res.repro5_best.model)
plotResiduals(res.repro5_best.model)
check_model(repro5_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
repro5_top <- subset(repro5_set, delta <= 2) 
for (i in 1:nrow(repro5_top)) {
  assign(paste0("repro5_model", i), get.models(repro5_top, subset = i)[[1]])
} 

# Model averaging of top models
repro5_avg <- model.avg(repro5_set, subset = delta <= 2)
summary(repro5_avg)
repro5_importance <- sw(repro5_avg)
repro5_importance.df <- data.frame(variable = names(repro5_importance),
                                   importance = repro5_importance)
repro5_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
repro5_avg.all <- model.avg(repro5_set)
summary(repro5_avg.all)
repro5_importance.all <- sw(repro5_avg.all)
repro5_importance.all.df <- data.frame(variable = names(repro5_importance.all),
                                       importance = repro5_importance.all)
repro5_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()



## Repro 6: Drop flat obs -------------------------------------------------

# 6: lme4 version
lme4.repro6 <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                      Change_BGDensity_scaled +
                      Prev_year_precip_scaled * Change_BGDensity_scaled +
                      Prev_year_precip_scaled * PlotSlope_scaled + 
                      Prev_year_precip_scaled * Change_ShrubCover_scaled +
                      Prev_year_precip_scaled * Change_HerbCover_scaled +
                      Aspect * PlotSlope_scaled + (1 | Site / Transect),
                    data = culm.change.flat.rm)
summary(lme4.repro6)
r2(lme4.repro6) # marginal: 0.134
check_model(lme4.repro6)

# 6: glmmTMB version
repro6 <- glmmTMB(Change_Reproductive_culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                    Change_BGDensity_scaled +
                    Prev_year_precip_scaled * Change_BGDensity_scaled +
                    Prev_year_precip_scaled * PlotSlope_scaled + 
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Prev_year_precip_scaled * Change_HerbCover_scaled +
                    Aspect * PlotSlope_scaled + (1 | Site / Transect),
                  data = culm.change.flat.rm,
                  family = gaussian) 
summary(repro6)
r2(repro6) # marginal: 0.154
res.repro6 <- simulateResiduals(repro6)
plotQQunif(res.repro6)
plotResiduals(res.repro6)
check_collinearity(repro6)
check_model(repro6)


### 6: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
repro6_set <- dredge(repro6)

# Examine best model
repro6_best.model <- get.models(repro6_set, 1)[[1]]
summary(repro6_best.model)
r2(repro6_best.model) # marginal: 0.123
res.repro6_best.model <- simulateResiduals(repro6_best.model)
plotQQunif(res.repro6_best.model)
plotResiduals(res.repro6_best.model)
check_model(repro6_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
repro6_top <- subset(repro6_set, delta <= 2) 
for (i in 1:nrow(repro6_top)) {
  assign(paste0("repro6_model", i), get.models(repro6_top, subset = i)[[1]])
} 

# R^2 of top models
r2(repro6_model1) # marginal: 0.123
r2(repro6_model2) # marginal: 0.125
r2(repro6_model3) # marginal: 0.126
r2(repro6_model4) # marginal: 0.128
r2(repro6_model5) # marginal: 0.137
r2(repro6_model6) # marginal: 0.140

# Model averaging of top models
repro6_avg <- model.avg(repro6_set, subset = delta <= 2)
summary(repro6_avg)
repro6_importance <- sw(repro6_avg)
repro6_importance.df <- data.frame(variable = names(repro6_importance),
                                   importance = repro6_importance)
repro6_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
repro6_avg.all <- model.avg(repro6_set)
summary(repro6_avg.all)
repro6_importance.all <- sw(repro6_avg.all)
repro6_importance.all.df <- data.frame(variable = names(repro6_importance.all),
                                       importance = repro6_importance.all)
repro6_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()



# Repro change 7: Switch elevation from ft to m ---------------------------

# 7: lme4 version
lme4.repro7 <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled + Elevation_m_scaled + 
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                      Change_BGDensity_scaled +
                      Prev_year_precip_scaled * Change_BGDensity_scaled +
                      Prev_year_precip_scaled * PlotSlope_scaled + 
                      Prev_year_precip_scaled * Change_ShrubCover_scaled +
                      Prev_year_precip_scaled * Change_HerbCover_scaled +
                      Aspect * PlotSlope_scaled + (1 | Site / Transect),
                    data = culm.change.flat.rm)
summary(lme4.repro7)
r2(lme4.repro7) # marginal: 0.134
check_model(lme4.repro7)

# 7: glmmTMB version
repro7 <- glmmTMB(Change_Reproductive_culms ~ Prev_year_precip_scaled + Elevation_m_scaled + 
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                    Change_BGDensity_scaled +
                    Prev_year_precip_scaled * Change_BGDensity_scaled +
                    Prev_year_precip_scaled * PlotSlope_scaled + 
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Prev_year_precip_scaled * Change_HerbCover_scaled +
                    Aspect * PlotSlope_scaled + (1 | Site / Transect),
                  data = culm.change.flat.rm,
                  family = gaussian) 
summary(repro7)
r2(repro7) # marginal: 0.154
res.repro7 <- simulateResiduals(repro7)
plotQQunif(res.repro7)
plotResiduals(res.repro7)
check_collinearity(repro7)
check_model(repro7)


### 7: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
repro7_set <- dredge(repro7)

# Examine best model
repro7_best.model <- get.models(repro7_set, 1)[[1]]
summary(repro7_best.model)
r2(repro7_best.model) # marginal: 0.123
res.repro7_best.model <- simulateResiduals(repro7_best.model)
plotQQunif(res.repro7_best.model)
plotResiduals(res.repro7_best.model)
check_model(repro7_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
repro7_top <- subset(repro7_set, delta <= 2) 
for (i in 1:nrow(repro7_top)) {
  assign(paste0("repro7_model", i), get.models(repro7_top, subset = i)[[1]])
} 

# R^2 of top models
r2(repro7_model1) # marginal: 0.123
r2(repro7_model2) # marginal: 0.125
r2(repro7_model3) # marginal: 0.126
r2(repro7_model4) # marginal: 0.128
r2(repro7_model5) # marginal: 0.137
r2(repro7_model6) # marginal: 0.140

# Model averaging of top models
repro7_avg <- model.avg(repro7_set, subset = delta <= 2)
summary(repro7_avg)
repro7_importance <- sw(repro7_avg)
repro7_importance.df <- data.frame(variable = names(repro7_importance),
                                   importance = repro7_importance)
repro7_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
repro7_avg.all <- model.avg(repro7_set)
summary(repro7_avg.all)
repro7_importance.all <- sw(repro7_avg.all)
repro7_importance.all.df <- data.frame(variable = names(repro7_importance.all),
                                       importance = repro7_importance.all)
repro7_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()




# Buffelgrass density -----------------------------------------------------

## BG density 1: All variables, no interactions ---------------------------

# 1: lme4 version
lme4.bgden1 <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                      Change_HerbCover_scaled + (1 | Site),
                    data = plot.change)
summary(lme4.bgden1)
r2(lme4.bgden1) # marginal: 0.366
check_model(lme4.bgden1)

# 1: glmmTMB version
bgden1 <- glmmTMB(Change_BGDensity ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                    Change_HerbCover_scaled + (1 | Site),
                  data = plot.change,
                  family = gaussian)
summary(bgden1)
r2(bgden1) # marginal: 0.426
res.bgden1 <- simulateResiduals(bgden1)
plotQQunif(res.bgden1)
plotResiduals(res.bgden1)
check_collinearity(bgden1) 


### 1: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgden1_set <- dredge(bgden1)

# Examine best model
bgden1_best.model <- get.models(bgden1_set, 1)[[1]]
summary(bgden1_best.model)
r2(bgden1_best.model) # marginal: 0.426; can't calculate random effects
check_model(bgden1_best.model)
res.bgden1_best.model <- simulateResiduals(bgden1_best.model)
plotQQunif(res.bgden1_best.model)
plotResiduals(res.bgden1_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
bgden1_top <- subset(bgden1_set, delta <= 2)
for (i in 1:nrow(bgden1_top)) {
  assign(paste0("bgden1_model", i), get.models(bgden1_top, subset = i)[[1]])
}

# Model averaging of top models
bgden1_avg <- model.avg(bgden1_set, delta <= 2) 
summary(bgden1_avg)
bgden1_importance <- sw(bgden1_avg)
bgden1_importance.df <- data.frame(variable = names(bgden1_importance),
                                   importance = bgden1_importance)
bgden1_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
bgden1_avg.all <- model.avg(bgden1_set)
summary(bgden1_avg.all)
bgden1_importance.all <- sw(bgden1_avg.all)
bgden1_importance.all.df <- data.frame(variable = names(bgden1_importance.all),
                                       importance = bgden1_importance.all)
bgden1_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()



## BG density 2: Add precip interactions ----------------------------------

# 2: lme4 version
lme4.bgden2 <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                      Change_HerbCover_scaled +
                      Prev_year_precip_scaled * Aspect +
                      Prev_year_precip_scaled * PlotSlope_scaled + 
                      Aspect * PlotSlope_scaled + 
                      (1 | Site),
                    data = plot.change)
summary(lme4.bgden2)
r2(lme4.bgden2) # marginal: 0.607
check_model(lme4.bgden2)

# 2: glmmTMB version
bgden2 <- glmmTMB(Change_BGDensity ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                    Change_HerbCover_scaled + 
                    Prev_year_precip_scaled * Aspect +
                    Prev_year_precip_scaled * PlotSlope_scaled + 
                    Aspect * PlotSlope_scaled + 
                    (1 | Site),
                  data = plot.change,
                  family = gaussian)
summary(bgden2)
r2(bgden2) # marginal: 0.612; can't calculate random effects
res.bgden2 <- simulateResiduals(bgden2)
plotQQunif(res.bgden2)
plotResiduals(res.bgden2)
check_collinearity(bgden2) # Prev_year_precip & Aspect correlated; Aspect & PlotSlope correlated



## BG density 3: Remove collinear interactions ----------------------------

# 3: lme4 version
lme4.bgden3 <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                      Change_HerbCover_scaled + 
                      Prev_year_precip_scaled * PlotSlope_scaled +
                      (1 | Site),
                    data = plot.change)
summary(lme4.bgden3)
r2(lme4.bgden3) # marginal: 0.367
check_model(lme4.bgden3)

# 3: glmmTMB version
bgden3 <- glmmTMB(Change_BGDensity ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                    Change_HerbCover_scaled + 
                    Prev_year_precip_scaled * PlotSlope_scaled +
                    (1 | Site),
                  data = plot.change,
                  family = gaussian)
summary(bgden3)
r2(bgden3) # marginal: 0.427; can't calculate random effects
res.bgden3 <- simulateResiduals(bgden3)
plotQQunif(res.bgden3)
plotResiduals(res.bgden3)
check_collinearity(bgden3) 


### 3: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgden3_set <- dredge(bgden3)

# Examine best model
bgden3_best.model <- get.models(bgden3_set, 1)[[1]]
summary(bgden3_best.model)
r2(bgden3_best.model) # marginal: 0.426; can't calculate random effects
res.bgden3_best.model <- simulateResiduals(bgden3_best.model)
plotQQunif(res.bgden3_best.model)
plotResiduals(res.bgden3_best.model)
check_collinearity(bgden3_best.model)
check_model(bgden3_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
bgden3_top <- subset(bgden3_set, delta <= 2)
for (i in 1:nrow(bgden3_top)) {
  assign(paste0("bgden3_model", i), get.models(bgden3_top, subset = i)[[1]])
}

# Model averaging of top models
bgden3_avg <- model.avg(bgden3_set, subset = delta <= 2) 
summary(bgden3_avg)
bgden3_importance <- sw(bgden3_avg)
bgden3_importance.df <- data.frame(variable = names(bgden3_importance),
                                   importance = bgden3_importance)
bgden3_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
bgden3_avg.all <- model.avg(bgden3_set)
summary(bgden3_avg.all)
bgden3_importance.all <- sw(bgden3_avg.all)
bgden3_importance.all.df <- data.frame(variable = names(bgden3_importance.all),
                                       importance = bgden3_importance.all)
bgden3_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()



## BG density 4: Add shrub*precip interaction -----------------------------

# 4: lme4 version
lme4.bgden4 <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                      Change_HerbCover_scaled + 
                      Prev_year_precip_scaled * PlotSlope_scaled +
                      Prev_year_precip_scaled * Change_ShrubCover_scaled +
                      (1 | Site),
                    data = plot.change)
summary(lme4.bgden4)
r2(lme4.bgden4) # marginal: 0.403
check_model(lme4.bgden4)

# 4: glmmTMB version
bgden4 <- glmmTMB(Change_BGDensity ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                    Change_HerbCover_scaled + 
                    Prev_year_precip_scaled * PlotSlope_scaled +
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    (1 | Site),
                  data = plot.change,
                  family = gaussian)
summary(bgden4)
r2(bgden4) # marginal: 0.451; can't calculate random effects
res.bgden4 <- simulateResiduals(bgden4)
plotQQunif(res.bgden4)
plotResiduals(res.bgden4)
check_collinearity(bgden4) 
check_model(bgden4)


### 4: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgden4_set <- dredge(bgden4)

# Examine best model
bgden4_best.model <- get.models(bgden4_set, 1)[[1]]
summary(bgden4_best.model)
r2(bgden4_best.model) # marginal: 0.449; can't calculate random effects
res.bgden4_best.model <- simulateResiduals(bgden4_best.model)
plotQQunif(res.bgden4_best.model)
plotResiduals(res.bgden4_best.model)
check_collinearity(bgden4_best.model)
check_model(bgden4_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
bgden4_top <- subset(bgden4_set, delta <= 2)
for (i in 1:nrow(bgden4_top)) {
  assign(paste0("bgden4_model", i), get.models(bgden4_top, subset = i)[[1]])
}

# Model averaging of top models
bgden4_avg <- model.avg(bgden4_set, subset = delta <= 2) 
summary(bgden4_avg)
bgden4_importance <- sw(bgden4_avg)
bgden4_importance.df <- data.frame(variable = names(bgden4_importance),
                                   importance = bgden4_importance)
bgden4_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
bgden4_avg.all <- model.avg(bgden4_set)
summary(bgden4_avg.all)
bgden4_importance.all <- sw(bgden4_avg.all)
bgden4_importance.all.df <- data.frame(variable = names(bgden4_importance.all),
                                       importance = bgden4_importance.all)
bgden4_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()



## BG density 5: Add herb*precip interaction ------------------------------

# 5: lme4 version
lme4.bgden5 <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                      Change_HerbCover_scaled + 
                      Prev_year_precip_scaled * PlotSlope_scaled +
                      Prev_year_precip_scaled * Change_ShrubCover_scaled +
                      Prev_year_precip_scaled * Change_HerbCover_scaled +
                      (1 | Site),
                    data = plot.change)
summary(lme4.bgden5)
r2(lme4.bgden5) # marginal: 0.418
check_model(lme4.bgden5)

# 5: glmmTMB version
bgden5 <- glmmTMB(Change_BGDensity ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                    Change_HerbCover_scaled + 
                    Prev_year_precip_scaled * PlotSlope_scaled +
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Prev_year_precip_scaled * Change_HerbCover_scaled +
                    (1 | Site),
                  data = plot.change,
                  family = gaussian)
summary(bgden5)
r2(bgden5) # marginal: 0.456; can't calculate random effects
res.bgden5 <- simulateResiduals(bgden5)
plotQQunif(res.bgden5)
plotResiduals(res.bgden5)
check_collinearity(bgden5) 
check_model(bgden5)


### 5: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgden5_set <- dredge(bgden5)

# Examine best model
bgden5_best.model <- get.models(bgden5_set, 1)[[1]]
summary(bgden5_best.model)
r2(bgden5_best.model) # marginal: 0.454; can't calculate random effects
res.bgden5_best.model <- simulateResiduals(bgden5_best.model)
plotQQunif(res.bgden5_best.model)
plotResiduals(res.bgden5_best.model)
check_collinearity(bgden5_best.model)
check_model(bgden5_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
bgden5_top <- subset(bgden5_set, delta <= 2)
for (i in 1:nrow(bgden5_top)) {
  assign(paste0("bgden5_model", i), get.models(bgden5_top, subset = i)[[1]])
}

r2(bgden5_model1) # marginal: 0.454
r2(bgden5_model2) # marginal: 0.449
r2(bgden5_model3) # marginal: 0.428
r2(bgden5_model4) # marginal: 0.442
r2(bgden5_model5) # marginal: 0.411
r2(bgden5_model6) # marginal: 0.450

# Model averaging of top models
bgden5_avg <- model.avg(bgden5_set, subset = delta <= 2) 
summary(bgden5_avg)
bgden5_importance <- sw(bgden5_avg)
bgden5_importance.df <- data.frame(variable = names(bgden5_importance),
                                   importance = bgden5_importance)
bgden5_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
bgden5_avg.all <- model.avg(bgden5_set)
summary(bgden5_avg.all)
bgden5_importance.all <- sw(bgden5_avg.all)
bgden5_importance.all.df <- data.frame(variable = names(bgden5_importance.all),
                                       importance = bgden5_importance.all)
bgden5_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()




## BG density 6: Drop (1 | Site) ------------------------------------------

# 6: lm version
lm.bgden6 <- lm(Change_BGDensity ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                  Change_HerbCover_scaled + 
                  Prev_year_precip_scaled * PlotSlope_scaled +
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled,
                data = plot.change)
summary(lm.bgden6)
r2(lm.bgden6) # adjusted: 0.426
check_model(lm.bgden6)

# 6: glmmTMB version
bgden6 <- glmmTMB(Change_BGDensity ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                    Change_HerbCover_scaled + 
                    Prev_year_precip_scaled * PlotSlope_scaled +
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Prev_year_precip_scaled * Change_HerbCover_scaled,
                  data = plot.change,
                  family = gaussian)
summary(bgden6)
r2(bgden6) # adjusted: 0.423
res.bgden6 <- simulateResiduals(bgden6)
plotQQunif(res.bgden6)
plotResiduals(res.bgden6)
check_collinearity(bgden6) 
check_model(bgden6)


### 6: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgden6_set <- dredge(bgden6)

# Examine best model
bgden6_best.model <- get.models(bgden6_set, 1)[[1]]
summary(bgden6_best.model)
r2(bgden6_best.model) # adjusted: 0.427
res.bgden6_best.model <- simulateResiduals(bgden6_best.model)
plotQQunif(res.bgden6_best.model)
plotResiduals(res.bgden6_best.model)
check_collinearity(bgden6_best.model)
check_model(bgden6_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
bgden6_top <- subset(bgden6_set, delta <= 2)
for (i in 1:nrow(bgden6_top)) {
  assign(paste0("bgden6_model", i), get.models(bgden6_top, subset = i)[[1]])
}

r2(bgden6_model1) # adjusted: 0.427
r2(bgden6_model2) # adjusted: 0.424
r2(bgden6_model3) # adjusted: 0.419
r2(bgden6_model4) # adjusted: 0.422
r2(bgden6_model5) # adjusted: 0.422
r2(bgden6_model6) # adjusted: 0.425

# Model averaging of top models
bgden6_avg <- model.avg(bgden6_set, subset = delta <= 2) 
summary(bgden6_avg)
bgden6_importance <- sw(bgden6_avg)
bgden6_importance.df <- data.frame(variable = names(bgden6_importance),
                                   importance = bgden6_importance)
bgden6_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
bgden6_avg.all <- model.avg(bgden6_set)
summary(bgden6_avg.all)
bgden6_importance.all <- sw(bgden6_avg.all)
bgden6_importance.all.df <- data.frame(variable = names(bgden6_importance.all),
                                       importance = bgden6_importance.all)
bgden6_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()



# BG density 7: Switch elevation from ft to m -----------------------------

# 7: lm version
lm.bgden7 <- lm(Change_BGDensity ~ Prev_year_precip_scaled + Elevation_m_scaled +
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                  Change_HerbCover_scaled + 
                  Prev_year_precip_scaled * PlotSlope_scaled +
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled,
                data = plot.change)
summary(lm.bgden7)
r2(lm.bgden7) # adjusted: 0.426
check_model(lm.bgden7)

# 7: glmmTMB version
bgden7 <- glmmTMB(Change_BGDensity ~ Prev_year_precip_scaled + Elevation_m_scaled +
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                    Change_HerbCover_scaled + 
                    Prev_year_precip_scaled * PlotSlope_scaled +
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Prev_year_precip_scaled * Change_HerbCover_scaled,
                  data = plot.change,
                  family = gaussian)
summary(bgden7)
r2(bgden7) # adjusted: 0.423
res.bgden7 <- simulateResiduals(bgden7)
plotQQunif(res.bgden7)
plotResiduals(res.bgden7)
check_collinearity(bgden7) 
check_model(bgden7)


### 7: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgden7_set <- dredge(bgden7)

# Examine best model
bgden7_best.model <- get.models(bgden7_set, 1)[[1]]
summary(bgden7_best.model)
r2(bgden7_best.model) # adjusted: 0.427
res.bgden7_best.model <- simulateResiduals(bgden7_best.model)
plotQQunif(res.bgden7_best.model)
plotResiduals(res.bgden7_best.model)
check_collinearity(bgden7_best.model)
check_model(bgden7_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
bgden7_top <- subset(bgden7_set, delta <= 2)
for (i in 1:nrow(bgden7_top)) {
  assign(paste0("bgden7_model", i), get.models(bgden7_top, subset = i)[[1]])
}

r2(bgden7_model1) # adjusted: 0.427
r2(bgden7_model2) # adjusted: 0.424
r2(bgden7_model3) # adjusted: 0.419
r2(bgden7_model4) # adjusted: 0.422
r2(bgden7_model5) # adjusted: 0.422
r2(bgden7_model6) # adjusted: 0.425

# Model averaging of top models
bgden7_avg <- model.avg(bgden7_set, subset = delta <= 2) 
summary(bgden7_avg)
bgden7_importance <- sw(bgden7_avg)
bgden7_importance.df <- data.frame(variable = names(bgden7_importance),
                                   importance = bgden7_importance)
bgden7_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
bgden7_avg.all <- model.avg(bgden7_set)
summary(bgden7_avg.all)
bgden7_importance.all <- sw(bgden7_avg.all)
bgden7_importance.all.df <- data.frame(variable = names(bgden7_importance.all),
                                       importance = bgden7_importance.all)
bgden7_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()



# Buffelgrass cover -------------------------------------------------------

## BG cover 1: All variables, no interactions -----------------------------

# 1: lme4 version
lme4.bgcov1 <- lmer(Change_BGCover ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                      Change_HerbCover_scaled + (1 | Site),
                    data = plot.change)
summary(lme4.bgcov1)
r2(lme4.bgcov1) # marginal: 0.251
check_model(lme4.bgcov1)

# 1: glmmTMB version
bgcov1 <- glmmTMB(Change_BGCover ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                    Change_HerbCover_scaled + (1 | Site),
                  data = plot.change,
                  family = gaussian)
summary(bgcov1)
r2(bgcov1) # marginal: 0.255
res.bgcov1 <- simulateResiduals(bgcov1)
plotQQunif(res.bgcov1)
plotResiduals(res.bgcov1)
check_collinearity(bgcov1) 


### 1: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgcov1_set <- dredge(bgcov1)

# Examine best model
bgcov1_best.model <- get.models(bgcov1_set, 1)[[1]]
summary(bgcov1_best.model)
r2(bgcov1_best.model) # marginal: 0.249
check_model(bgcov1_best.model)
res.bgcov1_best.model <- simulateResiduals(bgcov1_best.model)
plotQQunif(res.bgcov1_best.model)
plotResiduals(res.bgcov1_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
bgcov1_top <- subset(bgcov1_set, delta <= 2)  %>% 
  filter(!is.na(df))
for (i in 1:nrow(bgcov1_top)) {
  assign(paste0("bgcov1_model", i), get.models(bgcov1_top, subset = i)[[1]])
}

# Model averaging of top models - cannot average because some models did not converge



## BG cover 2: Add precip interactions ------------------------------------

# 2: lme4 version
lme4.bgcov2 <- lmer(Change_BGCover ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                      Change_HerbCover_scaled +
                      Prev_year_precip_scaled * Aspect +
                      Prev_year_precip_scaled * PlotSlope_scaled + 
                      Aspect * PlotSlope_scaled + 
                      (1 | Site),
                    data = plot.change)
summary(lme4.bgcov2)
r2(lme4.bgcov2) # marginal: 0.373
check_model(lme4.bgcov2)

# 2: glmmTMB version
bgcov2 <- glmmTMB(Change_BGCover ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                    Change_HerbCover_scaled + 
                    Prev_year_precip_scaled * Aspect +
                    Prev_year_precip_scaled * PlotSlope_scaled + 
                    Aspect * PlotSlope_scaled + 
                    (1 | Site),
                  data = plot.change,
                  family = gaussian)
summary(bgcov2)
r2(bgcov2) # marginal: 0.409
res.bgcov2 <- simulateResiduals(bgcov2)
plotQQunif(res.bgcov2)
plotResiduals(res.bgcov2)
check_collinearity(bgcov2) # Prev_year_precip & Aspect correlated; Aspect & PlotSlope correlated



## BG cover 3: Remove collinear interactions ------------------------------

# 3: lme4 version
lme4.bgcov3 <- lmer(Change_BGCover ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                      Change_HerbCover_scaled + 
                      Prev_year_precip_scaled * PlotSlope_scaled +
                      (1 | Site),
                    data = plot.change)
summary(lme4.bgcov3)
r2(lme4.bgcov3) # marginal: 0.251
check_model(lme4.bgcov3)

# 3: glmmTMB version
bgcov3 <- glmmTMB(Change_BGCover ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                    Change_HerbCover_scaled + 
                    Prev_year_precip_scaled * PlotSlope_scaled +
                    (1 | Site),
                  data = plot.change,
                  family = gaussian)
summary(bgcov3)
r2(bgcov3) # marginal: 0.258
res.bgcov3 <- simulateResiduals(bgcov3)
plotQQunif(res.bgcov3)
plotResiduals(res.bgcov3)
check_collinearity(bgcov3) 


### 3: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgcov3_set <- dredge(bgcov3)

# Examine best model
bgcov3_best.model <- get.models(bgcov3_set, 1)[[1]]
summary(bgcov3_best.model)
r2(bgcov3_best.model) # marginal: 0.249
res.bgcov3_best.model <- simulateResiduals(bgcov3_best.model)
plotQQunif(res.bgcov3_best.model)
plotResiduals(res.bgcov3_best.model)
check_collinearity(bgcov3_best.model)
check_model(bgcov3_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
bgcov3_top <- subset(bgcov3_set, delta <= 2) %>% 
  filter(!is.na(df))
for (i in 1:nrow(bgcov3_top)) {
  assign(paste0("bgcov3_model", i), get.models(bgcov3_top, subset = i)[[1]])
}

# Model averaging of top models - cannot average because some models did not converge



## BG cover 4: Add shrub*precip interaction -------------------------------

# 4: lme4 version
lme4.bgcov4 <- lmer(Change_BGCover ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                      Change_HerbCover_scaled + 
                      Prev_year_precip_scaled * PlotSlope_scaled +
                      Prev_year_precip_scaled * Change_ShrubCover_scaled +
                      (1 | Site),
                    data = plot.change)
summary(lme4.bgcov4)
r2(lme4.bgcov4) # marginal: 0.261
check_model(lme4.bgcov4)

# 4: glmmTMB version
bgcov4 <- glmmTMB(Change_BGCover ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                    Change_HerbCover_scaled + 
                    Prev_year_precip_scaled * PlotSlope_scaled +
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    (1 | Site),
                  data = plot.change,
                  family = gaussian)
summary(bgcov4) # convergence problem?
r2(bgcov4) # marginal: 0.267; can't calculate random effects
res.bgcov4 <- simulateResiduals(bgcov4)
plotQQunif(res.bgcov4)
plotResiduals(res.bgcov4)
check_collinearity(bgcov4) 
check_model(bgcov4)


### 4: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgcov4_set <- dredge(bgcov4)

# Examine best model
bgcov4_best.model <- get.models(bgcov4_set, 1)[[1]]
summary(bgcov4_best.model)
r2(bgcov4_best.model) # marginal: 0.249
res.bgcov4_best.model <- simulateResiduals(bgcov4_best.model)
plotQQunif(res.bgcov4_best.model)
plotResiduals(res.bgcov4_best.model)
check_collinearity(bgcov4_best.model)
check_model(bgcov4_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
bgcov4_top <- subset(bgcov4_set, delta <= 2) %>% 
  filter(!is.na(df)) # not all models converged
for (i in 1:nrow(bgcov4_top)) {
  assign(paste0("bgcov4_model", i), get.models(bgcov4_top, subset = i)[[1]])
}

# Model averaging of top models - cannot average because some models did not converge



## BG cover 5: Add herb*precip interaction --------------------------------

# 5: lme4 version
lme4.bgcov5 <- lmer(Change_BGCover ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                      Change_HerbCover_scaled + 
                      Prev_year_precip_scaled * PlotSlope_scaled +
                      Prev_year_precip_scaled * Change_ShrubCover_scaled +
                      Prev_year_precip_scaled * Change_HerbCover_scaled +
                      (1 | Site),
                    data = plot.change)
summary(lme4.bgcov5)
r2(lme4.bgcov5) # marginal: 0.418
check_model(lme4.bgcov5)

# 5: glmmTMB version
bgcov5 <- glmmTMB(Change_BGCover ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                    Change_HerbCover_scaled + 
                    Prev_year_precip_scaled * PlotSlope_scaled +
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Prev_year_precip_scaled * Change_HerbCover_scaled +
                    (1 | Site),
                  data = plot.change,
                  family = gaussian)
summary(bgcov5)
r2(bgcov5) # marginal: 0.268
res.bgcov5 <- simulateResiduals(bgcov5)
plotQQunif(res.bgcov5)
plotResiduals(res.bgcov5)
check_collinearity(bgcov5) 
check_model(bgcov5)


### 5: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgcov5_set <- dredge(bgcov5)

# Examine best model
bgcov5_best.model <- get.models(bgcov5_set, 1)[[1]]
summary(bgcov5_best.model)
r2(bgcov5_best.model) # marginal: 0.249
res.bgcov5_best.model <- simulateResiduals(bgcov5_best.model)
plotQQunif(res.bgcov5_best.model)
plotResiduals(res.bgcov5_best.model)
check_collinearity(bgcov5_best.model)
check_model(bgcov5_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
bgcov5_top <- subset(bgcov5_set, delta <= 2) %>% 
  filter(!is.na(df)) # not all models converged
for (i in 1:nrow(bgcov5_top)) {
  assign(paste0("bgcov5_model", i), get.models(bgcov5_top, subset = i)[[1]])
}

# Model averaging of top models - cannot average because some models did not converge



## BG cover 6: Drop (1 | Site) --------------------------------------------

# 6: lm version
lm.bgcov6 <- lm(Change_BGCover ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                      Change_HerbCover_scaled + 
                      Prev_year_precip_scaled * PlotSlope_scaled +
                      Prev_year_precip_scaled * Change_ShrubCover_scaled +
                      Prev_year_precip_scaled * Change_HerbCover_scaled,
                    data = plot.change)
summary(lm.bgcov6)
r2(lm.bgcov6) # adjusted: 0.228
check_model(lm.bgcov6)

# 6: glmmTMB version
bgcov6 <- glmmTMB(Change_BGCover ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                    Change_HerbCover_scaled + 
                    Prev_year_precip_scaled * PlotSlope_scaled +
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Prev_year_precip_scaled * Change_HerbCover_scaled,
                  data = plot.change,
                  family = gaussian)
summary(bgcov6)
r2(bgcov6) # adjusted: 0.224
res.bgcov6 <- simulateResiduals(bgcov6)
plotQQunif(res.bgcov6)
plotResiduals(res.bgcov6)
check_collinearity(bgcov6) 
check_model(bgcov6)


### 6: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgcov6_set <- dredge(bgcov6)

# Examine best model
bgcov6_best.model <- get.models(bgcov6_set, 1)[[1]]
summary(bgcov6_best.model)
r2(bgcov6_best.model) # adjusted: 0.220
res.bgcov6_best.model <- simulateResiduals(bgcov6_best.model)
plotQQunif(res.bgcov6_best.model)
plotResiduals(res.bgcov6_best.model)
check_collinearity(bgcov6_best.model)
check_model(bgcov6_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
bgcov6_top <- subset(bgcov6_set, delta <= 2)
for (i in 1:nrow(bgcov6_top)) {
  assign(paste0("bgcov6_model", i), get.models(bgcov6_top, subset = i)[[1]])
}

r2(bgcov6_model1) # adjusted: 0.220
r2(bgcov6_model2) # adjusted: 0.231
r2(bgcov6_model3) # adjusted: 0.227
r2(bgcov6_model4) # adjusted: 0.223
r2(bgcov6_model5) # adjusted: 0.222
r2(bgcov6_model6) # adjusted: 0.224
r2(bgcov6_model7) # adjusted: 0.230

# Model averaging of top models
bgcov6_avg <- model.avg(bgcov6_set, subset = delta <= 2) 
summary(bgcov6_avg)
bgcov6_importance <- sw(bgcov6_avg)
bgcov6_importance.df <- data.frame(variable = names(bgcov6_importance),
                                   importance = bgcov6_importance)
bgcov6_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
bgcov6_avg.all <- model.avg(bgcov6_set)
summary(bgcov6_avg.all)
bgcov6_importance.all <- sw(bgcov6_avg.all)
bgcov6_importance.all.df <- data.frame(variable = names(bgcov6_importance.all),
                                       importance = bgcov6_importance.all)
bgcov6_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()



# BG cover 7: Switch elevation from ft to m -------------------------------

# 7: lm version
lm.bgcov7 <- lm(Change_BGCover ~ Prev_year_precip_scaled + Elevation_m_scaled +
                  Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                  Change_HerbCover_scaled + 
                  Prev_year_precip_scaled * PlotSlope_scaled +
                  Prev_year_precip_scaled * Change_ShrubCover_scaled +
                  Prev_year_precip_scaled * Change_HerbCover_scaled,
                data = plot.change)
summary(lm.bgcov7)
r2(lm.bgcov7) # adjusted: 0.228
check_model(lm.bgcov7)

# 7: glmmTMB version
bgcov7 <- glmmTMB(Change_BGCover ~ Prev_year_precip_scaled + Elevation_m_scaled +
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                    Change_HerbCover_scaled + 
                    Prev_year_precip_scaled * PlotSlope_scaled +
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Prev_year_precip_scaled * Change_HerbCover_scaled,
                  data = plot.change,
                  family = gaussian)
summary(bgcov7)
r2(bgcov7) # adjusted: 0.224
res.bgcov7 <- simulateResiduals(bgcov7)
plotQQunif(res.bgcov7)
plotResiduals(res.bgcov7)
check_collinearity(bgcov7) 
check_model(bgcov7)


### 7: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
bgcov7_set <- dredge(bgcov7)

# Examine best model
bgcov7_best.model <- get.models(bgcov7_set, 1)[[1]]
summary(bgcov7_best.model)
r2(bgcov7_best.model) # adjusted: 0.220
res.bgcov7_best.model <- simulateResiduals(bgcov7_best.model)
plotQQunif(res.bgcov7_best.model)
plotResiduals(res.bgcov7_best.model)
check_collinearity(bgcov7_best.model)
check_model(bgcov7_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
bgcov7_top <- subset(bgcov7_set, delta <= 2)
for (i in 1:nrow(bgcov7_top)) {
  assign(paste0("bgcov7_model", i), get.models(bgcov7_top, subset = i)[[1]])
}

r2(bgcov7_model1) # adjusted: 0.220
r2(bgcov7_model2) # adjusted: 0.231
r2(bgcov7_model3) # adjusted: 0.227
r2(bgcov7_model4) # adjusted: 0.223
r2(bgcov7_model5) # adjusted: 0.222
r2(bgcov7_model7) # adjusted: 0.230
r2(bgcov7_model7) # adjusted: 0.230

# Model averaging of top models
bgcov7_avg <- model.avg(bgcov7_set, subset = delta <= 2) 
summary(bgcov7_avg)
bgcov7_importance <- sw(bgcov7_avg)
bgcov7_importance.df <- data.frame(variable = names(bgcov7_importance),
                                   importance = bgcov7_importance)
bgcov7_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
bgcov7_avg.all <- model.avg(bgcov7_set)
summary(bgcov7_avg.all)
bgcov7_importance.all <- sw(bgcov7_avg.all)
bgcov7_importance.all.df <- data.frame(variable = names(bgcov7_importance.all),
                                       importance = bgcov7_importance.all)
bgcov7_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()



# Survival ----------------------------------------------------------------

## Survival: Linear model v7 equivalent -----------------------------------

# 7: lm version
lm.survival7 <- lm(survival_perc ~ Prev_year_precip_scaled + Elevation_m_scaled +
                  Aspect + PlotSlope_scaled + ShrubCover_scaled +
                  HerbCover_scaled + 
                  Prev_year_precip_scaled * PlotSlope_scaled +
                  Prev_year_precip_scaled * ShrubCover_scaled +
                  Prev_year_precip_scaled * HerbCover_scaled,
                data = dat.survival.plot)
summary(lm.survival7)
r2(lm.survival7) # adjusted: 0.485
check_model(lm.survival7)

# 7: glmmTMB version
survival7 <- glmmTMB(survival_perc ~ Prev_year_precip_scaled + Elevation_m_scaled +
                    Aspect + PlotSlope_scaled + ShrubCover_scaled +
                    HerbCover_scaled + 
                    Prev_year_precip_scaled * PlotSlope_scaled +
                    Prev_year_precip_scaled * ShrubCover_scaled +
                    Prev_year_precip_scaled * HerbCover_scaled,
                  data = dat.survival.plot,
                  family = gaussian)
summary(survival7)
r2(survival7) # adjusted: 0.482
res.survival7 <- simulateResiduals(survival7)
plotQQunif(res.survival7)
plotResiduals(res.survival7)
check_collinearity(survival7) 
check_model(survival7)


### 7: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
survival7_set <- dredge(survival7)

# Examine best model
survival7_best.model <- get.models(survival7_set, 1)[[1]]
summary(survival7_best.model)
r2(survival7_best.model) # adjusted: 0.480
res.survival7_best.model <- simulateResiduals(survival7_best.model)
plotQQunif(res.survival7_best.model)
plotResiduals(res.survival7_best.model)
check_collinearity(survival7_best.model)
check_model(survival7_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
survival7_top <- subset(survival7_set, delta <= 2)
for (i in 1:nrow(survival7_top)) {
  assign(paste0("survival7_model", i), get.models(survival7_top, subset = i)[[1]])
}

r2(survival7_model1) # adjusted: 0.480
r2(survival7_model2) # adjusted: 0.482
r2(survival7_model3) # adjusted: 0.479
r2(survival7_model4) # adjusted: 0.475
r2(survival7_model5) # adjusted: 0.475
r2(survival7_model6) # adjusted: 0.471
r2(survival7_model7) # adjusted: 0.476

# Model averaging of top models
survival7_avg <- model.avg(survival7_set, subset = delta <= 2) 
summary(survival7_avg)
survival7_importance <- sw(survival7_avg)
survival7_importance.df <- data.frame(variable = names(survival7_importance),
                                      importance = survival7_importance)
survival7_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
survival7_avg.all <- model.avg(survival7_set)
summary(survival7_avg.all)
survival7_importance.all <- sw(survival7_avg.all)
survival7_importance.all.df <- data.frame(variable = names(survival7_importance.all),
                                          importance = survival7_importance.all)
survival7_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()


save.image("RData/06.2_linear-models-3.0.RData")
