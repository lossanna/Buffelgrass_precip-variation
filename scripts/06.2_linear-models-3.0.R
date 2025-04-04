# Created: 2025-03-24
# Updated: 2025-04-03

# Purpose: Run linear models with Change_Reproductive_culms, Change_Total_Live_Culms, 
#   Change_BGDensity, and Change_BGCover as response variable.

# Continuous explanatory variables are centered and scaled. 
# Use (1 | Site / Transect) for culm counts and (1 | Site) for density and cover.

# Updates from 06.1.R script:
#   Response variables refer to change in culm count, density, or cover to better
#     pinpoint precip effects (culm counts are specific to individual).
#   Used model selection to identify important explanatory variables (HPC not needed for
#     these models; they run in a couple of minutes on local machine NMSU-DHJYFZ3).

# Not all models for change in total culms converge during model selection when including  
#   Change_BGDensity*Prev_year_precip interaction, which does not allow for model averaging.
# This is also true for change in BG density and BG cover models.

# Dropped terms in total culm models 6-8 to attempt to achieve convergence on all models
#   and therefore allow model averaging, but this did not work.

library(tidyverse)
library(glmmTMB)
library(performance)
library(DHARMa)
library(performance)
library(lme4)
library(lmerTest)
library(MuMIn)

# Load data ---------------------------------------------------------------

culm.change.raw <- read_csv("data/cleaned/04_change-in-culm-density-cover_clean.csv")

# Data wrangling ----------------------------------------------------------

# Center and scale numeric variables
culm.change <- culm.change.raw %>% 
  mutate(Perc_dev_scaled = scale(Perc_dev, center = TRUE, scale = TRUE)[, ],
         Elevation_ft_scaled = scale(Elevation_ft, center = TRUE, scale = TRUE)[, 1],
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
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1]) %>% 
  filter(Aspect != "flat")


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
  assign(paste0("repro3_model", i), get.models(repro1_top, subset = i)[[1]])
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
  assign(paste0("repro4_model", i), get.models(repro1_top, subset = i)[[1]])
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
  assign(paste0("repro5_model", i), get.models(repro1_top, subset = i)[[1]])
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
  assign(paste0("total3_model", i), get.models(total1_top, subset = i)[[1]])
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
  assign(paste0("total4_model", i), get.models(total1_top, subset = i)[[1]])
} 


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
  assign(paste0("total5_model", i), get.models(total1_top, subset = i)[[1]])
} 


# Model averaging of top models - cannot average because some models did not converge



## Total 6: Drop aspect*slope interaction ---------------------------------

# 6: lme4 version
lme4.total6 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                      Change_BGDensity_scaled +
                      Prev_year_precip_scaled * Change_BGDensity_scaled +
                      Prev_year_precip_scaled * PlotSlope_scaled + 
                      Prev_year_precip_scaled * Change_ShrubCover_scaled +
                      Prev_year_precip_scaled * Change_HerbCover_scaled +
                      (1 | Site / Transect),
                    data = culm.change)
summary(lme4.total6)
r2(lme4.total6) # marginal: 0.200
check_model(lme4.total6)

# 6: glmmTMB version
total6 <- glmmTMB(Change_Total_Live_Culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                    Change_BGDensity_scaled +
                    Prev_year_precip_scaled * Change_BGDensity_scaled +
                    Prev_year_precip_scaled * PlotSlope_scaled + 
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Prev_year_precip_scaled * Change_HerbCover_scaled +
                    (1 | Site / Transect),
                  data = culm.change,
                  family = gaussian) 
summary(total6)
r2(total6) # marginal: 0.225
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
total6_top <- subset(total6_set, delta <= 2) %>% 
  filter(!is.na(df)) # not all models converged
for (i in 1:nrow(total6_top)) {
  assign(paste0("total6_model", i), get.models(total1_top, subset = i)[[1]])
} 

# Model averaging of top models - cannot average because some models did not converge



## Total 7: Drop Aspect ---------------------------------------------------

# 7: lme4 version
lme4.total7 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                      PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                      Change_BGDensity_scaled +
                      Prev_year_precip_scaled * Change_BGDensity_scaled +
                      Prev_year_precip_scaled * PlotSlope_scaled + 
                      Prev_year_precip_scaled * Change_ShrubCover_scaled +
                      Prev_year_precip_scaled * Change_HerbCover_scaled +
                      (1 | Site / Transect),
                    data = culm.change)
summary(lme4.total7)
r2(lme4.total7) # marginal: 0.202
check_model(lme4.total7)

# 7: glmmTMB version
total7 <- glmmTMB(Change_Total_Live_Culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                    PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                    Change_BGDensity_scaled +
                    Prev_year_precip_scaled * Change_BGDensity_scaled +
                    Prev_year_precip_scaled * PlotSlope_scaled + 
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Prev_year_precip_scaled * Change_HerbCover_scaled +
                    (1 | Site / Transect),
                  data = culm.change,
                  family = gaussian) 
summary(total7)
r2(total7) # marginal: 0.237
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
r2(total7_best.model) # marginal: 0.236
res.total7_best.model <- simulateResiduals(total7_best.model)
plotQQunif(res.total7_best.model)
plotResiduals(res.total7_best.model)
check_model(total7_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
total7_top <- subset(total7_set, delta <= 2) %>% 
  filter(!is.na(df)) # not all models converged
for (i in 1:nrow(total7_top)) {
  assign(paste0("total7_model", i), get.models(total1_top, subset = i)[[1]])
} 

# Model averaging of top models - cannot average because some models did not converge



## Total 8: Drop precip*slope interaction ---------------------------------

# 8: lme4 version
lme4.total8 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                      Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                      Change_BGDensity_scaled +
                      Prev_year_precip_scaled * Change_BGDensity_scaled +
                      Prev_year_precip_scaled * Change_ShrubCover_scaled +
                      Prev_year_precip_scaled * Change_HerbCover_scaled +
                      (1 | Site / Transect),
                    data = culm.change)
summary(lme4.total8)
r2(lme4.total8) # marginal: 0.157
check_model(lme4.total8)

# 8: glmmTMB version
total8 <- glmmTMB(Change_Total_Live_Culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                    Change_BGDensity_scaled +
                    Prev_year_precip_scaled * Change_BGDensity_scaled +
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Prev_year_precip_scaled * Change_HerbCover_scaled +
                    (1 | Site / Transect),
                  data = culm.change,
                  family = gaussian) 
summary(total8)
r2(total8) # marginal: 0.177
res.total8 <- simulateResiduals(total8)
plotQQunif(res.total8)
plotResiduals(res.total8)
check_collinearity(total8)
check_model(total8)


### 8: Model selection ----------------------------------------------------

# Model selection
options(na.action = "na.fail")
total8_set <- dredge(total8)

 # Examine best model
total8_best.model <- get.models(total8_set, 1)[[1]]
summary(total8_best.model)
r2(total8_best.model) # marginal: 0.135
res.total8_best.model <- simulateResiduals(total8_best.model)
plotQQunif(res.total8_best.model)
plotResiduals(res.total8_best.model)
check_model(total8_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
total8_top <- subset(total8_set, delta <= 2) %>% 
  filter(!is.na(df)) # not all models converged
for (i in 1:nrow(total8_top)) {
  assign(paste0("total8_model", i), get.models(total1_top, subset = i)[[1]])
} 

# Model averaging of top models - cannot average because some models did not converge
#   also not sure why it created a bunch of null models




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
  assign(paste0("bgden3_model", i), get.models(bgden1_top, subset = i)[[1]])
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
  assign(paste0("bgden4_model", i), get.models(bgden1_top, subset = i)[[1]])
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
  assign(paste0("bgden5_model", i), get.models(bgden1_top, subset = i)[[1]])
}

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
  assign(paste0("bgcov3_model", i), get.models(bgcov1_top, subset = i)[[1]])
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
  assign(paste0("bgcov4_model", i), get.models(bgcov1_top, subset = i)[[1]])
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
  assign(paste0("bgcov5_model", i), get.models(bgcov1_top, subset = i)[[1]])
}

# Model averaging of top models - cannot average because some models did not converge




save.image("RData/06.2_linear-models-3.0.RData")
