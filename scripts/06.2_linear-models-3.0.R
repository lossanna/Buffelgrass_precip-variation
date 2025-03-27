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


save.image("RData/06.2_linear-models-3.0.RData")
