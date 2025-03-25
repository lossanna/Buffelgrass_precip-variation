# Created: 2025-03-24
# Updated: 2025-03-25

# Purpose: Run linear models with Change_Reproductive_culms, Change_Total_Live_Culms, 
#   Change_BGDensity, and Change_BGCover as response variable.

# Continuous explanatory variables are centered and scaled. 

# Updates from 06.1.R script:
#   Response variables refer to change in culm count, density, or cover to better
#     pinpoint precip effects (culm counts are specific to individual).
#   Used model selection to identify important explanatory variables (HPC not needed for
#     these models; they run in a couple of minutes on local machine NMSU-DHJYFZ3).


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

## 1: All variables, no interactions --------------------------------------

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
repro1_best_model <- get.models(repro1_set, 1)[[1]]
summary(repro1_best_model)
r2(repro1_best_model) # marginal: 0.091
check_model(repro1_best_model)

# Examine models within 2 AICc units of best and assign each top model to separate object
repro1_top <- subset(repro1_set, delta <= 2)
for (i in 1:nrow(repro1_top)) {
  assign(paste0("repro1_model", i), get.models(repro1_top, subset = i)[[1]])
}

# Model averaging of top models
repro1_avg <- model.avg(repro1_set, delta <= 2)
summary(repro1_avg)
sw(repro1_avg)


## 2: Add precip interactions ---------------------------------------------

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


## 3: Change precip*aspect to precip*density ------------------------------

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
repro3_best_model <- get.models(repro3_set, 1)[[1]]
summary(repro3_best_model)
r2(repro3_best_model) # marginal: 0.125

# Examine models within 2 AICc units of best and assign each top model to separate object
repro3_top <- subset(repro3_set, delta <= 2)
for (i in 1:nrow(repro3_top)) {
  assign(paste0("repro3_model", i), get.models(repro1_top, subset = i)[[1]])
}

# Model averaging of top models
repro3_avg <- model.avg(repro3_set, subset = delta <= 2)
summary(repro3_avg)
sw(repro3_avg)


save.image("RData/06.2_linear-models-3.0.RData")
