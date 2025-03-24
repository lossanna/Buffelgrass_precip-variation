# Created: 2025-03-24
# Updated: 2025-03-24

# Purpose: Run linear models with Change_Reproductive_culms and 
#   Change_Total_Live_Culms, 
#   Change_BGDensity, and Change_BGCover as response variable.

# Continuous explanatory variables are centered and scaled. 

# Updates from 06.1.R script:
#   Response variables refer to change in culm count, density, or cover to better
#     pinpoint precip effects (culm counts are specific to individual).


library(tidyverse)
library(glmmTMB)
library(performance)
library(DHARMa)
library(nlme)
library(performance)
library(lme4)
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

# 1: With Prev_year_precip
nlme.repro1 <- lme(Change_Reproductive_culms ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                     Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                     Change_HerbCover_scaled,
                   random = ~ 1 | Site / Transect,
                   data = culm.change)
summary(nlme.repro1)
r2(nlme.repro1)
check_model(nlme.repro1)

gtb.repro1 <- glmmTMB(Change_Reproductive_culms ~ Prev_year_precip_scaled + Elevation_ft_scaled +
                            Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                            Change_HerbCover_scaled + Change_BGDensity_scaled + (1 | Site / Transect),
                          data = culm.change,
                          family = gaussian)
summary(gtb.repro1)
r2(gtb.repro1)
res.gtb.repro1 <- simulateResiduals(gtb.repro1)
plotQQunif(res.gtb.repro1)
plotResiduals(res.gtb.repro1)
check_collinearity(gtb.repro1) 

#   1: Model selection
options(na.action = "na.fail")
gtb.repro1_set <- dredge(gtb.repro1)
gtb.repro1_best_model <- get.models(gtb.repro1_set, 1)[[1]]
summary(gtb.repro1_best_model)


# 2: With Prev_year_precip, add precip interactions
gtb.repro2 <- glmmTMB(Change_Reproductive_culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                        Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                        Change_BGDensity_scaled +
                        Prev_year_precip_scaled * Aspect +
                        Prev_year_precip_scaled * PlotSlope_scaled + 
                        Aspect * PlotSlope_scaled +
                        (1 | Site / Transect),
                      data = culm.change,
                      family = gaussian) 
summary(gtb.repro2)
r2(gtb.repro2)
res.gtb.repro2 <- simulateResiduals(gtb.repro2)
plotQQunif(res.gtb.repro2)
plotResiduals(res.gtb.repro2)
check_collinearity(gtb.repro2) # Prev_year_precip & Aspect highly correlated


# 3: Change precip*aspect interaction for precip*density
gtb.repro3 <- glmmTMB(Change_Reproductive_culms ~ Prev_year_precip_scaled + Elevation_ft_scaled + 
                        Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                        Change_BGDensity_scaled +
                        Prev_year_precip_scaled * Change_BGDensity_scaled +
                        Prev_year_precip_scaled * PlotSlope_scaled + 
                        Aspect * PlotSlope_scaled +
                        (1 | Site / Transect),
                      data = culm.change,
                      family = gaussian) 
summary(gtb.repro3)
r2(gtb.repro3)
res.gtb.repro3 <- simulateResiduals(gtb.repro3)
plotQQunif(res.gtb.repro3)
plotResiduals(res.gtb.repro3)
check_collinearity(gtb.repro3)

#   3: Model selection
options(na.action = "na.fail")
gtb.repro3_set <- dredge(gtb.repro3)
gtb.repro3_best_model <- get.models(gtb.repro3_set, 1)[[1]]
summary(gtb.repro3_best_model)
