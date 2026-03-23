# Created: 2026-03-23
# Updated: 2026-03-23

# Purpose: Rerun models and include initial BG density, shrub cover, and herb cover values
#   as explanatory variables (v1.2). Also add Aspect/Slope * precip interactions.

# Result: Aspect is too highly correlated with precip. Version 2 has kind of funky residuals
#   compared to v1.2, so I will stick with v1.2.

library(tidyverse)
library(performance)
library(DHARMa)
library(lme4)
library(lmerTest)
library(modelbased)

# Load data ---------------------------------------------------------------

dat <- read_csv("data/cleaned/11.1_demography-data_clean.csv")
culm.change.raw <- read_csv("data/cleaned/11.1_change-in-culm-density-cover_clean.csv")
survival.dat <- read_csv("data/cleaned/11.2_survival-data_clean.csv")

# Data wrangling ----------------------------------------------------------

# Center and scale numeric variables for culm change data
culm.change.flat.rm <- culm.change.raw %>% 
  filter(Aspect != "flat") %>% 
  mutate(PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         Change_BGDensity_scaled = scale(Change_BGDensity, scale = TRUE)[, 1],
         Change_ShrubCover_scaled = scale(Change_ShrubCover, scale = TRUE)[, 1],
         Change_HerbCover_scaled = scale(Change_HerbCover, scale = TRUE)[, 1],
         Change_BGCover_scaled = scale(Change_BGCover, scale = TRUE)[, 1],
         Init_BGDensity_scaled = scale(Init_BGDensity, scale = TRUE)[, 1],
         Init_ShrubCover_scaled = scale(Init_ShrubCover, scale = TRUE)[, 1],
         Init_HerbCover_scaled = scale(Init_HerbCover, scale = TRUE)[, 1],
         Init_BGCover_scaled = scale(Init_BGCover, scale = TRUE)[, 1])

# Center and scale numeric variables for plot-level data
plot.change <- culm.change.raw %>% 
  select(-Plant_ID, -Change_Reproductive_culms, -Change_Total_Live_Culms) %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(Aspect != "flat") %>% 
  mutate(PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         Change_ShrubCover_scaled = scale(Change_ShrubCover, scale = TRUE)[, 1],
         Change_HerbCover_scaled = scale(Change_HerbCover, scale = TRUE)[, 1],
         Change_BGCover_scaled = scale(Change_BGCover, scale = TRUE)[, 1],
         Init_BGDensity_scaled = scale(Init_BGDensity, scale = TRUE)[, 1],
         Init_ShrubCover_scaled = scale(Init_ShrubCover, scale = TRUE)[, 1],
         Init_HerbCover_scaled = scale(Init_HerbCover, scale = TRUE)[, 1],
         Init_BGCover_scaled = scale(Init_BGCover, scale = TRUE)[, 1])


# Center and scale numeric variables for survival data
dat.survival <- survival.dat %>% 
  filter(Aspect != "flat") %>% 
  mutate(Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         ShrubCover_scaled = scale(ShrubCover, center = TRUE, scale = TRUE)[, 1],
         HerbCover_scaled = scale(HerbCover, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         BGDensity_scaled = scale(BGDensity, center = TRUE, scale = TRUE)[, 1]) %>% 
  mutate(survival_prop = seedlings_surviving / remaining_toothpicks)


# Total culm change -------------------------------------------------------

# Version 1: Add Aspect/Slope * precip interactions
total1 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_BGDensity_scaled +
                 Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 Prev_year_precip_scaled * Aspect +
                 Prev_year_precip_scaled * PlotSlope_scaled +
                 (1 | Site / Transect),
               data = culm.change.flat.rm)
summary(total1)
r2(total1) # marginal: 0.203; conditional: 0.471
res.total1 <- simulateResiduals(total1)
plotQQunif(res.total1)
plotResiduals(res.total1) 
check_collinearity(total1) # have to drop Aspect * precip due to high VIF


# Version 2: Initial conditions as numeric, nested Site/Transect/Plant_ID
total2 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_BGDensity_scaled +
                 Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 Prev_year_precip_scaled * PlotSlope_scaled +
                 (1 | Site / Transect / Plant_ID),
               data = culm.change.flat.rm)
summary(total2)
r2(total2) # marginal: 0.137; conditional: 0.456
res.total2 <- simulateResiduals(total2)
plotQQunif(res.total2)
plotResiduals(res.total2) 

