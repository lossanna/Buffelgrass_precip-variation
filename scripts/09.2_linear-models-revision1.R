# Created: 2026-01-28
# Updated: 2026-01-28

# Purpose: Run models without model selection via AICc so p-values can be used instead.


library(tidyverse)
library(glmmTMB)
library(performance)
library(DHARMa)
library(lme4)
library(lmerTest)
library(modelbased)

# Load data ---------------------------------------------------------------

dat <- read_csv("data/cleaned/04_demography-data_clean.csv")
culm.change.raw <- read_csv("data/cleaned/04_change-in-culm-density-cover_clean.csv")

# Data wrangling ----------------------------------------------------------

# Center and scale numeric variables
culm.change.flat.rm <- culm.change.raw %>% 
  filter(Aspect != "flat") %>% 
  mutate(PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         Change_BGDensity_scaled = scale(Change_BGDensity, scale = TRUE)[, 1],
         Change_ShrubCover_scaled = scale(Change_ShrubCover, scale = TRUE)[, 1],
         Change_HerbCover_scaled = scale(Change_HerbCover, scale = TRUE)[, 1],
         Change_BGCover_scaled = scale(Change_BGCover, scale = TRUE)[, 1])

# Separate out plot-level data
plot.change <- culm.change.raw %>% 
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm,
         -Change_Reproductive_culms, -Change_Total_Live_Culms, -Notes) %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(Aspect != "flat") %>% 
  mutate(PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         Change_ShrubCover_scaled = scale(Change_ShrubCover, scale = TRUE)[, 1],
         Change_HerbCover_scaled = scale(Change_HerbCover, scale = TRUE)[, 1],
         Change_BGCover_scaled = scale(Change_BGCover, scale = TRUE)[, 1])


# Prepare survival data
dat.survival <- dat %>% 
  filter(!is.na(survival_perc),
         Aspect != "flat") %>% 
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm,
         -Notes) %>% 
  distinct(.keep_all = TRUE)

dat.survival <- dat.survival %>% 
  mutate(Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         ShrubCover_scaled = scale(ShrubCover, center = TRUE, scale = TRUE)[, 1],
         HerbCover_scaled = scale(HerbCover, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         BGDensity_scaled = scale(BGDensity, center = TRUE, scale = TRUE)[, 1])

#   Transform 0s and 1s
dat.survival <- dat.survival %>% 
  mutate(survival_transf = pmin(pmax(survival_perc, 1e-6), 1 - 1e-6))



# Total culm change -------------------------------------------------------

# Version 1: Transect
total1 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Transect),
              data = culm.change.flat.rm)
summary(total1)
r2(total1) # marginal: 0.144; conditional: 0.391
res.total1 <- simulateResiduals(total1)
plotQQunif(res.total1)
plotResiduals(res.total1) 
check_model(total1) 


# Version 2: Nested Site/Transect
total2 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Site / Transect),
               data = culm.change.flat.rm)
summary(total2)
r2(total2) # marginal: 0.119; conditional: 0.410
res.total2 <- simulateResiduals(total2)
plotQQunif(res.total2)
plotResiduals(res.total2) 
check_model(total2) 


# Version 3: Nested Site/Transect/Plant_ID
total3 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Site / Transect / Plant_ID),
               data = culm.change.flat.rm)
summary(total3)
r3(total3) # marginal: 0.119; conditional: 0.410
res.total3 <- simulateResiduals(total3)
plotQQunif(res.total3)
plotResiduals(res.total3) 
check_model(total3) 
