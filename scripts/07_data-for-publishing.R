# Created: 2025-08-01
# Updated: 2025-08-01

# Purpose: Create data for publishing.

library(tidyverse)

# Load data ---------------------------------------------------------------

dat <- read_csv("data/cleaned/04_demography-data_clean.csv")
culm.change.raw <- read_csv("data/cleaned/04_change-in-culm-density-cover_clean.csv")
prism.dat.raw <- read_csv("data/cleaned/02_monitoring-info-with-PRISM-data_clean.csv")

# Data wrangling ----------------------------------------------------------

# Site info
prism.dat <- prism.dat.raw %>% 
  select(Site, Latitude, Longitude, Elevation_ft, MAP, MAT) %>% 
  distinct(.keep_all = TRUE)

# Culm change
culm.change <- culm.change.raw %>% 
  select(Year, Site, Transect, Plot, Plant_ID, Aspect, PlotSlope, Prev_year_precip,
         Change_Reproductive_culms, Change_Total_Live_Culms, Change_BGDensity, Change_BGCover,
         Change_ShrubCover, Change_HerbCover) %>% 
  filter(Aspect != "flat") %>% 
  rename(Change_ReproductiveCulms = Change_Reproductive_culms,
         Change_TotalCulms = Change_Total_Live_Culms)

# Plot change
plot.change <- culm.change %>% 
  select(Year, Site, Transect, Plot, Aspect, PlotSlope, Prev_year_precip,
         Change_BGDensity, Change_BGCover, Change_ShrubCover, Change_HerbCover) %>% 
  distinct(.keep_all = TRUE) 

# Survival
dat.survival <- dat %>% 
  filter(!is.na(survival_perc),
         Aspect != "flat") %>% 
  select(Year, Site, Transect, Plot, Aspect, PlotSlope, Prev_year_precip,
         survival_perc, BGDensity, BGCover, ShrubCover, HerbCover) %>% 
  distinct(.keep_all = TRUE)
