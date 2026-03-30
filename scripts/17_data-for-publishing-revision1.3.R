# Created: 2026-03-30
# Updated: 2026-03-30

# Purpose: Create data for publishing, version revision1.3.

library(tidyverse)

# Load data ---------------------------------------------------------------

dat.raw <- read_csv("data/cleaned/11.1_demography-data_clean.csv")
culm.change.raw <- read_csv("data/cleaned/11.1_change-in-culm-density-cover_clean.csv")
prism.dat.raw <- read_csv("data/cleaned/02_monitoring-info-with-PRISM-data_clean.csv")
dat.survival.raw <- read_csv("data/cleaned/11.2_survival-data_clean.csv")

# Data wrangling ----------------------------------------------------------

# Site info
prism.dat <- prism.dat.raw %>% 
  select(Site, Latitude, Longitude, Elevation_ft, MAP, MAT) %>% 
  distinct(.keep_all = TRUE)

# Actual values
dat <- dat.raw %>% 
  select(Year, Site, Transect, Plot, Plant_ID, PlotSlope, Prev_year_precip, Perc_dev, MAP,
         Total_Live_Culms, Reproductive_culms, BGDensity, BGCover, ShrubCover, HerbCover) %>% 
  rename(ReproductiveCulms = Reproductive_culms,
         TotalCulms = Total_Live_Culms)

# Culm change
culm.change <- culm.change.raw %>% 
  select(Year, Site, Transect, Plot, Plant_ID, PlotSlope, Prev_year_precip,
         Change_Total_Live_Culms, Change_Reproductive_culms, Change_BGDensity, Change_BGCover,
         Change_ShrubCover, Change_HerbCover, Init_BGDensity, Init_BGCover,
         Init_ShrubCover, Init_HerbCover) %>% 
  rename(Change_ReproductiveCulms = Change_Reproductive_culms,
         Change_TotalCulms = Change_Total_Live_Culms)

# Plot change
plot.change <- culm.change %>% 
  select(Year, Site, Transect, Plot, PlotSlope, Prev_year_precip,
         Change_BGDensity, Change_BGCover, Change_ShrubCover, Change_HerbCover,
         Init_BGDensity, Init_BGCover, Init_ShrubCover, Init_HerbCover) %>% 
  distinct(.keep_all = TRUE) 

# Survival
dat.survival <- dat.survival.raw %>% 
  mutate(survival_prop = seedlings_surviving / remaining_toothpicks)


# Check IDs ---------------------------------------------------------------

# Number of individual plants tracked - all
length(unique(dat$Plant_ID)) # 851

# Number of individual plants tracked - year-to-year change
length(unique(culm.change$Plant_ID)) # 782

# Plots not included in year-to-year change
unique(setdiff(dat$Plant_ID, culm.change$Plant_ID))
plant.missing <- dat %>% 
  filter(Plant_ID %in% c(unique(setdiff(dat$Plant_ID, culm.change$Plant_ID))))

# Number of plots - all
length(unique(dat$Plot)) # 80

# Number of plots - year-to-year change
length(unique(culm.change$Plot)) # 80

# Number of transects
length(unique(culm.change$Transect)) # 27

# Note that some plants were not included in year-to-year change, but all plots were
#   represented in both

# Remove plants not included in year-to-year change from dat
dat <- dat %>% 
  filter(!Plant_ID %in% c(unique(setdiff(dat$Plant_ID, culm.change$Plant_ID))))


# Write to csv ------------------------------------------------------------

write_csv(prism.dat,
          file = "data/publish2.3/site-info.csv")
write_csv(dat,
          file = "data/publish2.3/all-data.csv")
write_csv(culm.change,
          file = "data/publish2.3/culm-data.csv")
write_csv(plot.change,
          file = "data/publish2.3/plot-data.csv")
write_csv(dat.survival,
          file = "data/publish2.3/survival-data.csv")
