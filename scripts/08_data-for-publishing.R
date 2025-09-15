# Created: 2025-08-01
# Updated: 2025-09-15

# Purpose: Create data for publishing.

library(tidyverse)

# Load data ---------------------------------------------------------------

dat.raw <- read_csv("data/cleaned/04_demography-data_clean.csv")
culm.change.raw <- read_csv("data/cleaned/04_change-in-culm-density-cover_clean.csv")
prism.dat.raw <- read_csv("data/cleaned/02_monitoring-info-with-PRISM-data_clean.csv")

# Data wrangling ----------------------------------------------------------

# Site info
prism.dat <- prism.dat.raw %>% 
  select(Site, Latitude, Longitude, Elevation_ft, MAP, MAT) %>% 
  distinct(.keep_all = TRUE)

# Actual values
dat <- dat.raw %>% 
  select(Year, Site, Transect, Plot, Plant_ID, Aspect, PlotSlope, Prev_year_precip,
         Reproductive_culms, Total_Live_Culms, BGDensity, BGCover, ShrubCover, HerbCover) %>% 
  filter(Aspect != "flat") %>% 
  rename(ReproductiveCulms = Reproductive_culms,
         TotalCulms = Total_Live_Culms)

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
         survival_perc, BGDensity, ShrubCover, HerbCover) %>% 
  distinct(.keep_all = TRUE) %>% 
  rename(Survival_perc = survival_perc)


# Check IDs ---------------------------------------------------------------

# Number of individual plants tracked - all
length(unique(dat$Plant_ID)) # 841

# Number of individual plants tracked - year-to-year change
length(unique(culm.change$Plant_ID)) # 771

# Plots not included in year-to-year change
unique(setdiff(dat$Plant_ID, culm.change$Plant_ID))
plant.missing <- dat %>% 
  filter(Plant_ID %in% c(unique(setdiff(dat$Plant_ID, culm.change$Plant_ID))))

# Number of plots - all
length(unique(dat$Plot)) # 79

# Number of plots - year-to-year change
length(unique(culm.change$Plot)) # 79

# Note that some plants were not included in year-to-year change, but all plots were
#   represented in both

# Remove plants not included in year-to-year change from dat
dat <- dat %>% 
  filter(!Plant_ID %in% c(unique(setdiff(dat$Plant_ID, culm.change$Plant_ID))))


# Write to csv ------------------------------------------------------------

write_csv(prism.dat,
          file = "data/publish/site-info.csv")
write_csv(dat,
          file = "data/publish/all-data.csv")
write_csv(culm.change,
          file = "data/publish/culm-data.csv")
write_csv(plot.change,
          file = "data/publish/plot-data.csv")
write_csv(dat.survival,
          file = "data/publish/survival-data.csv")
