# Created: 2025-09-15
# Updated: 2025-09-26

# Purpose: Compile table of basic plot statistics:
# - Number of observations,
# - Averages of actual values by site and year (not year-to-year change values)

library(tidyverse)

# Load data ---------------------------------------------------------------

dat <- read_csv("data/publish/all-data.csv")
culm.change <- read_csv("data/publish/culm-data.csv")
plot.change <- read_csv("data/publish/plot-data.csv")
survival <- read_csv("data/publish/survival-data.csv")

# Calculate number of observations ----------------------------------------

# Number of culm observations
nrow(dat) # 2019

# Number of plot-level observations
dat %>% 
  select(Year, Site, Transect, Plot) %>% 
  distinct(.keep_all = TRUE) %>% 
  nrow() # 207

# Number of survival observations
nrow(survival) # 138

# Number of individual plants tracked
length(unique(dat$Plant_ID)) # 771

# Number of plots
length(unique(dat$Plot)) # 79

# Number of transects
length(unique(dat$Transect)) # 27


# Plot slope --------------------------------------------------------------

summary(filter(dat, Site == "TumamocHill")$PlotSlope) # 4 to 32
summary(filter(dat, Site %in% c("KinneyHill", "ApachePeak"))$PlotSlope) # 9 to 28
summary(filter(dat, Site == "LomaVerde")$PlotSlope) # 12 to 22
summary(dat$PlotSlope) # mean of 17


# Precip (based on NOAA) --------------------------------------------------

# 2020: percent decrease from normal
#   2020 precip: 4.17
#   Average: 11.59
(11.59 - 4.17) / 11.59 # 64%


# Aspects per site --------------------------------------------------------

dat %>% 
  select(Site, Aspect) %>% 
  distinct(.keep_all = TRUE)


# Construct table ---------------------------------------------------------

actual.summary.site.year <- dat %>% 
  group_by(Site, Year) %>% 
  summarise(Prev_year_precip = round(mean(Prev_year_precip), 0),
            TotalCulms_mean = round(mean(TotalCulms), 0),
            TotalCulms_sd = round(sd(TotalCulms), 0),
            ReproductiveCulms_mean = round(mean(ReproductiveCulms), 0),
            ReproductiveCulms_sd = round(sd(ReproductiveCulms), 0),
            BGDensity_mean = round(mean(BGDensity), 1),
            BGDensity_sd = round(sd(BGDensity), 1),
            BGCover_mean = round(mean(BGCover), 0),
            BGCover_sd = round(sd(BGCover), 0),
            ShrubCover_mean = round(mean(ShrubCover), 0),
            ShrubCover_sd = round(sd(ShrubCover), 0),
            HerbCover_mean = round(mean(HerbCover), 1),
            HerbCover_sd = round(sd(HerbCover), 1),
            .groups = "keep") 

actual.summary.year <- dat %>% 
  group_by(Year) %>% 
  summarise(Prev_year_precip = round(mean(Prev_year_precip), 0),
            TotalCulms_mean = round(mean(TotalCulms), 0),
            TotalCulms_sd = round(sd(TotalCulms), 0),
            ReproductiveCulms_mean = round(mean(ReproductiveCulms), 0),
            ReproductiveCulms_sd = round(sd(ReproductiveCulms), 0),
            BGDensity_mean = round(mean(BGDensity), 1),
            BGDensity_sd = round(sd(BGDensity), 1),
            BGCover_mean = round(mean(BGCover), 0),
            BGCover_sd = round(sd(BGCover), 0),
            ShrubCover_mean = round(mean(ShrubCover), 0),
            ShrubCover_sd = round(sd(ShrubCover), 0),
            HerbCover_mean = round(mean(HerbCover), 1),
            HerbCover_sd = round(sd(HerbCover), 1),
            .groups = "keep") 
actual.summary.year2 <- actual.summary.year %>% 
  mutate(TotalCulms = paste(TotalCulms_mean, "±", TotalCulms_sd),
         ReproductiveCulms = paste(ReproductiveCulms_mean, "±", ReproductiveCulms_sd),
         BGDensity = paste(BGDensity_mean, "±", BGDensity_sd),
         BGCover = paste(BGCover_mean, "±", BGCover_sd),
         ShrubCover = paste(ShrubCover_mean, "±", ShrubCover_sd),
         HerbCover = paste(HerbCover_mean, "±", HerbCover_sd)) %>% 
  select(Year, Prev_year_precip, TotalCulms, ReproductiveCulms, BGDensity, BGCover, ShrubCover, HerbCover)

summary(dat$TotalCulms)
summary(dat$ReproductiveCulms)
summary(dat$BGDensity)
summary(dat$BGCover)
summary(dat$ShrubCover)
summary(dat$HerbCover)



# Save --------------------------------------------------------------------

# For supplemental
save(actual.summary.year2,
     file = "RData/publish_actual-summary.RData")
