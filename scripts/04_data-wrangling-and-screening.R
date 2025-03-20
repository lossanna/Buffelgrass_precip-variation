# Created: 2024-09-23
# Updated: 2025-03-20

# Purpose: Examine distributions; write out clean data with precip deviation variable added.

# Culm counts have a Poisson distribution. Buffelgrass density & cover should be log-transformed to improve
#   normality. Native grass, native forb, and herb (forb + grass) roughly have a Poisson distribution,
#   with a lot of 0s.

# MAT correlated with precip-related variables.

library(readxl)
library(tidyverse)
library(GGally)

# Load data ---------------------------------------------------------------

culms.raw <- read_xlsx("data/raw/2024-09_LO_Buffelgrass-culm-demography_master.xlsx", sheet = "Demography")
monitor.prism <- read_csv("data/cleaned/02_monitoring-info-with-PRISM-data_clean.csv")

# Data wrangling ----------------------------------------------------------

dat <- culms.raw %>% 
  left_join(monitor.prism)

# Check for NAs
apply(dat, 2, anyNA)

# Separate NA culm counts
na.culm <- dat %>% 
  filter(is.na(Vegetative_culms))

# Remove NA culm count - plants could not be located and weren't sampled
dat <- dat %>% 
  filter(!is.na(Vegetative_culms))

# Add Perc_dev and Deviation_mm cols
dat <- dat %>% 
  mutate(Perc_dev = (Prev_year_precip - MAP) / MAP,
         Deviation_mm = Prev_year_precip - MAP) 

# Change year cols to character
dat <- dat %>% 
  mutate(Year = as.character(Year),
         StudyYear = as.character(StudyYear))

# Add HerbCover col
dat <- dat %>% 
  mutate(HerbCover = NGCover + ForbCover)

# Separate out continuous explanatory variables
exp.cont <- dat %>% 
  select(PlotSlope, Elevation_ft, Prev_year_precip, MAT, MAP, Perc_dev, Deviation_mm)

# Separate out response variables
res.cont <- dat %>% 
  select(BGCover, BGDensity, ShrubCover, ForbCover, NGCover, HerbCover,
         Vegetative_culms, Reproductive_culms, Total_Live_Culms,
         Longestleaflength_cm)




# Precipitation variability -----------------------------------------------

summary(dat$Perc_dev) # ranges from -28% to +40%

dat %>% 
  ggplot(aes(x = Date, y = Perc_dev)) +
  geom_point()

dat %>% 
  ggplot(aes(x = Site, y = Perc_dev)) +
  geom_boxplot() +
  geom_jitter() +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept = 0, linetype = "dashed")


# Response variable: Culm count -------------------------------------------

## Histogram --------------------------------------------------------------

# All, Reproductive
hist(dat$Reproductive_culms) # Poisson

# By aspect, Reproductive
hist(filter(dat, Aspect == "S")$Reproductive_culms)
hist(filter(dat, Aspect == "SW")$Reproductive_culms)
hist(filter(dat, Aspect == "E")$Reproductive_culms)
hist(filter(dat, Aspect == "W")$Reproductive_culms)
hist(filter(dat, Aspect == "N")$Reproductive_culms)

# By site, Reproductive
hist(filter(dat, Site == "TumamocHill")$Reproductive_culms)
hist(filter(dat, Site == "LomaVerde")$Reproductive_culms)
hist(filter(dat, Site == "KinneyHill")$Reproductive_culms)
hist(filter(dat, Site == "ApachePeak")$Reproductive_culms)

# All, Total
hist(dat$Total_Live_Culms)

# All, Vegetative
hist(dat$Vegetative_culms)


## Boxplot ----------------------------------------------------------------

# All, by site
#   Reproductive
dat %>% 
  ggplot(aes(x = Site, y = Reproductive_culms)) +
  geom_boxplot()

#   Total
dat %>% 
  ggplot(aes(x = Site, y = Total_Live_Culms)) +
  geom_boxplot()

#   Vegetative
dat %>% 
  ggplot(aes(x = Site, y = Vegetative_culms)) +
  geom_boxplot()


# All, by aspect
#   Reproductive
dat %>% 
  ggplot(aes(x = Aspect, y = Reproductive_culms)) +
  geom_boxplot()

#   Total
dat %>% 
  ggplot(aes(x = Aspect, y = Total_Live_Culms)) +
  geom_boxplot()

#   Vegetative
dat %>% 
  ggplot(aes(x = Aspect, y = Vegetative_culms)) +
  geom_boxplot()


## Pair plot --------------------------------------------------------------

# Reproductive & vegetative culms
dat %>%
  ggplot(aes(x = Vegetative_culms, y = Reproductive_culms)) +
  geom_point()



# Response variable: BG density & cover -----------------------------------

# Pair plot
res.cont %>% 
  select(-Longestleaflength_cm, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms) %>% 
  ggpairs()

# BG Cover
hist(dat$BGCover)
dat %>% 
  ggplot(aes(x = Site, y = BGCover)) +
  geom_boxplot()

#   Log transformation
dat <- dat %>% 
  mutate(BGCover_log = log(BGCover))
hist(dat$BGCover_log) # log transformation kind of improves normality?

# BG Density
hist(dat$BGDensity)
dat %>% 
  ggplot(aes(x = Site, y = BGDensity)) +
  geom_boxplot()

#   Log transformation
dat <- dat %>% 
  mutate(BGDensity_log = log(BGDensity))
hist(dat$BGDensity_log) # log transformation improves normality


# Longest leaf
hist(dat$Longestleaflength_cm)
dat %>% 
  filter(!is.na(Longestleaflength_cm)) %>% 
  ggplot(aes(x = Site, y = Longestleaflength_cm)) + 
  geom_boxplot()



# Response variable: Native plant cover -----------------------------------

# Shrub cover
hist(dat$ShrubCover)
dat %>% 
  ggplot(aes(x = Site, y = ShrubCover)) +
  geom_boxplot()
dat %>% 
  ggplot(aes(x = Aspect, y = ShrubCover)) +
  geom_boxplot()

# Forb cover
hist(dat$ForbCover)
dat %>% 
  ggplot(aes(x = Site, y = ForbCover)) +
  geom_boxplot()
dat %>% 
  ggplot(aes(x = Aspect, y = ForbCover)) +
  geom_boxplot()

# Native grass cover
hist(dat$NGCover)
dat %>% 
  ggplot(aes(x = Site, y = NGCover)) +
  geom_boxplot()
dat %>% 
  ggplot(aes(x = Aspect, y = NGCover)) +
  geom_boxplot()

# Herb cover
hist(dat$HerbCover)
dat %>% 
  ggplot(aes(x = Site, y = HerbCover)) +
  geom_boxplot()
dat %>% 
  ggplot(aes(x = Aspect, y = HerbCover)) +
  geom_boxplot()


# Continuous explanatory variables ----------------------------------------

# Summary
summary(dat$Elevation_ft)
summary(dat$PlotSlope)
summary(dat$MAT)

# Distribution
hist(dat$Elevation_ft)
hist(dat$PlotSlope)
hist(dat$MAT)

# Pair plot
dat %>% 
  select(PlotSlope, Elevation_ft, Prev_year_precip, MAT, MAP, Perc_dev, Deviation_mm) %>% 
  distinct(.keep_all = TRUE) %>% 
  ggpairs()



# Create response variable: change in total culm count --------------------

culm.change <- dat %>%
  arrange(Plant_ID, Year) %>%
  group_by(Plant_ID) %>%
  mutate(Change_Reproductive_culms = Reproductive_culms - lag(Reproductive_culms),
         Change_Total_Live_Culms = Total_Live_Culms - lag(Total_Live_Culms)) %>% 
  mutate(Change_BGDensity = BGDensity - lag(BGDensity),
         Change_BGCover = BGCover - lag(BGCover)) %>% 
  mutate(Change_HerbCover = HerbCover - lag(HerbCover),
         Change_ShrubCover = ShrubCover - lag(ShrubCover)) %>% 
  filter(!is.na(Change_Total_Live_Culms))

hist(culm.change$Change_Reproductive_culms, breaks = 20)
hist(culm.change$Change_Total_Live_Culms, breaks = 20)
hist(culm.change$Change_BGDensity, breaks = 20)
hist(culm.change$Change_BGCover, breaks = 20)



# Write out data to csv ---------------------------------------------------

write_csv(dat,
          file = "data/cleaned/04_demography-data_clean.csv")

write_csv(culm.change,
          file = "data/cleaned/04_change-in-culm-density-cover_clean.csv")


save.image("RData/04_data-wrangling-and-screening.RData")
