# Created: 2024-09-23
# Updated: 2024-09-23

# Purpose: Examine distributions.

library(readxl)
library(tidyverse)
library(GGally)

# Load data ---------------------------------------------------------------

culms.raw <- read_xlsx("data/raw/2024-09_LO_Buffelgrass-culm-demography_master.xlsx", sheet = "Demography")
monitor.prism <- read_csv("data/cleaned/02_monitoring-info-with-PRISM-data_clean.csv")

# Data wrangling ----------------------------------------------------------

dat <- culms.raw |> 
  left_join(monitor.prism)

# Check for NAs
apply(dat, 2, anyNA)

# Separate NA culm counts
na.culm <- dat |> 
  filter(is.na(Vegetative_culms))

# Remove NA culm count - plants could not be located and weren't sampled
dat <- dat |> 
  filter(!is.na(Vegetative_culms))

# Add Perc_dev, Perc_dev_abs, and Deviation_mm cols
dat <- dat |> 
  mutate(Perc_dev = (Prev_year_precip - MAP) / MAP,
         Deviation_mm = Prev_year_precip - MAP) |> 
  mutate(Perc_dev_abs = abs(Perc_dev))

# Change year cols to character
dat <- dat |> 
  mutate(Year = as.character(Year),
         StudyYear = as.character(StudyYear))

# Separate out continuous explanatory variables
exp.cont <- dat |> 
  select(PlotSlope, Elevation_ft, Prev_year_precip, MAT, MAP, Perc_dev, Deviation_mm)

# Separate out response variables
res.cont <- dat |> 
  select(BGCover, BGDensity, ShrubCover, ForbCover, NGCover, Vegetative_culms, Reproductive_culms, Total_Live_Culms,
         Longestleaflength_cm)




# Precipitation variability -----------------------------------------------

summary(dat$Perc_dev) # ranges from -28% to +40%

dat |> 
  ggplot(aes(x = Date, y = Perc_dev)) +
  geom_point()

dat |> 
  ggplot(aes(x = Site, y = Perc_dev)) +
  geom_boxplot() +
  geom_jitter() +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept = 0, linetype = "dashed")


# Response variable: Culm count -------------------------------------------

## Histogram --------------------------------------------------------------

# All, Reproductive
hist(dat$Reproductive_culms)

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
dat |> 
  ggplot(aes(x = Site, y = Reproductive_culms)) +
  geom_boxplot()

#   Total
dat |> 
  ggplot(aes(x = Site, y = Total_Live_Culms)) +
  geom_boxplot()

#   Vegetative
dat |> 
  ggplot(aes(x = Site, y = Vegetative_culms)) +
  geom_boxplot()


# By aspect
dat |> 
  ggplot(aes(x = Aspect, y = Reproductive_culms)) +
  geom_boxplot()


## Pair plot --------------------------------------------------------------

# Reproductive & vegetative culms
dat |>
  ggplot(aes(x = Vegetative_culms, y = Reproductive_culms)) +
  geom_point()


# Response variable: Density & cover --------------------------------------

# Pair plot
res.cont |> 
  select(-Longestleaflength_cm, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms) |> 
  ggpairs()

# BG Cover
hist(dat$BGCover)
dat |> 
  ggplot(aes(x = Site, y = BGCover)) +
  geom_boxplot()

# BG Density
hist(dat$BGDensity)
dat |> 
  ggplot(aes(x = Site, y = BGDensity)) +
  geom_boxplot()

# Shrub cover
hist(dat$ShrubCover)
dat |> 
  ggplot(aes(x = Site, y = ShrubCover)) +
  geom_boxplot()

# Forb cover
hist(dat$ForbCover)
dat |> 
  ggplot(aes(x = Site, y = ForbCover)) +
  geom_boxplot()

# Native grass cover
hist(dat$NGCover)
dat |> 
  ggplot(aes(x = Site, y = NGCover)) +
  geom_boxplot()

# Longest leaf
hist(dat$Longestleaflength_cm)
dat |> 
  filter(!is.na(Longestleaflength_cm)) |> 
  ggplot(aes(x = Site, y = Longestleaflength_cm)) + 
  geom_boxplot()



# Continuous explanatory variables ----------------------------------------

# Pair plot
dat |> 
  select(PlotSlope, Elevation_ft, Prev_year_precip, MAT, MAP, Perc_dev, Deviation_mm) |> 
  distinct(.keep_all = TRUE) |> 
  ggpairs()



# Write out data to csv ---------------------------------------------------

write_csv(dat,
          file = "data/cleaned/04_demography-data_clean.csv")
