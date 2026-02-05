# Created: 2026-02-05
# Updated: 2026-02-05

# Purpose: Reformat survival data so it can be analyzed as a binomial GLM.

library(tidyverse)
library(readxl)

# Load data ---------------------------------------------------------------

survival.raw <- read_xlsx("data/raw/2025-04_LO_Buffelgrass-seedling-survival.xlsx", sheet = "survival_R")
dat <- read_csv("data/cleaned/11.1_demography-data_clean.csv")


# Data wrangling ----------------------------------------------------------

# Prepare survival data
survival.join <- survival.raw %>% 
  filter(!is.na(Year)) %>%  # remove observations from 09/2021 (all other obs from spring)
  select(Site, Plot, Year, remaining_toothpicks, seedlings_surviving) %>% 
  filter(seedlings_surviving != "na") %>% 
  mutate(seedlings_surviving = as.numeric(seedlings_surviving))

# Prepare other cols
dat.join <- dat %>% 
  select(Year, Site, Transect, Plot, Prev_year_precip, PlotSlope, Aspect,
         BGDensity, BGCover, ShrubCover, HerbCover) %>% 
  distinct(.keep_all = TRUE)

# Join to get other cols
survival.dat <- survival.join %>% 
  left_join(dat.join) %>% 
  select(Year, Site, Transect, Plot, Prev_year_precip, PlotSlope, Aspect,
         BGDensity, BGCover, ShrubCover, HerbCover,
         remaining_toothpicks, seedlings_surviving) 


# Determine initial conditions
initial <- dat %>% 
  filter(StudyYear == 1) %>% 
  select(Site, Transect, Plot, BGCover, BGDensity, ShrubCover, HerbCover) %>% 
  distinct(.keep_all = TRUE) %>% 
  rename(Init_BGDensity = BGDensity,
         Init_BGCover = BGCover,
         Init_ShrubCover = ShrubCover,
         Init_HerbCover = HerbCover)

# Join with survival data
survival.dat <- survival.dat %>% 
  left_join(initial)



# Write to CSV ------------------------------------------------------------

write_csv(survival.dat,
          file = "data/cleaned/11.2_survival-data_clean.csv") 
  
