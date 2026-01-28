# Created: 2026-01-28
# Updated: 2026-01-28

# Purpose: 

library(tidyverse)
library(insight)
library(modelbased)
library(ggeffects)
library(viridis)
library(ggpubr)

# Load data ---------------------------------------------------------------

dat <- read_csv("data/cleaned/04_demography-data_clean.csv")
zip.total6 <- load("RData/09_zip.total.6.RData")

# Data wrangling ----------------------------------------------------------

# Need 3 datasets:
#   1. Data for graphing observed data
#   2. Data to construct datagrid with predictions & CI
#   3. Data to construct datagrid with unscaled explanatory variable (to be added to #2)


# 1. Dataset for graphing (response and unscaled explanatory variables)
#   Culm - total & repro
dat.culm <- dat %>% 
  select(Total_Live_Culms, Reproductive_culms, 
         Prev_year_precip, Aspect, PlotSlope, BGDensity, ShrubCover, HerbCover) %>% 
  mutate(PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         ShrubCover_scaled = scale(ShrubCover, center = TRUE, scale = TRUE)[, 1],
         HerbCover_scaled = scale(HerbCover, center = TRUE, scale = TRUE)[, 1],
         BGDensity_scaled = scale(BGDensity, center = TRUE, scale = TRUE)[, 1]) %>% 
  filter(Aspect != "flat")

#   Plot - density & cover
dat.plot <- dat %>% 
  select(BGDensity, BGCover, 
         Prev_year_precip, Aspect, PlotSlope, ShrubCover, HerbCover)


# 2. Dataset for constructing datagrid with prediction & CI (scaled explanatory variables only)
#   Culm - total & repro 
dat.culm.ex <- dat.culm %>% 
  select(Prev_year_precip_scaled, Aspect, PlotSlope_scaled, 
         BGDensity_scaled, ShrubCover_scaled, HerbCover_scaled)

#   Plot - density & cover
dat.plot.ex <- dat.culm %>% 
  select(Prev_year_precip_scaled,
         Aspect, PlotSlope_scaled, ShrubCover_scaled, HerbCover_scaled)


# 3. Dataset for constructing datagrid with unscaled variables to match graph (unscaled explanatory variables only)
#   Culm total & repro
dat.culm.unscaled <- dat %>% 
  select(Prev_year_precip, Aspect, PlotSlope, BGDensity, ShrubCover, HerbCover) %>% 
  filter(Aspect != "flat")

#   Plot - density & cover
dat.plot.unscaled <- dat %>% 
  select(Prev_year_precip, Aspect, PlotSlope, ShrubCover, HerbCover) %>% 
  filter(Aspect != "flat")



# Test different precip levels --------------------------------------------

# Am trying to generate a datagrid with a precip level that is closest to 0 (the mean),
#   so I can then have the highest, mean, and lowest graphed. HerbCover is just used as
#   an example here, but the numbers will be the same for any two continuous variables.
# I can then just remove the other unwanted levels to still just have three lines on the graph.
# (There is probably a smarter way to go about this than checking every level, but that's what
#   I'm going to do lol.)

# Precip range
range(dat.culm.ex$Prev_year_precip_scaled) # -1.115098, 3.063304


## Check for closest to 0 -------------------------------------------------

# Results: I checked 3-16 precip levels, and the closest that came to 0 was -0.001,
#   which was generated at 16 precip levels.

# 3 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.974

# 4 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 4, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.278

# 5 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 5, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.071

# 6 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 6, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.279

# 7 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 7, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.278

# 8 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 8, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.079

# 9 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.02

# 10 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 10, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.187

# 11 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 11, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.085

# 12 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 12, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.024

# 13 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 13, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.071

# 14 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 14, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.151

# 15 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 15, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.079

# 16 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 16, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.001


# Total culms -------------------------------------------------------------

## Total: Precip * shrub --------------------------------------------------

dat.culm %>% 
  ggplot(aes(x = ShrubCover, y = Total_Live_Culms,
             color = Prev_year_precip)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)")



## Total: Precip * herb ---------------------------------------------------


dat.culm %>% 
  ggplot(aes(x = HerbCover, y = Total_Live_Culms,
             color = Prev_year_precip)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)")
