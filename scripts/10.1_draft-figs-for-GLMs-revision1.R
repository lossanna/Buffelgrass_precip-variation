# Created: 2026-01-28
# Updated: 2026-01-29

# Purpose: Graph data to accompany 09.1.R of revision1 GLMs.


library(tidyverse)
library(insight)
library(modelbased)
library(ggeffects)
library(viridis)
library(ggpubr)

# Load data ---------------------------------------------------------------

dat <- read_csv("data/cleaned/04_demography-data_clean.csv")
load("RData/09_zip.total.6.RData")

# Data wrangling ----------------------------------------------------------

# Need 3 datasets:
#   1. Data for graphing observed data
#   2. Data to construct datagrid with predictions & CI
#   3. Data to construct datagrid with unscaled explanatory variable (to be added to #2)


# 1. Dataset for graphing (response and unscaled explanatory variables)
#   Culm - total & repro
dat.culm <- dat %>% 
  filter(Aspect != "flat") %>% 
  select(Total_Live_Culms, Reproductive_culms, 
         Prev_year_precip, Aspect, PlotSlope, BGDensity, ShrubCover, HerbCover) %>% 
  mutate(PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         ShrubCover_scaled = scale(ShrubCover, center = TRUE, scale = TRUE)[, 1],
         HerbCover_scaled = scale(HerbCover, center = TRUE, scale = TRUE)[, 1],
         BGDensity_scaled = scale(BGDensity, center = TRUE, scale = TRUE)[, 1])

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

# Precip levels (min, mean, max)
range(dat.culm.ex$Prev_year_precip_scaled) # -1.114094, 3.044518


## Check for closest to 0 -------------------------------------------------

# Results: I checked 3-30 precip levels, and the closest that came to 0 was -0.005,
#   which was generated at 16 precip levels.

# 3 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.965

# 4 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 4, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.272

# 5 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 5, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.074

# 6 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 6, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.282

# 7 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 7, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.272

# 8 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 8, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.074

# 9 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.074

# 10 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 10, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.190

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
unique(x$Prev_year_precip_scaled) # 0.020

# 13 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 13, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.074

# 14 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 14, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.154

# 15 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 15, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.074

# 16 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 16, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.005 (5th)

# 17 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 17, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.074

# 18 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 18, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.109

# 19 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 19, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.041

# 20 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 20, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.020 

# 21 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 21, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.074

# 22 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 22, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.074

# 23 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 23, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.020

# 24 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 24, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.029

# 25 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 25, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.074

# 26 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 26, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.051

# 27 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 27, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.006

# 28 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 28, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.036

# 29 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 29, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # 0.074

# 30 precip levels
x <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 30, numerics = "all") %>% 
  arrange(HerbCover_scaled)
unique(x$Prev_year_precip_scaled) # -0.033 



# Total culms -------------------------------------------------------------

## Total: Precip * shrub --------------------------------------------------

# Generate prediction and add unscaled variable 
#   Construct datagrid with scaled variable - 16 precip levels to get close to mean precip
insight.total.shrub.precip <- dat.culm.ex %>% 
  get_datagrid(c("ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 16, numerics = "all") %>% 
  arrange(ShrubCover_scaled) %>% 
  filter(Prev_year_precip_scaled %in% c(-1.114, -0.005, 3.045)) %>% 
  distinct(.keep_all = TRUE)
#   Add prediction column
insight.total.shrub.precip$Predicted <- get_predicted(zip.total6, insight.total.shrub.precip)
#   Construct datagrid with unscaled variables
unscaled.shrub.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 16, numerics = "all") %>% 
  arrange(ShrubCover) %>% 
  distinct(.keep_all = TRUE)
#   Precip value in 5th position will be equivalent to scaled value of -0.005 
unique(unscaled.shrub.precip$Prev_year_precip)
unscaled.shrub.precip <- unscaled.shrub.precip %>% 
  filter(Prev_year_precip %in% c(219.450, 294.607, 501.290))
#   Add unscaled variables to datagrid with prediction 
insight.total.shrub.precip$ShrubCover <- unscaled.shrub.precip$ShrubCover
insight.total.shrub.precip$Prev_year_precip <- unscaled.shrub.precip$Prev_year_precip


# Graph
total.shrub.precip <- dat.culm %>% 
  ggplot(aes(x = ShrubCover, y = Total_Live_Culms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.total.shrub.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  labs(y = "Total culm count",
       x = "Native shrub cover (%)",
       title = "Total culm count vs. shrub cover")
total.shrub.precip



## Total: Precip * herb ---------------------------------------------------


# Generate prediction and add unscaled variable 
#   Construct datagrid with scaled variable - 16 precip levels to get close to mean precip
insight.total.herb.precip <- dat.culm.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 16, numerics = "all") %>% 
  arrange(HerbCover_scaled) %>% 
  filter(Prev_year_precip_scaled %in% c(-1.114, -0.005, 3.045)) %>% 
  distinct(.keep_all = TRUE)
#   Add prediction column
insight.total.herb.precip$Predicted <- get_predicted(zip.total6, insight.total.herb.precip)
#   Construct datagrid with unscaled variables
unscaled.herb.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("HerbCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 16, numerics = "all") %>% 
  arrange(HerbCover) %>% 
  distinct(.keep_all = TRUE)
#   Precip value in 5th position will be equivalent to scaled value of -0.005 
unique(unscaled.herb.precip$Prev_year_precip)
unscaled.herb.precip <- unscaled.herb.precip %>% 
  filter(Prev_year_precip %in% c(219.450, 294.607, 501.290))
#   Add unscaled variables to datagrid with prediction 
insight.total.herb.precip$HerbCover <- unscaled.herb.precip$HerbCover
insight.total.herb.precip$Prev_year_precip <- unscaled.herb.precip$Prev_year_precip


# Graph
total.herb.precip <- dat.culm %>% 
  ggplot(aes(x = HerbCover, y = Total_Live_Culms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.total.herb.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  labs(y = "Total culm count",
       x = "Native herb cover (%)",
       title = "Total culm count vs. herb cover")
total.herb.precip


save.image("RData/10.1_draft-figs-for GLMs-revision1.RData")
