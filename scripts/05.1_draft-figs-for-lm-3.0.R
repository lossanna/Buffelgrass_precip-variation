# Created: 2025-04-28
# Updated: 2025-04-28

# Purpose: Visualize relationships relevant to linear models (from 06.2.R).

library(tidyverse)
library(scales)
library(viridis)

# Load data ---------------------------------------------------------------

dat <- read_csv("data/cleaned/04_demography-data_clean.csv")
culm.change.raw <- read_csv("data/cleaned/04_change-in-culm-density-cover_clean.csv")


# Data wrangling ----------------------------------------------------------

# Remove flat obs
culm.change <- culm.change.raw %>% 
  filter(Aspect != "flat")


# Separate out plot-level data and remove flat
dat.plot <- dat %>% 
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm) %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(Aspect != "flat")

plot.change <- culm.change %>% 
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm,
         -Change_Reproductive_culms, -Change_Total_Live_Culms) %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(Aspect != "flat")


# Aspects at each site ----------------------------------------------------

dat %>% 
  filter(Site == "ApachePeak") %>% 
  count(Aspect) # E, S, W, flat
dat %>% 
  filter(Site == "KinneyHill") %>% 
  count(Aspect) # E, S, SW, W
dat %>% 
  filter(Site == "LomaVerde") %>% 
  count(Aspect) # S, SW, W
dat %>% 
  filter(Site == "TumamocHill") %>% 
  count(Aspect) # E, N, S, W



# Precip ------------------------------------------------------------------

# 30-year averages by site
dat.plot %>% 
  select(Site, Transect, MAP) %>% 
  distinct(.keep_all = TRUE) %>% 
  print(n = 27)

map.avg <- dat.plot %>% 
  select(Site, Transect, MAP) %>% 
  distinct(.keep_all = TRUE) %>% 
  group_by(Site) %>% 
  summarise(MAP_avg = mean(MAP))
map.avg



# Change in total culms ---------------------------------------------------

## Total change: Significant ----------------------------------------------

# Total change: Linear regression by Prev_year_precip
total.change.precip.lm <- culm.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  xlab("Previous year precip (mm)") +
  ggtitle("Change in total culm count vs. precip") +
  labs(y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")  
total.change.precip.lm

# Total change: linear regression by buffelgrass density (change)
total.change.bgden.lm <- culm.change %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ paste("Density (individuals /  ", m^2, ")")),
       title = "Change in total culm count vs. plot density")
total.change.bgden.lm

# Total change: linear regression by shrub cover (change)
total.change.shrub.lm <- culm.change %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in total culm count vs. plot shrub cover")
total.change.shrub.lm

# Total change: linear regression by shrub cover (change) and Prev_year_precip
total.change.shrub.prevprecip.lm <- culm.change %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Total_Live_Culms)) +
  geom_point(aes(color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in total culm count vs. plot shrub cover")
total.change.shrub.prevprecip.lm

# Total change: scatterplot by herb cover (change) and Prev_year_precip
total.change.herb.prevprecip <- culm.change %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Total_Live_Culms)) +
  geom_point(aes(color = Prev_year_precip)) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in total culm count vs. plot herb cover")
total.change.herb.prevprecip

# Total change: scatterplot PlotSlope and Prev_year_precip
total.change.plotslope.prevprecip <- culm.change %>% 
  ggplot(aes(x = PlotSlope, y = Change_Total_Live_Culms, color = Prev_year_precip)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = "Plot slope (degrees)",
       title = "Change in total culm count vs. plot slope") 
total.change.plotslope.prevprecip

# Total change: Linear regression by Elevation
total.change.all.elev.lm <- culm.change %>% 
  ggplot(aes(x = Elevation_ft, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  xlab("Elevation (ft)") +
  ggtitle("Change in total culm count") +
  labs(y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")  
total.change.all.elev.lm


## Total change: Not significant ------------------------------------------

# Total change: linear regression by herb cover (change)
total.change.herb.lm <- culm.change %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in total culm count vs. plot herb cover")
total.change.herb.lm

# Total change: linear regression by herb cover (change) and previous year precipitation
total.change.herb.prevprecip.lm <- culm.change %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Total_Live_Culms)) +
  geom_point(aes(color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in total culm count vs. plot herb cover")
total.change.herb.prevprecip.lm

# Total change: scatterplot buffelgrass density (change) and Prev_year_precip
total.change.bgden.prevprecip <- culm.change %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Total_Live_Culms, color = Prev_year_precip)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ paste("Density (individuals /  ", m^2, ")")),
       title = "Change in total culm count vs. plot density") 
total.change.bgden.prevprecip

# Total change: linear regression buffelgrass density (change) and Prev_year_precip
total.change.bgden.prevprecip.lm <- culm.change %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Total_Live_Culms)) +
  geom_point(aes(color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ paste("Density (individuals /  ", m^2, ")")),
       title = "Change in total culm count vs. plot density") 
total.change.bgden.prevprecip.lm



# Change in reproductive culms --------------------------------------------

## Repro change: Significant ----------------------------------------------

# Repro change: linear regression by buffelgrass density (change)
repro.change.bgden.lm <- culm.change %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ paste("Density (individuals /  ", m^2, ")")),
       title = "Change in reproductive culm count vs. plot density")
repro.change.bgden.lm

# Repro change: linear regression by shrub cover (change)
repro.change.shrub.lm <- culm.change %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in reproductive culm count vs. plot shrub cover")
repro.change.shrub.lm

# Repro change: linear regression by herb cover (change)
repro.change.herb.lm <- culm.change %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ "Native grass and forb cover (%)"),
       title = "Change in reproductive culm count vs. plot herb cover")
repro.change.herb.lm

# Repro change: scatterplot buffelgrass density (change) and Prev_year_precip
repro.change.bgden.prevprecip <- culm.change %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Reproductive_culms, color = Prev_year_precip)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ paste("Density (individuals /  ", m^2, ")")),
       title = "Change in reproductive culm count vs. plot density") 
repro.change.bgden.prevprecip

# Repro change: Aspect, all conditions (boxplot)
repro.change.all.aspect <- culm.change %>% 
  ggplot(aes(x = Aspect, y = Change_Reproductive_culms)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  theme_bw() +
  labs(title = "Change in reproductive culm count by aspect",
       y = expression(Delta ~ "Reproductive culm count"),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
repro.change.all.aspect


## Repro change: Not significant ------------------------------------------

# Repro change: Linear regression by Prev_year_precip
repro.change.precip.lm <- culm.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  xlab("Previous year precip (mm)") +
  ggtitle("Change in reproductive culm count vs. precip") +
  labs(y = expression(Delta ~ "Reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")  
repro.change.precip.lm

# Repro change: linear regression by shrub cover (change) and Prev_year_precip
repro.change.shrub.prevprecip.lm <- culm.change %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Reproductive_culms)) +
  geom_point(aes(color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in reproductive culm count vs. plot shrub cover")
repro.change.shrub.prevprecip.lm

# Repro change: linear regression by herb cover (change) and Prev_year_precip
repro.change.herb.prevprecip.lm <- culm.change %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Reproductive_culms)) +
  geom_point(aes(color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ "Native grass and forb cover (%)"),
       title = "Change in reproductive culm count vs. plot herb cover")
repro.change.herb.prevprecip.lm

# Repro change: linear regression buffelgrass density (change) and Prev_year_precip
repro.change.bgden.prevprecip.lm <- culm.change %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Reproductive_culms)) +
  geom_point(aes( color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ paste("Density (individuals /  ", m^2, ")")),
       title = "Change in reproductive culm count vs. plot density") 
repro.change.bgden.prevprecip.lm

# Repro change: scatterplot PlotSlope and Prev_year_precip
repro.change.plotslope.prevprecip <- culm.change %>% 
  ggplot(aes(x = PlotSlope, y = Change_Reproductive_culms, color = Prev_year_precip)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = "Plot slope (degrees)",
       title = "Change in reproductive culm count vs. plot slope") 
repro.change.plotslope.prevprecip



# Change in buffelgrass density -------------------------------------------

## BG density change: Significant -----------------------------------------

# BG density change: linear regression by Prev_year_precip
bgden.change.precip.lm <- plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGDensity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Previous year precip (mm)",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       title = "Change in buffelgrass density vs. precip")
bgden.change.precip.lm

# BG density change: linear regression by shrub cover (change)
bgden.change.shrub.lm <- plot.change %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGDensity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(paste(Delta ~ "Buffelgrass density (individuals /  ", m^2, ")")),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffelgrass density vs. shrub cover change")
bgden.change.shrub.lm

# BG density change: linear regression by shrub cover (change) and Prev_year_precip
bgden.change.shrub.prevprecip.lm <- plot.change %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGDensity)) +
  geom_point(aes(color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(paste(Delta ~ "Buffelgrass density (individuals /  ", m^2, ")")),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffelgrass density vs. shrub cover change")
bgden.change.shrub.prevprecip.lm

# BG density change: Aspect, all conditions (boxplot)
bgden.change.aspect <- plot.change %>% 
  filter(Aspect != "flat") %>% 
  ggplot(aes(x = Aspect, y = Change_BGDensity)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  theme_bw() +
  labs(title = "Change in buffelgrass density by aspect",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
bgden.change.aspect


## BG density change: Not significant -------------------------------------


# Change in buffelgrass cover ---------------------------------------------

## BG cover change: Significant -------------------------------------------


## BG cover change: Not significant ---------------------------------------




# Other precip ------------------------------------------------------------



# Survival ----------------------------------------------------------------



# Other random ------------------------------------------------------------

# Repro change: linear regression with buffelgrass cover (change)
culm.change %>% 
  ggplot(aes(x = Change_BGCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 


save.image("RData/05.1_draft-figs-for-lm-3.0.RData")
