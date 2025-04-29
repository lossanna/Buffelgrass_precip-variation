# Created: 2025-04-28
# Updated: 2025-04-29

# Purpose: Visualize relationships relevant to linear models from 06.2.R:
#   total7, repro7, bgden7, bgcov7.
# Also graph seedling survival (no corresponding linear models).

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

# Create df of average for precip
#   By site
plot.avg.site <- dat.plot %>% 
  group_by(Year, StudyYear, Site) %>% 
  summarise(Perc_dev_avg = mean(Perc_dev),
            Prev_year_precip_avg = mean(Prev_year_precip),
            .groups = "keep")


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

# Precip: scatterplot 
dat %>% 
  ggplot(aes(x = Date, y = Prev_year_precip, color = Site)) +
  geom_point() +
  theme_bw() +
  labs(x = NULL,
       y = "Previous year precip (mm)",
       title = "Precipitation conditions") +
  theme(axis.text.x = element_text(color = "black")) +
  theme(legend.title = element_blank())
  

# Precip: line graph
precip.site <- plot.avg.site %>% 
  ggplot(aes(x = Year, y = Prev_year_precip_avg)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw() +
  labs(x = NULL,
       y = "Previous year precip (mm)",
       title = "Precipitation conditions") +
  theme(axis.text.x = element_text(color = "black")) +
  geom_hline(data = map.avg, 
             aes(yintercept = MAP_avg), 
             linetype = "dashed", color = "red")
precip.site



# Total culms -------------------------------------------------------------

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

# Total change: Linear regression by Elevation
total.change.elev.lm <- culm.change %>% 
  ggplot(aes(x = Elevation_m, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  xlab("Elevation (m)") +
  ggtitle("Change in total culm count vs. elevation") +
  labs(y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")  
total.change.elev.lm

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
       x = "Plot slope (\u00B0)",
       title = "Change in total culm count vs. plot slope") 
total.change.plotslope.prevprecip


## Total change: Not significant ------------------------------------------

# Total change: Aspect (boxplot)
total.change.aspect <- culm.change %>% 
  ggplot(aes(x = Aspect, y = Change_Total_Live_Culms)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  theme_bw() +
  labs(title = "Change in total culm count by aspect",
       y = expression(Delta ~ "Total culm count"),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
total.change.aspect

# Total change: Linear regression by PlotSlope
total.change.plotslope.lm <- culm.change %>% 
  ggplot(aes(x = PlotSlope, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in total culm count") +
  labs(y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")  
total.change.plotslope.lm

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
       x = expression(Delta ~ paste("Density (individuals / ", m^2, ")")),
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
       x = expression(Delta ~ paste("Density (individuals / ", m^2, ")")),
       title = "Change in total culm count vs. plot density") 
total.change.bgden.prevprecip.lm

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



# Reproductive culms ------------------------------------------------------

## Repro change: Significant ----------------------------------------------

# Repro change: Aspect (boxplot)
repro.change.aspect <- culm.change %>% 
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
repro.change.aspect

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
       x = "Plot slope (\u00B0)",
       title = "Change in reproductive culm count vs. plot slope") 
repro.change.plotslope.prevprecip


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

# Repro change: Linear regression by Elevation
repro.change.elev.lm <- culm.change %>% 
  ggplot(aes(x = Elevation_m, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  xlab("Elevation (m)") +
  ggtitle("Change in reproductive culm count vs. elevation") +
  labs(y = expression(Delta ~ "Reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")  
repro.change.elev.lm

# Repro change: Linear regression by PlotSlope
repro.change.plotslope.lm <- culm.change %>% 
  ggplot(aes(x = PlotSlope, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in reproductive culm count") +
  labs(y = expression(Delta ~ "Reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")  
repro.change.plotslope.lm

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






# Buffelgrass density -----------------------------------------------------

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

# BG density change: Aspect (boxplot)
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


## BG density change: Not significant -------------------------------------

# BG density change: Linear regression by Elevation
bgden.change.elev.lm <- plot.change %>% 
  ggplot(aes(x = Elevation_m, y = Change_BGDensity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Elevation (m)",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       title = "Change in buffelgrass density vs. elevation")
bgden.change.elev.lm

# BG density change: Linear regression by PlotSlope
bgden.change.plotslope.lm <- plot.change %>% 
  ggplot(aes(x = PlotSlope, y = Change_BGDensity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Plot slope (\u00B0)",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       title = "Change in buffelgrass density vs. plot slope")
bgden.change.plotslope.lm

# BG density change: linear regression by herb cover (change)
bgden.change.herb.lm <- plot.change %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGDensity)) +
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
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in buffelgrass density vs. herb cover change")
bgden.change.herb.lm

# BG density change: linear regression by herb cover (change) and Prev_year_precip
bgden.change.herb.prevprecip.lm <- plot.change %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGDensity)) +
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
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in buffelgrass density vs. herb cover change")
bgden.change.herb.prevprecip.lm


# Buffelgrass cover -------------------------------------------------------

## BG cover change: Significant -------------------------------------------

# BG cover change: Linear regression by Prev_year_precip
bgcov.change.precip.lm <- plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Previous year precip (mm)",
       y = expression(Delta ~ "Cover (%)"),
       title = "Change in buffelgrass cover vs. precip")
bgcov.change.precip.lm

# BG cover change: Linear regression by Elevation
bgcov.change.elev.lm <- plot.change %>% 
  ggplot(aes(x = Elevation_m, y = Change_BGCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Elevation (m)",
       y = expression(Delta ~ "Cover (%)"),
       title = "Change in buffelgrass cover vs. elevation")
bgcov.change.elev.lm

# BG cover change: Aspect (boxplot)
bgcov.change.aspect <- plot.change %>% 
  filter(Aspect != "flat") %>% 
  ggplot(aes(x = Aspect, y = Change_BGCover)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  theme_bw() +
  labs(title = "Change in buffelgrass cover by aspect",
       y = expression(Delta ~ "Cover (%)"),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
bgcov.change.aspect


## BG cover change: Not significant ---------------------------------------

# BG cover change: Linear regression by PlotSlope
bgcov.change.plotslope.lm <- plot.change %>% 
  ggplot(aes(x = PlotSlope, y = Change_BGCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Plot slope (\u00B0)",
       y = expression(Delta ~ "Cover (%)"),
       title = "Change in buffelgrass cover vs. plot slope")
bgcov.change.plotslope.lm

# BG density change: linear regression by shrub cover (change)
bgcov.change.shrub.lm <- plot.change %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Buffelgrass cover (%)"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffegrass cover vs. shrub cover change")
bgcov.change.shrub.lm

# BG cover change: linear regression by shrub cover (change) and Prev_year_precip
bgcov.change.shrub.prevprecip.lm <- plot.change %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGCover)) +
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
  labs(y = expression(Delta ~ "Buffelgrass cover (%)"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffelgrass cover vs. shrub cover change")
bgcov.change.shrub.prevprecip.lm

# BG cover change: linear regression by PlotSlope and Prev_year_precip
bgcov.change.plotslope.prevprecip.lm <- plot.change %>% 
  ggplot(aes(x = PlotSlope, y = Change_BGCover)) +
  geom_point(aes(color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Buffelgrass cover (%)"),
       x = "Plot slope (\u00B0)",
       title = "Change in buffelgrass cover vs. plot slope")
bgcov.change.plotslope.prevprecip.lm


# Shrub & herb cover ------------------------------------------------------

# Shrub cover change: Linear regression by Prev_year_precip
shrub.change.precip.lm <- plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_ShrubCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Previous year precip (mm)",
       y = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in shrub cover vs. precip")
shrub.change.precip.lm

# Herb cover change: Linear regression by Prev_year_precip
herb.change.precip.lm <- plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_HerbCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Previous year precip (mm)",
       y = expression(Delta ~ "Native herb cover (%)"),
       title = "Change in grass & forb cover vs. precip")
herb.change.precip.lm


# Survival ----------------------------------------------------------------

# Survival: linear regression by Prev_year_precip
survival.precip.lm <- dat %>% 
  filter(!is.na(survival_perc)) %>% 
  ggplot(aes(x = Prev_year_precip, y = survival_perc)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  labs(x = "Previous year precip (mm)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. precip")
survival.precip.lm


# Change in culms
# Survival: linear regression by total culms (change)
culm.change %>% 
  filter(!is.na(survival_perc)) %>% 
  ggplot(aes(x = survival_perc, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent) +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Seedling survival (%)",
       y = expression(Delta ~ "Total culm count"),
       title = "Seedling survival vs. change in total culms")

# Survival: scatterplot by total culms (change) and Prev_year_precip
survival.total.change.prevprecip <- culm.change %>% 
  filter(!is.na(survival_perc)) %>% 
  ggplot(aes(x = survival_perc, y = Change_Total_Live_Culms)) +
  geom_point(aes(color = Prev_year_precip)) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = "Seedling survival (%)",
       title = "Seedling survival vs. change in total culms") 
survival.total.change.prevprecip

# Survival: linear regression by total culms (change) and Prev_year_precip
survival.total.change.prevprecip.lm <- culm.change %>% 
  filter(!is.na(survival_perc)) %>% 
  ggplot(aes(x = survival_perc, y = Change_Total_Live_Culms)) +
  geom_point(aes(color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = "Seedling survival (%)",
       title = "Seedling survival vs. change in total culms") 
survival.total.change.prevprecip.lm

# Survival: scatterplot by repro culms (change) and Prev_year_precip
survival.repro.change.prevprecip <- culm.change %>% 
  filter(!is.na(survival_perc)) %>% 
  ggplot(aes(x = survival_perc, y = Change_Reproductive_culms)) +
  geom_point(aes(color = Prev_year_precip)) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = "Seedling survival (%)",
       title = "Seedling survival vs. change in reproductive culms") 
survival.repro.change.prevprecip

# Survival: scatterplot by repro culms (change) and Prev_year_precip
survival.repro.change.prevprecip.lm <- culm.change %>% 
  filter(!is.na(survival_perc)) %>% 
  ggplot(aes(x = survival_perc, y = Change_Reproductive_culms)) +
  geom_point(aes(color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent) +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = "Seedling survival (%)",
       title = "Seedling survival vs. change in reproductive culms") 
survival.repro.change.prevprecip.lm


# Raw culm counts
# Survival: linear regression by total culms and Prev_year_precip
dat %>% 
  filter(!is.na(survival_perc)) %>% 
  ggplot(aes(x = survival_perc, y = Total_Live_Culms)) +
  geom_point(aes(color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent) + 
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  theme_bw() +
  labs(y = "No. of culms",
       x = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. total culm count") 

# Survival: linear regression by repro culms and Prev_year_precip
dat %>% 
  filter(!is.na(survival_perc)) %>% 
  ggplot(aes(x = survival_perc, y = Reproductive_culms)) +
  geom_point(aes(color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent) + 
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  theme_bw() +
  labs(y = "No. of reproductive culms",
       x = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. reproductive culm count") 


# Survival: linear regression by buffelgrass density (change) and Prev_year_precip
culm.change %>% 
  filter(!is.na(survival_perc)) %>% 
  ggplot(aes(x = survival_perc, y = Change_BGDensity)) +
  geom_point(aes(color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent) + 
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  theme_bw() +
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. change in plot density") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")

# Survival: linear regression by buffelgrass density and previous precip
dat %>% 
  filter(!is.na(survival_perc)) %>% 
  ggplot(aes(x = survival_perc, y = BGDensity)) +
  geom_point(aes(color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent) + 
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  theme_bw() +
  labs(y = expression(paste("Density (individuals /  ", m^2, ")")),
       x = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. plot density") 



# Other -------------------------------------------------------------------

# Total change: linear regression with buffelgrass cover (change)
culm.change %>% 
  ggplot(aes(x = Change_BGCover, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 



# Write out draft figures -------------------------------------------------

## Precipitation ----------------------------------------------------------

tiff("figures/2025-05_draft-figures/Precip_site.tiff", units = "in", height = 5, width = 7, res = 150)
precip.site
dev.off()


## Total change -----------------------------------------------------------

# Significant
# Total change vs. Prev_year_precip
tiff("figures/2025-05_draft-figures/Total-change_by-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.change.precip.lm
dev.off()

# Total change vs. Elevation
tiff("figures/2025-05_draft-figures/Total-change_by-elevation_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.change.elev.lm
dev.off()

# Total change vs. BG density change
tiff("figures/2025-05_draft-figures/Total-change_by-BG-density-change_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.change.bgden.lm
dev.off()

# Total change vs. shrub cover change
tiff("figures/2025-05_draft-figures/Total-change_by-shrub-cover-change_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.change.shrub.lm
dev.off()

# Total change vs. shrub cover change by Prev_year_precip (linear regression)
tiff("figures/2025-05_draft-figures/Total-change_by-shrub-cover-change-and-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.change.shrub.prevprecip.lm
dev.off()

# Total change vs. herb cover change by Prev_year_precip
tiff("figures/2025-05_draft-figures/Total-change_by-herb-cover-change-and-prev-year-precip.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.change.herb.prevprecip
dev.off()

# Total change vs. PlotSlope by Prev_year_precip
tiff("figures/2025-05_draft-figures/Total-change_by-plot-slope-and-prev-year-precip.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.change.plotslope.prevprecip
dev.off()


# Not significant
# Total change vs. Aspect
tiff("figures/2025-05_draft-figures/Total-change_by-aspect_boxplot.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.change.aspect
dev.off()

# Total change vs. PlotSlope
tiff("figures/2025-05_draft-figures/Total-change_by-plot-slope_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.change.elev.lm
dev.off()

# Total change vs. herb cover change
tiff("figures/2025-05_draft-figures/Total-change_by-herb-cover-change_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.change.herb.lm
dev.off()

# Total change vs. BG density change by Prev_year_precip
tiff("figures/2025-05_draft-figures/Total-change_by-BG-density-change-and-prev-year-precip.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.change.bgden.prevprecip
dev.off()

# Total change vs. BG density change by Prev_year_precip (linear regression)
tiff("figures/2025-05_draft-figures/Total-change_by-BG-density-change-and-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.change.bgden.prevprecip.lm
dev.off()

# Total change vs. herb cover change by Prev_year_precip (linear regression)
tiff("figures/2025-05_draft-figures/Total-change_by-herb-cover-change-and-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.change.herb.prevprecip.lm
dev.off()


## Repro change -----------------------------------------------------------

# Significant
# Repro change by Aspect
tiff("figures/2025-05_draft-figures/Repro-change_by-aspect_boxplot.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.change.aspect
dev.off()

# Repro change vs. BG density change
tiff("figures/2025-05_draft-figures/Repro-change_by-BG-density-change_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.change.bgden.lm
dev.off()

# Repro change vs. shrub cover change
tiff("figures/2025-05_draft-figures/Repro-change_by-shrub-cover-change_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.change.shrub.lm
dev.off()

# Repro change vs. herb cover change
tiff("figures/2025-05_draft-figures/Repro-change_by-herb-cover-change_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.change.herb.lm
dev.off()

# Repro change vs. BG density change by Prev_year_precip (linear regression)
tiff("figures/2025-05_draft-figures/Repro-change_by-BG-density-change-and-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.change.bgden.prevprecip.lm
dev.off()

# Repro change vs. BG density change by Prev_year_precip
tiff("figures/2025-05_draft-figures/Repro-change_by-BG-density-change-and-prev-year-precip.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.change.bgden.prevprecip
dev.off()

# Repro change vs. PlotSlope by Prev_year_precip
tiff("figures/2025-05_draft-figures/Repro-change_by-plot-slope-and-prev-year-precip.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.change.plotslope.prevprecip
dev.off()


# Not significant
# Repro change vs. Prev_year_precip
tiff("figures/2025-05_draft-figures/Repro-change_by-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.change.precip.lm
dev.off()

# Repro change vs. Elevation
tiff("figures/2025-05_draft-figures/Repro-change_by-elevation_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.change.elev.lm
dev.off()

# Repro change vs. PlotSlope
tiff("figures/2025-05_draft-figures/Repro-change_by-plot-slope_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.change.elev.lm
dev.off()

# Repro change vs. shrub cover change by Prev_year_precip (linear regression)
tiff("figures/2025-05_draft-figures/Repro-change_by-shrub-cover-change-and-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.change.shrub.prevprecip.lm
dev.off()

# Repro change vs. herb cover change by Prev_year_precip (linear regression)
tiff("figures/2025-05_draft-figures/Repro-change_by-herb-cover-change-and-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.change.herb.prevprecip.lm
dev.off()


## Buffelgrass density change ---------------------------------------------

# Significant
# BG density change vs. Prev_year_precip
tiff("figures/2025-05_draft-figures/BG-density-change_by-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.change.precip.lm
dev.off()

# BG density change by Aspect
tiff("figures/2025-05_draft-figures/BG-density-change_by-aspect_boxplot.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.change.aspect
dev.off()

# BG density change vs. shrub cover change
tiff("figures/2025-05_draft-figures/BG-density-change_by-shrub-cover-change_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.change.shrub.lm
dev.off()

# BG density change vs. shrub cover change by Prev_year_precip (linear regression)
tiff("figures/2025-05_draft-figures/BG-density-change_by-shrub-cover-change-and-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.change.shrub.prevprecip.lm
dev.off()


# Not significant
# BG density change vs. Elevation
tiff("figures/2025-05_draft-figures/BG-density-change_by-elevation_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.change.elev.lm
dev.off()

# BG density change vs. PlotSlope
tiff("figures/2025-05_draft-figures/BG-density-change_by-plot-slope_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.change.plotslope.lm
dev.off()

# BG density change vs. herb cover change
tiff("figures/2025-05_draft-figures/BG-density-change_by-herb-cover-change_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.change.herb.lm
dev.off()

# BG density change vs. herb cover change by Prev_year_precip (linear regression)
tiff("figures/2025-05_draft-figures/BG-density-change_by-herb-cover-change-and-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.change.herb.prevprecip.lm
dev.off()


## Buffelgrass cover change -----------------------------------------------

# Significant
# BG cover change vs. Prev_year_precip
tiff("figures/2025-05_draft-figures/BG-cover-change_by-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.change.precip.lm
dev.off()

# BG cover change vs. Elevation
tiff("figures/2025-05_draft-figures/BG-cover-change_by-elevation_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.change.elev.lm
dev.off()

# BG cover change by Aspect
tiff("figures/2025-05_draft-figures/BG-cover-change_by-aspect_boxplot.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.change.aspect
dev.off()

# Not significant
# BG cover change vs. PlotSlope
tiff("figures/2025-05_draft-figures/BG-cover-change_by-plot-slope_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.change.plotslope.lm
dev.off()

# BG cover change vs. shrub cover change
tiff("figures/2025-05_draft-figures/BG-cover-change_by-shrub-cover-change_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.change.shrub.lm
dev.off()

# BG cover change vs. shrub cover change by Prev_year_precip (linear regression)
tiff("figures/2025-05_draft-figures/BG-cover-change_by-shrub-cover-change-and-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.change.shrub.prevprecip.lm
dev.off()

# BG cover change vs. PlotSlope by Prev_year_precip
tiff("figures/2025-05_draft-figures/BG-cover-change_by-plot-slope-and-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.change.plotslope.prevprecip.lm
dev.off()


## Shrub & herb cover -----------------------------------------------------

# Shrub cover change vs. Prev_year_precip
tiff("figures/2025-05_draft-figures/Shrub-change_by-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
shrub.change.precip.lm
dev.off()

# Herb cover change by Prev_year_precip
tiff("figures/2025-05_draft-figures/Herb-change_by-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
herb.change.precip.lm
dev.off()



save.image("RData/05.1_draft-figs-for-lm-3.0.RData")
