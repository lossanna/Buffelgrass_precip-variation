# Created: 2025-07-11
# Updated: 2025-07-15

# Purpose: Visualize relationships relevant to v5.3 linear models for all response 
#   variables (total culm change, reproductive culm change, buffelgrass density & cover change,
#   survival); linear models from 06.5_linear-models-5.3.R.
# Figures have not actually changed from 05.1.R (in 2025-05_draft-figures/), as data are the same, 
#   but significance has sometimes changed, and Site now added as a boxplot.

# I have excluded some iterations of density/shrub/herb plots (alone & with precip interaction)
#   to consolidate script, but these are still available from 2025-07_draft-figures.


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

# Prepare survival data
survival <- dat %>% 
  filter(!is.na(survival_perc)) %>% 
  select(Date, Year, StudyYear, Site, Transect, Plot, survival_perc) %>% 
  distinct(.keep_all = TRUE)

# Create df of average for precip by site
plot.avg.site <- dat.plot %>% 
  group_by(Year, StudyYear, Site) %>% 
  summarise(Perc_dev_avg = mean(Perc_dev),
            Prev_year_precip_avg = mean(Prev_year_precip),
            .groups = "keep")



# Site characteristics ----------------------------------------------------

# Aspects
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

# Plot slope
dat.plot %>% 
  group_by(Site) %>% 
  summarise(PlotSlope_min = min(PlotSlope),
            PlotSlope_max = max(PlotSlope),
            PlotSlope_avg = mean(PlotSlope),
            PlotSlope_median = median(PlotSlope))
summary(dat.plot$PlotSlope)


# Number of plots
length(unique(dat.plot$Plot)) # 79

# Transects with two plots
dat.plot %>% 
  select(Site, Transect, Plot) %>% 
  distinct(.keep_all = TRUE) %>% 
  group_by(Transect) %>% 
  summarise(PlotCount = n()) %>% 
  print(n = 27) %>% 
  filter(PlotCount == 2)


# Survival
nrow(survival) # 141 observations
length(unique(survival$Plot)) # 65 unique plots
count(survival, Site)


# Precip ------------------------------------------------------------------

map.avg <- dat.plot %>% 
  select(Site, Transect, MAP) %>% 
  distinct(.keep_all = TRUE) %>% 
  group_by(Site) %>% 
  summarise(MAP_avg = mean(MAP))
map.avg

# Line graph of averages across sites
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



# Total culm change -------------------------------------------------------

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

# Total change: Aspect (boxplot) (SW > E)
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
       title = "Change in total culm count vs. plot density change")
total.change.bgden.lm

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
       title = "Change in total culm count vs. shrub cover change")
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
       title = "Change in total culm count vs. herb cover change")
total.change.herb.prevprecip

# Total change: Site (boxplot) (AP > KH & TH)
total.change.site <- culm.change %>% 
  ggplot(aes(x = Site, y = Change_Total_Live_Culms)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  theme_bw() +
  labs(title = "Change in total culm count by site",
       y = expression(Delta ~ "Total culm count"),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
total.change.site


## Total change: Not significant ------------------------------------------

# Total change: Linear regression by PlotSlope
total.change.plotslope.lm <- culm.change %>% 
  ggplot(aes(x = PlotSlope, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in total culm count vs. plot slope") +
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
       title = "Change in total culm count vs. herb cover change")
total.change.herb.lm

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
       title = "Change in total culm count vs. plot density change") 
total.change.bgden.prevprecip.lm




# Reproductive culm change ------------------------------------------------

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
       title = "Change in repro culm count vs. plot density change")
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
       title = "Change in repro culm count vs. shrub cover change")
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
       title = "Change in repro culm count vs. herb cover change")
repro.change.herb.lm

# Repro change: Site (boxplot)
repro.change.site <- culm.change %>% 
  ggplot(aes(x = Site, y = Change_Reproductive_culms)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  theme_bw() +
  labs(title = "Change in reproductive culm count by site",
       y = expression(Delta ~ "Reproductive culm count"),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
repro.change.site


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

# Repro change: scatterplot by buffelgrass density (change) and Prev_year_precip
repro.change.bgden.prevprecip <- culm.change %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Reproductive_culms)) +
  geom_point(aes( color = Prev_year_precip)) +
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
       title = "Change in reproductive culm count vs. plot density change") 
repro.change.bgden.prevprecip

# Repro change: scatterplot by shrub cover (change) and Prev_year_precip
repro.change.shrub.prevprecip <- culm.change %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Reproductive_culms)) +
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
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in reproductive culm count vs. shrub cover change")
repro.change.shrub.prevprecip

# Repro change: scatterplot by herb cover (change) and Prev_year_precip
repro.change.herb.prevprecip <- culm.change %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Reproductive_culms)) +
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
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ "Native grass and forb cover (%)"),
       title = "Change in reproductive culm count vs. herb cover change")
repro.change.herb.prevprecip



# Buffelgrass density change ----------------------------------------------

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

# BG density change: scatterplot by shrub cover (change) and Prev_year_precip
bgden.change.shrub.prevprecip <- plot.change %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGDensity)) +
  geom_point(aes(color = Prev_year_precip)) +
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
bgden.change.shrub.prevprecip

# BG density change: Site (boxplot)
bgden.change.site <- plot.change %>% 
  ggplot(aes(x = Site, y = Change_BGDensity)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  theme_bw() +
  labs(title = "Change in buffelgrass density by site",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
bgden.change.site


## BG density change: Not significant -------------------------------------

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




# Buffelgrass cover change ------------------------------------------------

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

# BG cover change: linear regression by shrub cover (change)
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

# BG cover change: Site (boxplot)
bgcov.change.site <- plot.change %>% 
  ggplot(aes(x = Site, y = Change_BGCover)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  theme_bw() +
  labs(title = "Change in buffelgrass cover by site",
       y = expression(Delta ~ "Cover (%)"),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
bgcov.change.site



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

# BG cover change: scatterplot by shrub cover (change) and Prev_year_precip
bgcov.change.shrub.prevprecip <- plot.change %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGCover)) +
  geom_point(aes(color = Prev_year_precip)) +
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
bgcov.change.shrub.prevprecip

# BG cover change: linear regression by herb cover (change) and Prev_year_precip
bgcov.change.herb.prevprecip.lm <- plot.change %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGCover)) +
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
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in buffelgrass cover vs. herb cover change")
bgcov.change.herb.prevprecip.lm



# Survival ----------------------------------------------------------------

## Survival: Significant --------------------------------------------------

# Survival: linear regression by Prev_year_precip
survival.precip.lm <- dat %>% 
  filter(!is.na(survival_perc),
         Aspect != "flat") %>% 
  ggplot(aes(x = Prev_year_precip, y = survival_perc)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  labs(x = "Previous year precip (mm)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. precip")
survival.precip.lm


## Survival: Not significant -----------------------------------------------

# Survival: linear regression by PlotSlope
survival.plotslope.lm <- dat %>% 
  filter(!is.na(survival_perc),
         Aspect != "flat") %>% 
  ggplot(aes(x = Prev_year_precip, y = survival_perc)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  labs(x = "Plot slope (\u00B0)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. plot slope")
survival.plotslope.lm

# Survival: linear regression by BG density and Prev_year_precip
survival.bgden.prevprecip.lm <- dat %>% 
  filter(!is.na(survival_perc),
         Aspect != "flat") %>% 
  ggplot(aes(x = BGDensity, y = survival_perc)) +
  geom_point(aes(color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  theme_bw() +
  labs(x = expression(paste("Density (individuals / ", m^2, ")")),
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. density")
survival.bgden.prevprecip.lm

# Survival: linear regression by herb cover and Prev_year_precip
survival.herb.prevprecip.lm <- dat %>% 
  filter(!is.na(survival_perc),
         Aspect != "flat") %>% 
  ggplot(aes(x = HerbCover, y = survival_perc)) +
  geom_point(aes(color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  labs(y = "Buffelgrass seedling survival (%)",
       x = "Native grass & forb cover (%)",
       title = "Buffegrass seedling survival vs. herb cover")
survival.herb.prevprecip.lm

# Survival: linear regression by shrub cover
survival.shrub.lm <- dat %>% 
  filter(!is.na(survival_perc),
         Aspect != "flat") %>% 
  ggplot(aes(x = ShrubCover, y = survival_perc)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  labs(y = "Buffelgrass seedling survival (%)",
       x = "Native shrub cover (%)",
       title = "Buffegrass seedling survival vs. shrub cover")
survival.shrub.lm




# Write out draft figures -------------------------------------------------

## Precipitation ----------------------------------------------------------

tiff("figures/2025-07_draft-figures/Precip_site.tiff", units = "in", height = 5, width = 7, res = 150)
precip.site
dev.off()


## Total change -----------------------------------------------------------

# Significant
# Total change vs. Prev_year_precip
tiff("figures/2025-07_draft-figures/Total-change_by-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.change.precip.lm
dev.off()

# Total change vs. Aspect
tiff("figures/2025-07_draft-figures/Total-change_by-aspect_boxplot.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.change.aspect
dev.off()

# Total change vs. BG density change
tiff("figures/2025-07_draft-figures/Total-change_by-BG-density-change_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.change.bgden.lm
dev.off()

# Total change vs. shrub cover change by Prev_year_precip (linear regression)
tiff("figures/2025-07_draft-figures/Total-change_by-shrub-cover-change-and-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.change.shrub.prevprecip.lm
dev.off()

# Total change vs. herb cover change by Prev_year_precip
tiff("figures/2025-07_draft-figures/Total-change_by-herb-cover-change-and-prev-year-precip.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.change.herb.prevprecip
dev.off()

# Total change vs. Site
tiff("figures/2025-07_draft-figures/Total-change_by-site_boxplot.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.change.site
dev.off()


# Not significant
# Total change vs. PlotSlope
tiff("figures/2025-07_draft-figures/Total-change_by-plot-slope_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.change.plotslope.lm
dev.off()

# Total change vs. herb cover change
tiff("figures/2025-07_draft-figures/Total-change_by-herb-cover-change_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.change.herb.lm
dev.off()

# Total change vs. BG density change by Prev_year_precip (linear regression)
tiff("figures/2025-07_draft-figures/Total-change_by-BG-density-change-and-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.change.bgden.prevprecip.lm
dev.off()



## Repro change -----------------------------------------------------------

# Significant
# Repro change by Aspect
tiff("figures/2025-07_draft-figures/Repro-change_by-aspect_boxplot.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.change.aspect
dev.off()

# Repro change vs. BG density change
tiff("figures/2025-07_draft-figures/Repro-change_by-BG-density-change_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.change.bgden.lm
dev.off()

# Repro change vs. shrub cover change
tiff("figures/2025-07_draft-figures/Repro-change_by-shrub-cover-change_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.change.shrub.lm
dev.off()

# Repro change vs. herb cover change
tiff("figures/2025-07_draft-figures/Repro-change_by-herb-cover-change_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.change.herb.lm
dev.off()

# Repro change vs. Site
tiff("figures/2025-07_draft-figures/Repro-change_by-site_boxplot.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.change.site
dev.off()


# Not significant
# Repro change vs. Prev_year_precip
tiff("figures/2025-07_draft-figures/Repro-change_by-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.change.precip.lm
dev.off()

# Repro change vs. PlotSlope
tiff("figures/2025-07_draft-figures/Repro-change_by-plot-slope_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.change.plotslope.lm
dev.off()

# Repro change vs. BG density change by Prev_year_precip
tiff("figures/2025-07_draft-figures/Repro-change_by-BG-density-change-and-prev-year-precip.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.change.bgden.prevprecip
dev.off()

# Repro change vs. shrub cover change by Prev_year_precip
tiff("figures/2025-07_draft-figures/Repro-change_by-shrub-cover-change-and-prev-year-precip.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.change.shrub.prevprecip
dev.off()

# Repro change vs. herb cover change by Prev_year_precip 
tiff("figures/2025-07_draft-figures/Repro-change_by-herb-cover-change-and-prev-year-precip.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.change.herb.prevprecip
dev.off()




## Buffelgrass density change ---------------------------------------------

# Significant
# BG density change vs. Prev_year_precip
tiff("figures/2025-07_draft-figures/BG-density-change_by-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.change.precip.lm
dev.off()

# BG density change by Aspect
tiff("figures/2025-07_draft-figures/BG-density-change_by-aspect_boxplot.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.change.aspect
dev.off()

# BG density change vs. shrub cover change by Prev_year_precip (scatterplot)
tiff("figures/2025-07_draft-figures/BG-density-change_by-shrub-cover-change-and-prev-year-precip.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.change.shrub.prevprecip
dev.off()

# BG density change by Site
tiff("figures/2025-07_draft-figures/BG-density-change_by-site_boxplot.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.change.site
dev.off()


# Not significant
# BG density change vs. PlotSlope
tiff("figures/2025-07_draft-figures/BG-density-change_by-plot-slope_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.change.plotslope.lm
dev.off()

# BG density change vs. shrub cover change
tiff("figures/2025-07_draft-figures/BG-density-change_by-shrub-cover-change_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.change.shrub.lm
dev.off()

## Buffelgrass cover change -----------------------------------------------

# Significant
# BG cover change vs. Prev_year_precip
tiff("figures/2025-07_draft-figures/BG-cover-change_by-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.change.precip.lm
dev.off()

# BG cover change by Aspect
tiff("figures/2025-07_draft-figures/BG-cover-change_by-aspect_boxplot.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.change.aspect
dev.off()

# BG cover change vs. shrub cover change
tiff("figures/2025-07_draft-figures/BG-cover-change_by-shrub-cover-change_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.change.shrub.lm
dev.off()

# BG cover change by Site
tiff("figures/2025-07_draft-figures/BG-cover-change_by-site_boxplot.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.change.site
dev.off()


# Not significant
# BG cover change vs. PlotSlope
tiff("figures/2025-07_draft-figures/BG-cover-change_by-plot-slope_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.change.plotslope.lm
dev.off()

# BG cover change vs. shrub cover change by Prev_year_precip (scatterplot)
tiff("figures/2025-07_draft-figures/BG-cover-change_by-shrub-cover-change-and-prev-year-precip.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.change.shrub.prevprecip
dev.off()

# BG cover change vs. herb cover change by Prev_year_precip (linear regression)
tiff("figures/2025-07_draft-figures/BG-cover-change_by-herb-cover-change-and-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.change.herb.prevprecip.lm
dev.off()



## Survival ---------------------------------------------------------------

# Significant
# Survival vs. Prev_year_precip
tiff("figures/2025-07_draft-figures/Survival_by-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.precip.lm
dev.off()


# Not significant
# Survival vs. PlotSlope
tiff("figures/2025-07_draft-figures/Survival_by-plot-slope_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.plotslope.lm
dev.off()

# Survival vs. BG density by Prev_year_precip (linear regression)
tiff("figures/2025-07_draft-figures/Survival_by-BG-density-and-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.bgden.prevprecip.lm
dev.off()

# Survival vs. shrub cover
tiff("figures/2025-07_draft-figures/Survival_by-shrub-cover_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.shrub.lm
dev.off()

# Survival vs. herb cover by Prev_year_precip (linear regression)
tiff("figures/2025-07_draft-figures/Survival_by-herb-cover-and-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.herb.prevprecip.lm
dev.off()


save.image("RData/05.2_draft-figs-for-lm-5.3.RData")
