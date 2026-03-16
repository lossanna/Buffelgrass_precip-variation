# Created: 2026-02-19
# Updated: 2026-03-16

# Purpose: Create supplemental figures for publishing (revision1.2).

# Aspect graphs have to use ggeffects becvause for some reason the modelbased version
#   doesn't work.

library(tidyverse)
library(insight)
library(modelbased)
library(ggeffects)
library(viridis)
library(ggpubr)

# Load data ---------------------------------------------------------------

load("RData/publish2.2_data-and-models.RData")
dat.survival.raw <- dat.survival
dat <- read_csv("data/publish2.2/all-data.csv")


# Data wrangling ----------------------------------------------------------

# Need 3 datasets:
#   1. Data for graphing observed data
#   2. Data to construct datagrid with predictions & CI
#   3. Data to construct datagrid with unscaled explanatory variable (to be added to #2)


# 1. Dataset for graphing (response and unscaled explanatory variables)
#   Culm change - total & repro
dat.culm <- culm.change %>% 
  select(Change_TotalCulms, Change_ReproductiveCulms, 
         Prev_year_precip, Aspect, PlotSlope, Change_BGDensity, Change_ShrubCover, Change_HerbCover,
         Init_BGDensity, Init_ShrubCover, Init_HerbCover)

#   Plot change - density & cover
dat.plot <- plot.change %>% 
  select(Change_BGDensity, Change_BGCover, 
         Prev_year_precip, Aspect, PlotSlope, Change_ShrubCover, Change_HerbCover,
         Init_BGDensity, Init_ShrubCover, Init_HerbCover) 

#   Survival  
dat.survival <- dat.survival.raw %>%
  select(remaining_toothpicks, seedlings_surviving,
         Prev_year_precip, Aspect, PlotSlope, BGDensity, ShrubCover, HerbCover,
         Prev_year_precip_scaled, PlotSlope_scaled, BGDensity_scaled,
         ShrubCover_scaled, HerbCover_scaled) %>% 
  mutate(survival_prop = seedlings_surviving / remaining_toothpicks)


# 2. Dataset for constructing datagrid with prediction & CI (scaled explanatory variables only)
#   Culm change - total & repro 
dat.culm.ex <- culm.change %>% 
  select(Prev_year_precip_scaled, Aspect, PlotSlope_scaled, 
         Change_BGDensity_scaled, Change_ShrubCover_scaled, Change_HerbCover_scaled,
         Init_BGDensity_scaled, Init_ShrubCover_scaled, Init_HerbCover_scaled)

#   Plot change - density & cover
dat.plot.ex <- plot.change %>% 
  select(Prev_year_precip_scaled,
         Aspect, PlotSlope_scaled, Change_ShrubCover_scaled, Change_HerbCover_scaled,
         Init_BGDensity_scaled, Init_ShrubCover_scaled, Init_HerbCover_scaled)

#   Survival  
dat.survival.ex <- dat.survival.raw %>%
  select(Prev_year_precip_scaled, Aspect,
         PlotSlope_scaled, BGDensity_scaled, ShrubCover_scaled, HerbCover_scaled)


# 3. Dataset for constructing datagrid with unscaled variables to match graph (unscaled explanatory variables only)
#   Culm change - total & repro
dat.culm.unscaled <- culm.change %>% 
  select(Prev_year_precip, Aspect, PlotSlope, 
         Change_BGDensity, Change_ShrubCover, Change_HerbCover,
         Init_BGDensity, Init_ShrubCover, Init_HerbCover)

#   Plot change - density & cover
dat.plot.unscaled <- plot.change %>% 
  select(Prev_year_precip, Aspect, PlotSlope, Change_ShrubCover, Change_HerbCover,
         Init_BGDensity, Init_ShrubCover, Init_HerbCover)

#   Survival 
dat.survival.unscaled <- dat.survival.raw %>%
  select(Prev_year_precip, Aspect, PlotSlope, BGDensity, ShrubCover, HerbCover)



# Figure S1: Precip conditions by site ------------------------------------

# Create df of dat for with precip cols
dat.precip <- dat %>% 
  select(Year, Site, Transect, Plot, Prev_year_precip, Perc_dev, MAP) %>% 
  distinct(.keep_all = TRUE) 

# Create df of average for precip by site
precip.avg.site <- dat.precip %>% 
  group_by(Year, Site) %>% 
  summarise(Perc_dev_avg = mean(Perc_dev),
            Prev_year_precip_avg = mean(Prev_year_precip),
            .groups = "keep")

map.avg <- dat.precip %>% 
  select(Site, Transect, MAP) %>% 
  distinct(.keep_all = TRUE) %>% 
  group_by(Site) %>% 
  summarise(MAP_avg = mean(MAP))

# Line graph of averages across sites
precip.site.all <- precip.avg.site %>% 
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
precip.site.all


# Figure S2: Repro culm, precip * shrub -----------------------------------

# Generate prediction and add unscaled variable - 9 precip levels to get mean
insight.repro.shrub.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 5) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.repro.shrub.precip$Predicted <- get_predicted(repro, insight.repro.shrub.precip)
unscaled.shrub.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 5) %>% 
  get_datagrid("Prev_year_precip", length = 9, numerics = "all") %>% 
  arrange(Change_ShrubCover) %>% 
  distinct(.keep_all = TRUE)
insight.repro.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip$Change_ShrubCover
insight.repro.shrub.precip$Prev_year_precip <- unscaled.shrub.precip$Prev_year_precip
unique(insight.repro.shrub.precip$Prev_year_precip_scaled)
insight.repro.shrub.precip <- insight.repro.shrub.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.951, 0.021, 1.642))

# Graph
repro.shrub.precip <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_ReproductiveCulms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.repro.shrub.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive buffelgrass culm count"),
       x = expression(Delta ~ "Native shrub cover (%)"))
repro.shrub.precip



# Figure S3: Repro culm, precip * herb ------------------------------------

# Generate prediction and add unscaled variable - 9 precip levels to get mean
insight.repro.herb.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 5) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.repro.herb.precip$Predicted <- get_predicted(repro, insight.repro.herb.precip)
unscaled.herb.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 5) %>% 
  get_datagrid("Prev_year_precip", length = 9, numerics = "all") %>% 
  arrange(Change_HerbCover) %>% 
  distinct(.keep_all = TRUE)
insight.repro.herb.precip$Change_HerbCover <- unscaled.herb.precip$Change_HerbCover
insight.repro.herb.precip$Prev_year_precip <- unscaled.herb.precip$Prev_year_precip
unique(insight.repro.herb.precip$Prev_year_precip_scaled)
insight.repro.herb.precip <- insight.repro.herb.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.951, 0.021, 1.642))

# Graph
repro.herb.precip <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_ReproductiveCulms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.repro.herb.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Buffelgrass reproductive culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)")) 
repro.herb.precip



# Figure S4: Shrub/herb cover vs. precip ----------------------------------

# Shrub cover change vs. precip
shrub.change.precip <- plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_ShrubCover)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(y = expression(Delta ~ "Native shrub cover (%)"),
       x = "Previous year precipitation (mm)") +
  theme(plot.margin = margin(10, 10, 10, 20))
shrub.change.precip

# Herb cover change vs. precip
herb.change.precip <- plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_HerbCover)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(y = expression(Delta ~ "Native grass & forb cover (%)"),
       x = "Previous year precipitation (mm)") +
  theme(plot.margin = margin(10, 10, 10, 20))
herb.change.precip



# Figure S5: Total culm, initial BG density -------------------------------

# Generate prediction and add unscaled variable
insight.total.inbgden <- get_datagrid(dat.culm.ex, by = c("Init_BGDensity_scaled"),
                                      length = 100)
insight.total.inbgden$Change_TotalCulms <- get_predicted(total, insight.total.inbgden)
unscaled.inbgden <- get_datagrid(dat.culm.unscaled, by = "Init_BGDensity",
                                 length = 100) %>% 
  arrange(Init_BGDensity)
insight.total.inbgden$Init_BGDensity <- unscaled.inbgden$Init_BGDensity

# Graph
total.inbgden <- dat.culm %>% 
  ggplot(aes(x = Init_BGDensity, y = Change_TotalCulms)) +
  geom_point(alpha = 0.5) +
  geom_line(data = insight.total.inbgden,
            aes(y = Change_TotalCulms), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  labs(x = expression(paste("Initial buffelgrass density (individuals / ", m^2, ")")),
       y = expression(Delta ~ "Total buffelgrass culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
total.inbgden



# Figure S6: Repro culm, initial BG density -------------------------------

# Generate prediction and add unscaled variable
insight.repro.inbgden <- get_datagrid(dat.culm.ex, by = c("Init_BGDensity_scaled"),
                                      length = 100)
insight.repro.inbgden$Change_ReproductiveCulms <- get_predicted(repro, insight.repro.inbgden)
unscaled.inbgden <- get_datagrid(dat.culm.unscaled, by = "Init_BGDensity",
                                 length = 100) %>% 
  arrange(Init_BGDensity)
insight.repro.inbgden$Init_BGDensity <- unscaled.inbgden$Init_BGDensity

# Graph
repro.inbgden <- dat.culm %>% 
  ggplot(aes(x = Init_BGDensity, y = Change_ReproductiveCulms)) +
  geom_point(alpha = 0.5) +
  geom_line(data = insight.repro.inbgden,
            aes(y = Change_ReproductiveCulms), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  labs(x = expression(paste("Initial buffelgrass density (individuals / ", m^2, ")")),
       y = expression(Delta ~ "Buffelgrass reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
repro.inbgden



# Figure S7: Repro, aspect ------------------------------------------------

# Generate prediction & CI
gg.repro.aspect <- predict_response(repro, terms = "Aspect") %>% 
  rename(Aspect = x,
         Change_ReproductiveCulms = predicted)

# Graph (ggeffects version)
repro.aspect <- dat.culm %>% 
  ggplot(aes(x = Aspect, y = Change_ReproductiveCulms)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3) +
  geom_point(data = gg.repro.aspect,
             aes(x = Aspect, y = Change_ReproductiveCulms),
             color = "purple3",
             size = 2.5,
             shape = 15) +
  theme_bw() +
  labs(y = expression(Delta ~ "Buffelgrass reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
repro.aspect


# Figure S8: Density, aspect ----------------------------------------------

# Generate prediction & CI
gg.bgden.aspect <- predict_response(bgden, terms = "Aspect") %>% 
  rename(Aspect = x,
         Change_BGDensity = predicted)

# Graph 
bgden.aspect <- dat.plot %>% 
  ggplot(aes(x = Aspect, y = Change_BGDensity)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3) +
  geom_point(data = gg.bgden.aspect,
             aes(x = Aspect, y = Change_BGDensity),
             color = "purple3",
             size = 2.5,
             shape = 15) +
  theme_bw() +
  labs(y = expression(paste(Delta ~ "Buffelgrass density (individuals / ", m^2, ")"))) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
bgden.aspect



# Figure S9: Cover, aspect ------------------------------------------------

# Generate prediction & CI
gg.bgcov.aspect <- predict_response(bgcov, terms = "Aspect") %>% 
  rename(Aspect = x,
         Change_BGCover = predicted)

# Graph 
bgcov.aspect <- dat.plot %>% 
  ggplot(aes(x = Aspect, y = Change_BGCover)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3) +
  geom_point(data = gg.bgcov.aspect,
             aes(x = Aspect, y = Change_BGCover),
             color = "purple3",
             size = 2.5,
             shape = 15) +
  theme_bw() +
  labs(y = expression(Delta ~ "Buffelgrass cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
bgcov.aspect


# Figure S10: Density vs. cover -------------------------------------------

density.cover <- plot.change %>% 
  ggplot(aes(x = Change_BGCover, y = Change_BGDensity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(x = expression(Delta ~ "Buffelgrass cover (%)"),
       y = expression(Delta ~ paste("Buffelgrass density (individuals / ", m^2, ")")))
density.cover



# Figure S11: Total vs. cover ---------------------------------------------

total.cover <- culm.change %>% 
  ggplot(aes(x = Change_BGCover, y = Change_TotalCulms)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(y = expression(Delta ~ "Total buffelgrass culm count"),
       x = expression(Delta ~ "Buffelgrass cover (%)"))
total.cover


# Figure S12: Survival, BG density ----------------------------------------

# Generate prediction and add scaled variable
insight.survival.bgden <- get_datagrid(dat.survival.ex, by = c("BGDensity_scaled"),
                                       length = 50)
insight.survival.bgden$survival_prop <- get_predicted(survival, insight.survival.bgden)
unscaled.bgden <- get_datagrid(dat.survival.unscaled, by = "BGDensity",
                               length = 50) %>%
  arrange(BGDensity)
insight.survival.bgden$BGDensity <- unscaled.bgden$BGDensity

# Graph
survival.bgden <- dat.survival %>%
  ggplot(aes(x = BGDensity, y = survival_prop)) +
  geom_point() +
  geom_line(data = insight.survival.bgden,
            aes(y = survival_prop), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  labs(x = expression(paste("Buffelgrass density (individuals / ", m^2, ")")),
       y = "Proportion of surviving buffelgrass seedlings")
survival.bgden


# Figure S13: Survival, shrub ---------------------------------------------

# Generate prediction and add scaled variable
insight.survival.shrub <- get_datagrid(dat.survival.ex, by = c("ShrubCover_scaled"),
                                       length = 50)
insight.survival.shrub$survival_prop <- get_predicted(survival, insight.survival.shrub) 
unscaled.shrub <- get_datagrid(dat.survival.unscaled, by = "ShrubCover",
                               length = 50) %>%
  arrange(ShrubCover)
insight.survival.shrub$ShrubCover <- unscaled.shrub$ShrubCover

# Graph
survival.shrub <- dat.survival %>%
  ggplot(aes(x = ShrubCover, y = survival_prop)) +
  geom_point() +
  geom_line(data = insight.survival.shrub,
            aes(y = survival_prop), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  labs(x = "Native shrub cover (%)",
       y = "Proportion of surviving buffelgrass seedlings")
survival.shrub


# Save --------------------------------------------------------------------

# Graphs only
save(precip.site.all, repro.shrub.precip, repro.herb.precip, 
     shrub.change.precip, herb.change.precip,
     total.inbgden, repro.inbgden, repro.aspect,
     bgden.aspect, bgcov.aspect,
     density.cover, total.cover,
     survival.bgden, survival.shrub,
     file = "RData/publish2.2_figsS1-S13.RData")


save.image("RData/publish2.2_supp-figures.RData")
