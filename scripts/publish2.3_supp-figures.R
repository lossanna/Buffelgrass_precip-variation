# Created: 2026-04-02
# Updated: 2026-04-02

# Purpose: Create supplemental figures for publishing (revision1.3).

library(tidyverse)
library(insight)
library(viridis)
library(ggpubr)

# Load data ---------------------------------------------------------------

load("RData/publish2.3_data-and-models.RData")
survival.dat <- dat.survival
dat <- read_csv("data/publish2.3/all-data.csv")


# Data wrangling ----------------------------------------------------------

# Need 3 datasets:
#   1. Data for graphing observed data
#   2. Data to construct datagrid with predictions & CI
#   3. Data to construct datagrid with unscaled explanatory variable (to be added to #2)


# 1. Dataset for graphing (response and unscaled explanatory variables)
#   Culm change - total & repro
dat.culm <- culm.change %>% 
  select(Change_TotalCulms, Change_ReproductiveCulms, 
         Prev_year_precip, PlotSlope, Change_BGDensity, Change_ShrubCover, Change_HerbCover,
         Init_BGDensity, Init_ShrubCover, Init_HerbCover)

#   Plot change - density & cover
dat.bgden <- plot.change %>% 
  select(Change_BGDensity,  
         Prev_year_precip, PlotSlope, Change_ShrubCover, Change_HerbCover,
         Init_BGDensity, Init_ShrubCover, Init_HerbCover) 

dat.bgcov <- plot.change %>% 
  select(Change_BGCover, 
         Prev_year_precip, PlotSlope, Change_ShrubCover, Change_HerbCover,
         Init_BGCover, Init_ShrubCover, Init_HerbCover) 

#   Survival  
dat.survival <- survival.dat %>%
  select(remaining_toothpicks, seedlings_surviving,
         Prev_year_precip, PlotSlope, BGDensity, ShrubCover, HerbCover,
         Prev_year_precip_scaled, PlotSlope_scaled, BGDensity_scaled,
         ShrubCover_scaled, HerbCover_scaled) %>% 
  mutate(survival_prop = seedlings_surviving / remaining_toothpicks)


# 2. Dataset for constructing datagrid with prediction & CI (scaled explanatory variables only)
#   Culm change - total & repro 
dat.culm.ex <- culm.change %>% 
  select(Prev_year_precip_scaled, PlotSlope_scaled, 
         Change_BGDensity_scaled, Change_ShrubCover_scaled, Change_HerbCover_scaled,
         Init_BGDensity_scaled, Init_ShrubCover_scaled, Init_HerbCover_scaled)

#   Plot change - density & cover
dat.bgden.ex <- plot.change %>% 
  select(Prev_year_precip_scaled,
         PlotSlope_scaled, Change_ShrubCover_scaled, Change_HerbCover_scaled,
         Init_BGDensity_scaled, Init_ShrubCover_scaled, Init_HerbCover_scaled)

dat.bgcov.ex <- plot.change %>% 
  select(Prev_year_precip_scaled,
         PlotSlope_scaled, Change_ShrubCover_scaled, Change_HerbCover_scaled,
         Init_BGCover_scaled, Init_ShrubCover_scaled, Init_HerbCover_scaled)

#   Survival  
dat.survival.ex <- survival.dat %>%
  select(Prev_year_precip_scaled,
         PlotSlope_scaled, BGDensity_scaled, ShrubCover_scaled, HerbCover_scaled)


# 3. Dataset for constructing datagrid with unscaled variables to match graph (unscaled explanatory variables only)
#   Culm change - total & repro
dat.culm.unscaled <- culm.change %>% 
  select(Prev_year_precip, PlotSlope, 
         Change_BGDensity, Change_ShrubCover, Change_HerbCover,
         Init_BGDensity, Init_ShrubCover, Init_HerbCover)

#   Plot change - density & cover
dat.bgden.unscaled <- plot.change %>% 
  select(Prev_year_precip, PlotSlope, Change_ShrubCover, Change_HerbCover,
         Init_BGDensity, Init_ShrubCover, Init_HerbCover)

dat.bgcov.unscaled <- plot.change %>% 
  select(Prev_year_precip, PlotSlope, Change_ShrubCover, Change_HerbCover,
         Init_BGCover, Init_ShrubCover, Init_HerbCover)

#   Survival 
dat.survival.unscaled <- survival.dat %>%
  select(Prev_year_precip, PlotSlope, BGDensity, ShrubCover, HerbCover)



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



# Figure S2: Repro culm, herb ---------------------------------------------

# Generate prediction and add unscaled variable
insight.repro.herb <- get_datagrid(dat.culm.ex, by = c("Change_HerbCover_scaled"),
                                   length = 100)
insight.repro.herb$Change_ReproductiveCulms <- get_predicted(repro, insight.repro.herb)
unscaled.herb <- get_datagrid(dat.culm.unscaled, by = "Change_HerbCover",
                              length = 100) %>% 
  arrange(Change_HerbCover)
insight.repro.herb$Change_HerbCover <- unscaled.herb$Change_HerbCover

# Graph
repro.herb <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_ReproductiveCulms)) +
  geom_point(alpha = 0.35) +
  geom_line(data = insight.repro.herb,
            aes(y = Change_ReproductiveCulms), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  labs(y = expression(Delta ~ "Buffelgrass reproductive culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
repro.herb


# Figure S3: Shrub/herb cover vs. precip ----------------------------------

# Shrub cover change vs. precip
shrub.change.precip <- plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_ShrubCover)) +
  geom_point(alpha = 0.7) +
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
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(y = expression(Delta ~ "Native grass & forb cover (%)"),
       x = "Previous year precipitation (mm)") +
  theme(plot.margin = margin(10, 10, 10, 20))
herb.change.precip



# Figure S4: Repro culm, BG density ---------------------------------------

# Generate prediction and add unscaled variable
insight.total.bgden <- get_datagrid(dat.culm.ex, by = c("Change_BGDensity_scaled"),
                                    length = 100)
insight.total.bgden$Change_TotalCulms <- get_predicted(total, insight.total.bgden)
unscaled.bgden <- get_datagrid(dat.culm.unscaled, by = "Change_BGDensity",
                               length = 100) %>% 
  arrange(Change_BGDensity)
insight.total.bgden$Change_BGDensity <- unscaled.bgden$Change_BGDensity

# Graph
total.bgden <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_TotalCulms)) +
  geom_point(alpha = 0.35) +
  geom_line(data = insight.total.bgden,
            aes(y = Change_TotalCulms), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  labs(y = expression(Delta ~ "Total buffelgrass culm count"),
       x = expression(Delta ~ paste("Buffelgrass density (individuals / ", m^2, ")"))) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
total.bgden



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
  geom_point(alpha = 0.35) +
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
  geom_point(alpha = 0.35) +
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



# Figure S7: Cover, initial BG cover --------------------------------------

# Generate prediction and add unscaled variable 
insight.bgcov.inbgcov <- get_datagrid(dat.bgcov.ex, by = c("Init_BGCover_scaled"),
                                      length = 10)
insight.bgcov.inbgcov$Change_BGCover <- get_predicted(bgcov, insight.bgcov.inbgcov)
unscaled.inbgcov <- get_datagrid(dat.bgcov.unscaled, by = "Init_BGCover",
                                 length = 10) %>% 
  arrange(Init_BGCover) # idk why but this only produces 24 rows when asked for 100, so I just have to go with 10, which it can do
insight.bgcov.inbgcov$Init_BGCover <- unscaled.inbgcov$Init_BGCover

# Graph
bgcov.inbgcov <- dat.bgcov %>% 
  ggplot(aes(x = Init_BGCover, y = Change_BGCover)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.bgcov.inbgcov,
            aes(y = Change_BGCover), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  labs(y = expression(Delta ~ "Buffelgrass cover (%)"),
       x = expression("Initial buffelgrass cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
bgcov.inbgcov


# Figure S8: Density vs. cover --------------------------------------------

density.cover <- plot.change %>% 
  ggplot(aes(x = Change_BGCover, y = Change_BGDensity)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(x = expression(Delta ~ "Buffelgrass cover (%)"),
       y = expression(Delta ~ paste("Buffelgrass density (individuals / ", m^2, ")")))
density.cover


# Figure S9: Total vs. cover ----------------------------------------------

total.cover <- culm.change %>% 
  ggplot(aes(x = Change_BGCover, y = Change_TotalCulms)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(y = expression(Delta ~ "Total buffelgrass culm count"),
       x = expression(Delta ~ "Buffelgrass cover (%)"))
total.cover



# Figure S10: Survival, BG density ----------------------------------------

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
  geom_point(alpha = 0.7) +
  geom_line(data = insight.survival.bgden,
            aes(y = survival_prop), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  labs(x = expression(paste("Buffelgrass density (individuals / ", m^2, ")")),
       y = "Proportion of surviving buffelgrass seedlings")
survival.bgden


# Figure S11: Survival, shrub ---------------------------------------------

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
  geom_point(alpha = 0.7) +
  geom_line(data = insight.survival.shrub,
            aes(y = survival_prop), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  labs(x = "Native shrub cover (%)",
       y = "Proportion of surviving buffelgrass seedlings")
survival.shrub



# Save --------------------------------------------------------------------

# Graphs only
save(precip.site.all, repro.herb,
     shrub.change.precip, herb.change.precip,
     total.bgden,
     total.inbgden, repro.inbgden, 
     bgcov.inbgcov,
     density.cover, total.cover,
     survival.bgden, survival.shrub,
     file = "RData/publish2.3_figsS1-S11.RData")


save.image("RData/publish2.3_supp-figures.RData")
