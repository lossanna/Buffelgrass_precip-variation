# Created: 2026-03-31
# Updated: 2026-04-02

# Purpose: Create main figures for publishing (revision 1.3).

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
  select(remaining_toothpicks, seedlings_surviving, survival_prop,
         Prev_year_precip, PlotSlope, BGDensity, ShrubCover, HerbCover,
         Prev_year_precip_scaled, PlotSlope_scaled, BGDensity_scaled,
         ShrubCover_scaled, HerbCover_scaled)


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



# Figure 1: Total culm, precip * shrub/herb -------------------------------

## Precip * shrub ---------------------------------------------------------

# Generate prediction and add unscaled variable - 9 precip levels to get mean
insight.total.shrub.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.total.shrub.precip$Predicted <- get_predicted(total, insight.total.shrub.precip)
unscaled.shrub.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 3) %>% 
  get_datagrid("Prev_year_precip", length = 9, numerics = "all") %>% 
  arrange(Change_ShrubCover) %>% 
  distinct(.keep_all = TRUE)
insight.total.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip$Change_ShrubCover
insight.total.shrub.precip$Prev_year_precip <- unscaled.shrub.precip$Prev_year_precip
unique(insight.total.shrub.precip$Prev_year_precip_scaled)
insight.total.shrub.precip <- insight.total.shrub.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.943, 0.033, 1.659))

# Graph
total.shrub.precip <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_TotalCulms,
             color = Prev_year_precip)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.total.shrub.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year precip (mm) ") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total buffelgrass culm count"),
       x = expression(Delta ~ "Native shrub cover (%)")) +
  theme(plot.margin = margin(10, 20, 10, 10))
total.shrub.precip


## Precip * herb ----------------------------------------------------------

# Generate prediction and add unscaled variable - 9 precip levels to get mean
insight.total.herb.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.total.herb.precip$Predicted <- get_predicted(total, insight.total.herb.precip)
unscaled.herb.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 3) %>% 
  get_datagrid("Prev_year_precip", length = 9, numerics = "all") %>% 
  arrange(Change_HerbCover) %>% 
  distinct(.keep_all = TRUE)
insight.total.herb.precip$Change_HerbCover <- unscaled.herb.precip$Change_HerbCover
insight.total.herb.precip$Prev_year_precip <- unscaled.herb.precip$Prev_year_precip
unique(insight.total.herb.precip$Prev_year_precip_scaled)
insight.total.herb.precip <- insight.total.herb.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.943, 0.033, 1.659))

# Graph
total.herb.precip <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_TotalCulms,
             color = Prev_year_precip)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.total.herb.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year precip (mm) ") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total buffelgrass culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)")) +
  theme(plot.margin = margin(10, 20, 10, 10))
total.herb.precip


## Combine ----------------------------------------------------------------

ggarrange(total.shrub.precip, total.herb.precip, 
          ncol = 2, nrow = 1,
          labels = c("(A)", "(B)"),
          common.legend = TRUE, legend = "bottom")




# Figure 2: Total/repro culm, slope * shrub -------------------------------

## Total ------------------------------------------------------------------

# Generate prediction and add unscaled variable - 10 slope levels to get mean
insight.total.shrub.slope <- dat.culm.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 10, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.total.shrub.slope$Predicted <- get_predicted(total, insight.total.shrub.slope)
unscaled.shrub.slope <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "PlotSlope"), length = 3) %>% 
  get_datagrid("PlotSlope", length = 10, numerics = "all") %>% 
  arrange(Change_ShrubCover) %>% 
  distinct(.keep_all = TRUE)
insight.total.shrub.slope$Change_ShrubCover <- unscaled.shrub.slope$Change_ShrubCover
insight.total.shrub.slope$PlotSlope <- unscaled.shrub.slope$PlotSlope
unique(insight.total.shrub.slope$PlotSlope_scaled)
insight.total.shrub.slope <- insight.total.shrub.slope %>% 
  filter(PlotSlope_scaled %in% c(-2.268, -0.054, 2.713))

# Graph
total.shrub.slope <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_TotalCulms,
             color = PlotSlope)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.total.shrub.slope,
            aes(y = Predicted, group = PlotSlope), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "plasma", direction = -1,
                      name = "Plot slope (\u00B0)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total buffelgrass culm count"),
       x = expression(Delta ~ "Native shrub cover (%)")) +
  theme(plot.margin = margin(10, 20, 10, 10))
total.shrub.slope


## Repro ------------------------------------------------------------------

# Generate prediction and add unscaled variable - 10 slope levels to get mean
insight.repro.shrub.slope <- dat.culm.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 10, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.repro.shrub.slope$Predicted <- get_predicted(repro, insight.repro.shrub.slope)
unscaled.shrub.slope <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "PlotSlope"), length = 3) %>% 
  get_datagrid("PlotSlope", length = 10, numerics = "all") %>% 
  arrange(Change_ShrubCover) %>% 
  distinct(.keep_all = TRUE)
insight.repro.shrub.slope$Change_ShrubCover <- unscaled.shrub.slope$Change_ShrubCover
insight.repro.shrub.slope$PlotSlope <- unscaled.shrub.slope$PlotSlope
unique(insight.repro.shrub.slope$PlotSlope_scaled)
insight.repro.shrub.slope <- insight.repro.shrub.slope %>% 
  filter(PlotSlope_scaled %in% c(-2.268, -0.054, 2.713))

# Graph
repro.shrub.slope <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_ReproductiveCulms,
             color = PlotSlope)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.repro.shrub.slope,
            aes(y = Predicted, group = PlotSlope), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "plasma", direction = -1,
                      name = "Plot slope (\u00B0)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Buffelgrass reproductive culm count"),
       x = expression(Delta ~ "Native shrub cover (%)")) +
  theme(plot.margin = margin(10, 20, 10, 10))
repro.shrub.slope


## Combine ----------------------------------------------------------------

ggarrange(total.shrub.slope, repro.shrub.slope, 
          ncol = 2, nrow = 1,
          labels = c("(A)", "(B)"),
          common.legend = TRUE, legend = "bottom")



# Figure 3: Precip for density, cover, survival ---------------------------

# BG density
#   Generate prediction and add unscaled variable 
insight.bgden.precip <- get_datagrid(dat.bgden.ex, by = c("Prev_year_precip_scaled"),
                                     length = 100)
insight.bgden.precip$Change_BGDensity <- get_predicted(bgden, insight.bgden.precip)
unscaled.precip <- get_datagrid(dat.bgden.unscaled, by = "Prev_year_precip",
                                length = 100) %>% 
  arrange(Prev_year_precip)
insight.bgden.precip$Prev_year_precip <- unscaled.precip$Prev_year_precip

#   Graph 
bgden.precip <- dat.bgden %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGDensity)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.bgden.precip,
            aes(y = Change_BGDensity), linewidth = 1.3,
            color = "purple3") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Previous year precipitation (mm)",
       y = expression(paste(Delta ~ "Density (individuals / ", m^2, ")"))) +
  theme(plot.margin = margin(10, 10, 10, 20))
bgden.precip


# BG cover
#   Generate prediction and add scaled variable 
insight.bgcov.precip <- get_datagrid(dat.bgcov.ex, by = c("Prev_year_precip_scaled"),
                                     length = 100)
insight.bgcov.precip$Change_BGCover <- get_predicted(bgcov, insight.bgcov.precip)
unscaled.precip <- get_datagrid(dat.bgcov.unscaled, by = "Prev_year_precip",
                                length = 100) %>% 
  arrange(Prev_year_precip)
insight.bgcov.precip$Prev_year_precip <- unscaled.precip$Prev_year_precip

#   Graph
bgcov.precip <- dat.bgcov %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGCover)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.bgcov.precip,
            aes(y = Change_BGCover), linewidth = 1.3,
            color = "purple3") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Previous year precipitation (mm)",
       y = expression(Delta ~ "Cover (%)")) +
  theme(plot.margin = margin(10, 10, 10, 20))
bgcov.precip


# Survival
#   Generate prediction and add unscaled variable 
insight.survival.precip <- get_datagrid(dat.survival.ex, by = c("Prev_year_precip_scaled"),                                   
                                        length = 50)
insight.survival.precip$survival_prop <- get_predicted(survival, insight.survival.precip)
unscaled.precip <- get_datagrid(dat.survival.unscaled, by = "Prev_year_precip",
                                length = 50) %>% 
  arrange(Prev_year_precip)
insight.survival.precip$Prev_year_precip <- unscaled.precip$Prev_year_precip

#   Graph
survival.precip <- dat.survival %>% 
  ggplot(aes(x = Prev_year_precip, y = survival_prop)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.survival.precip,
            aes(y = survival_prop), linewidth = 1.3,
            color = "purple3") +
  theme_bw() +
  labs(x = "Previous year precipitation (mm)",
       y = "Proportion of surviving seedlings") +
  theme(plot.margin = margin(10, 10, 10, 30))
survival.precip 


# Combine
ggarrange(bgden.precip, bgcov.precip, survival.precip,
          ncol = 2, nrow = 2,
          labels = c("(A)", "(B)", "(C)"))



# Figure 4: BG density, precip * shrub ------------------------------------

# Generate prediction and add unscaled variable - 12 levels to get mean
insight.bgden.shrub.precip <- dat.bgden.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 12, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.bgden.shrub.precip$Predicted <- get_predicted(bgden, insight.bgden.shrub.precip)
unscaled.shrub.precip <- dat.bgden.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 12, numerics = "all") %>% 
  arrange(Change_ShrubCover) %>% 
  distinct(.keep_all = TRUE)
insight.bgden.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip$Change_ShrubCover
insight.bgden.shrub.precip$Prev_year_precip <- unscaled.shrub.precip$Prev_year_precip
unique(insight.bgden.shrub.precip$Prev_year_precip_scaled) 
insight.bgden.shrub.precip <- insight.bgden.shrub.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.941, 0.004, 1.657))

# Graph
bgden.shrub.precip <- dat.bgden %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGDensity,
             color = Prev_year_precip)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.bgden.shrub.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(paste(Delta ~ "Buffelgrass density (individuals / ", m^2, ")")),
       x = expression(Delta ~ "Native shrub cover (%)")) +
  theme(plot.margin = margin(10, 10, 10, 10))
bgden.shrub.precip




# Write out figures -------------------------------------------------------

# Figure 1
tiff("figures/publish2.3-figures/Figure1_600dpi.tiff",
     units = "in", height = 6, width = 8, res = 600)
ggarrange(total.shrub.precip, total.herb.precip, 
          ncol = 2, nrow = 1,
          labels = c("(A)", "(B)"),
          common.legend = TRUE, legend = "bottom")
dev.off()

# Figure 2
tiff("figures/publish2.3-figures/Figure2_600dpi.tiff",
     units = "in", height = 6, width = 8, res = 600)
ggarrange(total.shrub.slope, repro.shrub.slope, 
          ncol = 2, nrow = 1,
          labels = c("(A)", "(B)"),
          common.legend = TRUE, legend = "bottom")
dev.off()

# Figure 3
tiff("figures/publish2.3-figures/Figure3_600dpi.tiff",
     units = "in", height = 6, width = 8, res = 600)
ggarrange(bgden.precip, bgcov.precip, survival.precip,
          ncol = 2, nrow = 2,
          labels = c("(A)", "(B)", "(C)"))
dev.off()

# Figure 4
tiff("figures/publish2.3-figures/Figure4_600dpi.tiff",
     units = "in", height = 4, width = 6, res = 600)
bgden.shrub.precip
dev.off()

save.image("RData/publish2.3_main-figures.RData")
