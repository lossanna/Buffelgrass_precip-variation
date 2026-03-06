# Created: 2026-03-02
# Updated: 2026-03-06

# Purpose: Create main figures for publishing (both low and high quality versions) [revision 1.2].

library(tidyverse)
library(insight)
library(viridis)
library(ggpubr)
library(ggeffects)

# Load data ---------------------------------------------------------------

load("RData/publish2.2_data-and-models.RData")
survival.dat <- dat.survival


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
dat.survival <- survival.dat %>% 
  select(survival_prop, 
         Prev_year_precip, Aspect, PlotSlope, BGDensity, ShrubCover, HerbCover)


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
dat.survival.ex <- survival.dat %>% 
  select(Prev_year_precip_scaled, Aspect,
         PlotSlope_scaled, ShrubCover_scaled, HerbCover_scaled,
         BGDensity_scaled)


# 3. Dataset for constructing datagrid with unscaled variables to match graph (unscaled explanatory variables only)
#   Culm change - total & repro
dat.culm.unscaled <- culm.change %>% 
  select(Prev_year_precip, Aspect, PlotSlope, Change_BGDensity, Change_ShrubCover, Change_HerbCover,
         Init_BGDensity, Init_ShrubCover, Init_HerbCover)

#   Plot change - density & cover
dat.plot.unscaled <- plot.change %>% 
  select(Prev_year_precip, Aspect, PlotSlope, Change_ShrubCover, Change_HerbCover,
         Init_BGDensity, Init_ShrubCover, Init_HerbCover)

#   Survival 
dat.survival.unscaled <- survival.dat %>% 
  select(Prev_year_precip, Aspect, PlotSlope, BGDensity, ShrubCover, HerbCover)



# Figure 1: Total culm, precip * shrub/herb/density -----------------------

## Precip * shrub ---------------------------------------------------------

# Generate prediction and add unscaled variable - 9 precip levels to get mean
insight.total.shrub.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 5) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.total.shrub.precip$Predicted <- get_predicted(total, insight.total.shrub.precip)
unscaled.shrub.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 5) %>% 
  get_datagrid("Prev_year_precip", length = 9, numerics = "all") %>% 
  arrange(Change_ShrubCover) %>% 
  distinct(.keep_all = TRUE)
insight.total.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip$Change_ShrubCover
insight.total.shrub.precip$Prev_year_precip <- unscaled.shrub.precip$Prev_year_precip
unique(insight.total.shrub.precip$Prev_year_precip_scaled)
insight.total.shrub.precip <- insight.total.shrub.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.951, 0.021, 1.642))

# Graph
total.shrub.precip <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_TotalCulms,
             color = Prev_year_precip)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.total.shrub.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.2) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total buffelgrass culm count"),
       x = expression(Delta ~ "Native shrub cover (%)")) +
  theme(plot.margin = margin(10, 10, 10, 20))
total.shrub.precip



## Precip * herb ----------------------------------------------------------

# Generate prediction and add unscaled variable - 9 precip levels to get mean
insight.total.herb.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 5) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.total.herb.precip$Predicted <- get_predicted(total, insight.total.herb.precip)
unscaled.herb.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 5) %>% 
  get_datagrid("Prev_year_precip", length = 9, numerics = "all") %>% 
  arrange(Change_HerbCover) %>% 
  distinct(.keep_all = TRUE)
insight.total.herb.precip$Change_HerbCover <- unscaled.herb.precip$Change_HerbCover
insight.total.herb.precip$Prev_year_precip <- unscaled.herb.precip$Prev_year_precip
unique(insight.total.herb.precip$Prev_year_precip_scaled)
insight.total.herb.precip <- insight.total.herb.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.951, 0.021, 1.642))

# Graph
total.herb.precip <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_TotalCulms,
             color = Prev_year_precip)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.total.herb.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.2) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total buffelgrass culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)")) +
  theme(plot.margin = margin(10, 10, 10, 20))
total.herb.precip


## Precip * density -------------------------------------------------------

# Generate prediction and add unscaled variable - 9 precip levels to get mean
insight.total.bgden.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_BGDensity_scaled", "Prev_year_precip_scaled"), length = 5) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_BGDensity_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.total.bgden.precip$Predicted <- get_predicted(total, insight.total.bgden.precip)
unscaled.bgden.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_BGDensity", "Prev_year_precip"), length = 5) %>% 
  get_datagrid("Prev_year_precip", length =  9, numerics = "all") %>% 
  arrange(Change_BGDensity) %>% 
  distinct(.keep_all = TRUE)
insight.total.bgden.precip$Change_BGDensity <- unscaled.bgden.precip$Change_BGDensity
insight.total.bgden.precip$Prev_year_precip <- unscaled.bgden.precip$Prev_year_precip
unique(insight.total.bgden.precip$Prev_year_precip_scaled)
insight.total.bgden.precip <- insight.total.bgden.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.951, 0.021, 1.642))

# Graph
total.bgden.precip <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_TotalCulms,
             color = Prev_year_precip)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.total.bgden.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.2) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total buffelgrass culm count"),
       x = expression(Delta ~ paste("Buffelgrass density (individuals / ", m^2, ")"))) +
  theme(plot.margin = margin(10, 10, 10, 20))
total.bgden.precip


## Combine ----------------------------------------------------------------

ggarrange(total.shrub.precip, total.herb.precip, total.bgden.precip,
          ncol = 2, nrow = 2,
          labels = c("(A)", "(B)", "(C)"),
          common.legend = TRUE, legend = "right")



# Figure 2: Total culm, plot slope ----------------------------------------

# Generate prediction and add unscaled variable
insight.total.slope <- get_datagrid(dat.culm.ex, by = c("PlotSlope_scaled"),
                                    length = 100)
insight.total.slope$Change_TotalCulms <- get_predicted(total, insight.total.slope)
unscaled.slope <- get_datagrid(dat.culm.unscaled, by = "PlotSlope",
                               length = 100) %>% 
  arrange(PlotSlope)
insight.total.slope$PlotSlope <- unscaled.slope$PlotSlope

# Graph
total.slope <- dat.culm %>% 
  ggplot(aes(x = PlotSlope, y = Change_TotalCulms)) +
  geom_point(alpha = 0.5,
             stroke = NA,
             size = 2) +
  geom_line(data = insight.total.slope,
            aes(y = Change_TotalCulms), linewidth = 1.2,
            color = "purple3") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  labs(y = expression(Delta ~ "Total buffelgrass culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
total.slope



# Figure 3: Total culm, aspect --------------------------------------------

# Generate prediction & CI
gg.total.aspect <- predict_response(total, terms = "Aspect") %>% 
  rename(Aspect = x,
         Change_TotalCulms = predicted)

# Graph (ggeffects version)
total.aspect <- dat.culm %>% 
  ggplot(aes(x = Aspect, y = Change_TotalCulms)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.25,
              stroke = NA,
              size = 2) +
  geom_point(data = gg.total.aspect,
             aes(x = Aspect, y = Change_TotalCulms),
             color = "purple3",
             size = 2,
             shape = 15) +
  theme_bw() +
  labs(y = expression(Delta ~ "Total buffelgrass culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
total.aspect


# Figure 4: Precip for density, cover, survival ---------------------------

# BG density
#   Generate prediction and add unscaled variable 
insight.bgden.precip <- get_datagrid(dat.plot.ex, by = c("Prev_year_precip_scaled"),
                                     length = 100)
insight.bgden.precip$Change_BGDensity <- get_predicted(bgden, insight.bgden.precip)
unscaled.precip <- get_datagrid(dat.plot.unscaled, by = "Prev_year_precip",
                                length = 100) %>% 
  arrange(Prev_year_precip)
insight.bgden.precip$Prev_year_precip <- unscaled.precip$Prev_year_precip

#   Graph 
bgden.precip <- dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGDensity)) +
  geom_point() +
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
insight.bgcov.precip <- get_datagrid(dat.plot.ex, by = c("Prev_year_precip_scaled"),
                                     length = 100)
insight.bgcov.precip$Change_BGCover <- get_predicted(bgcov, insight.bgcov.precip)
unscaled.precip <- get_datagrid(dat.plot.unscaled, by = "Prev_year_precip",
                                length = 100) %>% 
  arrange(Prev_year_precip)
insight.bgcov.precip$Prev_year_precip <- unscaled.precip$Prev_year_precip

#   Graph
bgcov.precip <- dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGCover)) +
  geom_point() +
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
  geom_point() +
  geom_line(data = insight.survival.precip,
            aes(y = survival_prop), linewidth = 1.3,
            color = "purple3") +
  theme_bw() +
  labs(x = "Previous year precipitation (mm)",
       y = "Proportion of surviving seedlings") +
  theme(plot.margin = margin(10, 10, 10, 30))
survival.precip 



# Figure 5: Density, precip * shrub ---------------------------------------

# Generate prediction and add unscaled variable - 12 levels to get mean
insight.bgden.shrub.precip <- dat.plot.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 12, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.bgden.shrub.precip$Predicted <- get_predicted(bgden, insight.bgden.shrub.precip)
unscaled.shrub.precip <- dat.plot.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 12, numerics = "all") %>% 
  arrange(Change_ShrubCover) %>% 
  distinct(.keep_all = TRUE)
insight.bgden.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip$Change_ShrubCover
insight.bgden.shrub.precip$Prev_year_precip <- unscaled.shrub.precip$Prev_year_precip
unique(insight.bgden.shrub.precip$Prev_year_precip_scaled) 
insight.bgden.shrub.precip <- insight.bgden.shrub.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.947, -0.006, 1.640))


# Graph
bgden.shrub.precip <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGDensity,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.bgden.shrub.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.3) +
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
       x = expression(Delta ~ "Native shrub cover (%)"))
bgden.shrub.precip



# Write out figures -------------------------------------------------------

# Figure 1
tiff("figures/publish2.2-figures/Figure1_150dpi.tiff",
     units = "in", height = 8, width = 8, res = 150)
ggarrange(total.shrub.precip, total.herb.precip, total.bgden.precip,
          ncol = 2, nrow = 2,
          labels = c("(A)", "(B)", "(C)"),
          common.legend = TRUE, legend = "right")
dev.off()
tiff("figures/publish2.2-figures/Figure1_600dpi.tiff",
     units = "in", height = 8, width = 8, res = 600)
ggarrange(total.shrub.precip, total.herb.precip, total.bgden.precip,
          ncol = 2, nrow = 2,
          labels = c("(A)", "(B)", "(C)"),
          common.legend = TRUE, legend = "right")
dev.off()

# Figure 2
tiff("figures/publish2.2-figures/Figure2_150dpi.tiff",
     units = "in", height = 3, width = 4, res = 150)
total.slope
dev.off()
tiff("figures/publish2.2-figures/Figure2_600dpi.tiff",
     units = "in", height = 3, width = 4, res = 600)
total.slope
dev.off()

# Figure 3
tiff("figures/publish2.2-figures/Figure3_150dpi.tiff",
     units = "in", height = 3, width = 4, res = 150)
total.aspect
dev.off()
tiff("figures/publish2.2-figures/Figure3_600dpi.tiff",
     units = "in", height = 3, width = 4, res = 600)
total.aspect
dev.off()

# Figure 4
tiff("figures/publish2.2-figures/Figure4_165dpi.tiff",
     units = "in", height = 6, width = 8, res = 165)
ggarrange(bgden.precip, bgcov.precip, survival.precip,
          ncol = 2, nrow = 2,
          labels = c("(A)", "(B)", "(C)"))
dev.off()
tiff("figures/publish2.2-figures/Figure4_600dpi.tiff",
     units = "in", height = 6, width = 8, res = 600)
ggarrange(bgden.precip, bgcov.precip, survival.precip,
          ncol = 2, nrow = 2,
          labels = c("(A)", "(B)", "(C)"))
dev.off()

# Figure 5
tiff("figures/publish2.2-figures/Figure5_165dpi.tiff",
     units = "in", height = 4, width = 6, res = 165)
bgden.shrub.precip
dev.off()
tiff("figures/publish2.2-figures/Figure5_600dpi.tiff",
     units = "in", height = 4, width = 6, res = 600)
bgden.shrub.precip
dev.off()


save.image("RData/publish2.2_main-figures.RData")
