# Created: 2025-08-14
# Updated: 2025-09-16

# Purpose: Create supplemental figures for publishing (both low and high quality versions).

# See 07_draft-figs-for-lm-6.3.1.R for more details. Published figures will be insight
#   version only, not include CI, and precip interaction graphs will have mean precip line.

library(tidyverse)
library(insight)
library(viridis)
library(ggpubr)

# Load data ---------------------------------------------------------------

load("RData/publish_data-and-best-models.RData")


# Data wrangling ----------------------------------------------------------

# Need 3 datasets:
#   1. Data for graphing observed data
#   2. Data to construct datagrid with predictions & CI
#   3. Data to construct datagrid with unscaled explanatory variable (to be added to #2)


# 1. Dataset for graphing (response and unscaled explanatory variables)
#   Culm change - total & repro
dat.culm <- culm.change %>% 
  select(Change_TotalCulms, Change_ReproductiveCulms, 
         Prev_year_precip, Aspect, PlotSlope, Change_BGDensity, Change_ShrubCover, Change_HerbCover)

#   Plot change - density & cover
dat.plot <- plot.change %>% 
  select(Change_BGDensity, Change_BGCover, 
         Prev_year_precip, Aspect, PlotSlope, Change_ShrubCover, Change_HerbCover)

#   Survival  
dat.survival <- survival.dat %>% 
  select(Survival_transf, 
         Prev_year_precip, Aspect, PlotSlope, BGDensity, ShrubCover, HerbCover) %>% 
  mutate(Survival_transf2 = Survival_transf * 100)


# 2. Dataset for constructing datagrid with prediction & CI (scaled explanatory variables only)
#   Culm change - total & repro 
dat.culm.ex <- culm.change %>% 
  select(Prev_year_precip_scaled, Aspect, PlotSlope_scaled, 
         Change_BGDensity_scaled, Change_ShrubCover_scaled, Change_HerbCover_scaled)

#   Plot change - density & cover
dat.plot.ex <- plot.change %>% 
  select(Prev_year_precip_scaled,
         Aspect, PlotSlope_scaled, Change_ShrubCover_scaled, Change_HerbCover_scaled)

#   Survival  
dat.survival.ex <- survival.dat %>% 
  select(Prev_year_precip_scaled,
         PlotSlope_scaled, ShrubCover_scaled, HerbCover_scaled,
         BGDensity_scaled)


# 3. Dataset for constructing datagrid with unscaled variables to match graph (unscaled explanatory variables only)
#   Culm change - total & repro
dat.culm.unscaled <- culm.change %>% 
  select(Prev_year_precip, Aspect, PlotSlope, Change_BGDensity, Change_ShrubCover, Change_HerbCover)

#   Plot change - density & cover
dat.plot.unscaled <- plot.change %>% 
  select(Prev_year_precip, Aspect, PlotSlope, Change_ShrubCover, Change_HerbCover)

#   Survival 
dat.survival.unscaled <- survival.dat %>% 
  select(Prev_year_precip, PlotSlope, BGDensity, ShrubCover, HerbCover)



# Figure S1: Repro culm, precip * shrub -----------------------------------

# Generate prediction and add unscaled variable (use model 3, which includes precip*shrub interaction)
insight.repro.shrub.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 20, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled)
insight.repro.shrub.precip$Predicted <- get_predicted(repro_model3, insight.repro.shrub.precip)
unscaled.shrub.precip3200000 <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 20, numerics = "all") %>% 
  arrange(Change_ShrubCover)
insight.repro.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip3200000$Change_ShrubCover
insight.repro.shrub.precip$Prev_year_precip <- unscaled.shrub.precip3200000$Prev_year_precip
insight.repro.shrub.precip <- insight.repro.shrub.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.952, 0.003, 1.641))

# Graph, no CI (insight version)
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
  labs(y = expression(Delta ~ "Buffelgrass reproductive culm count"),
       x = expression(Delta ~ "Native shrub cover (%)"))
repro.shrub.precip




# Figure S2: Repro culms, precip * herb -----------------------------------

# Generate prediction and add unscaled variable
insight.repro.herb.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 20, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
insight.repro.herb.precip$Predicted <- get_predicted(repro_best.model, insight.repro.herb.precip)
unscaled.herb.precip3200000 <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 20, numerics = "all") %>% 
  arrange(Change_HerbCover)
insight.repro.herb.precip$Change_HerbCover <- unscaled.herb.precip3200000$Change_HerbCover
insight.repro.herb.precip$Prev_year_precip <- unscaled.herb.precip3200000$Prev_year_precip
insight.repro.herb.precip <- insight.repro.herb.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.952, 0.003, 1.641))

# Graph, no CI (insight version)
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



# Figure S3: Shrub/herb cover vs. precip ----------------------------------

# Shrub cover change vs. precip
shrub.change.precip <- plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_ShrubCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
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
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(y = expression(Delta ~ "Native grass & forb cover (%)"),
       x = "Previous year precipitation (mm)") +
  theme(plot.margin = margin(10, 10, 10, 20))
herb.change.precip



# Figure S4: BG cover, precip * shrub -------------------------------------

# Generate prediction and add unscaled variable 
insight.bgcov.shrub.precip <- dat.plot.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled)
insight.bgcov.shrub.precip$Predicted <- get_predicted(bgcov_best.model, insight.bgcov.shrub.precip)
unscaled.shrub.precip6561 <- dat.plot.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 9, numerics = "all") %>% 
  arrange(Change_ShrubCover)
insight.bgcov.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip6561$Change_ShrubCover
insight.bgcov.shrub.precip$Prev_year_precip <- unscaled.shrub.precip6561$Prev_year_precip
insight.bgcov.shrub.precip <- insight.bgcov.shrub.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.957, 0.005, 1.609))

# Graph, no CI, average precip (insight version)
bgcov.shrub.precip <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGCover,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.bgcov.shrub.precip,
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
  labs(y = expression(Delta ~ "Buffelgrass cover (%)"),
       x = expression(Delta ~ "Native shrub cover (%)"))
bgcov.shrub.precip




# Write out figures -------------------------------------------------------

# Figure S1
tiff("figures/publish-figures/FigureS1_150dpi.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.shrub.precip
dev.off()
tiff("figures/publish-figures/FigureS1_600dpi.tiff",
     units = "in", height = 4, width = 6, res = 600)
repro.shrub.precip
dev.off()

# Figure S2
tiff("figures/publish-figures/FigureS2_150dpi.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.herb.precip
dev.off()
tiff("figures/publish-figures/FigureS2_600dpi.tiff",
     units = "in", height = 4, width = 6, res = 600)
repro.herb.precip
dev.off()

# Figure S3
tiff("figures/publish-figures/FigureS3_150dpi.tiff",
     units = "in", height = 3.5, width = 9, res = 150)
ggarrange(shrub.change.precip, herb.change.precip,
          ncol = 2, nrow = 1,
          labels = c("(A)", "(B)"))
dev.off()
tiff("figures/publish-figures/FigureS3_600dpi.tiff",
     units = "in", height = 3.5, width = 9, res = 600)
ggarrange(shrub.change.precip, herb.change.precip,
          ncol = 2, nrow = 1,
          labels = c("(A)", "(B)"))
dev.off()

# Figure S4
tiff("figures/publish-figures/FigureS4_150dpi.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.shrub.precip
dev.off()
tiff("figures/publish-figures/FigureS4_600dpi.tiff",
     units = "in", height = 4, width = 6, res = 600)
bgcov.shrub.precip
dev.off()


# Save --------------------------------------------------------------------

# Graphs only
save(repro.shrub.precip, repro.herb.precip, shrub.change.precip, herb.change.precip,
     bgcov.shrub.precip,
     file = "RData/publish_figsS1-S4.RData")

save.image("RData/publish_supp-figures.RData")
