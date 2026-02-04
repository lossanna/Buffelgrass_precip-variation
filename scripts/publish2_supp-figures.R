# Created: 2026-02-03
# Updated: 2026-02-03

# Purpose: Create supplemental figures for publishing (both low and high quality versions) [revision 1].

# Had to use ggeffects version for Aspect plots because modelbased version isn't working
#   for some reason (I think it has something to do with the nested random effect?)

library(tidyverse)
library(insight)
library(modelbased)
library(ggeffects)
library(viridis)
library(ggpubr)

# Load data ---------------------------------------------------------------

load("RData/publish2_data-and-models.RData")


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
  select(Prev_year_precip_scaled, Aspect,
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
  select(Prev_year_precip, Aspect, PlotSlope, BGDensity, ShrubCover, HerbCover)



# Figure S1: Repro culm, precip * shrub -----------------------------------

# Generate prediction and add unscaled variable - 20 precip levels to get mean
insight.repro.shrub.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 20, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.repro.shrub.precip$Predicted <- get_predicted(repro, insight.repro.shrub.precip)
unscaled.shrub.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 20, numerics = "all") %>% 
  arrange(Change_ShrubCover) %>% 
  distinct(.keep_all = TRUE)
insight.repro.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip$Change_ShrubCover
insight.repro.shrub.precip$Prev_year_precip <- unscaled.shrub.precip$Prev_year_precip
unique(insight.repro.shrub.precip$Prev_year_precip_scaled)
insight.repro.shrub.precip <- insight.repro.shrub.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.952, 0.003, 1.641))


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
  labs(y = expression(Delta ~ "Buffelgrass reproductive culm count"),
       x = expression(Delta ~ "Native shrub cover (%)"))
repro.shrub.precip



# Figure S2: Repro culms, precip * herb -----------------------------------

# Generate prediction and add unscaled variable 
insight.repro.herb.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 20, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.repro.herb.precip$Predicted <- get_predicted(repro, insight.repro.herb.precip)
unscaled.herb.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 20, numerics = "all") %>% 
  arrange(Change_HerbCover) %>% 
  distinct(.keep_all = TRUE)
insight.repro.herb.precip$Change_HerbCover <- unscaled.herb.precip$Change_HerbCover
insight.repro.herb.precip$Prev_year_precip <- unscaled.herb.precip$Prev_year_precip
unique(insight.repro.herb.precip$Prev_year_precip_scaled)
insight.repro.herb.precip <- insight.repro.herb.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.952, 0.003, 1.641))

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



# Figure S3: Shrub/herb cover vs. precip ----------------------------------

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



# Figure S4: Total culm, precip * density ---------------------------------

# Generate prediction and add unscaled variable
insight.total.bgden.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_BGDensity_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 20, numerics = "all") %>% 
  arrange(Change_BGDensity_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.total.bgden.precip$Predicted <- get_predicted(total, insight.total.bgden.precip)
unscaled.bgden.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_BGDensity", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 20, numerics = "all") %>% 
  arrange(Change_BGDensity) %>% 
  distinct(.keep_all = TRUE)
insight.total.bgden.precip$Change_BGDensity <- unscaled.bgden.precip$Change_BGDensity
insight.total.bgden.precip$Prev_year_precip <- unscaled.bgden.precip$Prev_year_precip
unique(insight.total.bgden.precip$Prev_year_precip_scaled)
insight.total.bgden.precip <- insight.total.bgden.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.952, 0.003, 1.641))

# Graph
total.bgden.precip <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_TotalCulms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.total.bgden.precip,
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
  labs(y = expression(Delta ~ "Buffelgrass total culm count"),
       x = expression(Delta ~ paste("Buffelgrass plot density (individuals / ", m^2, ")")))
total.bgden.precip



# Figure S5: Repro culm, precip * density ---------------------------------

# Generate prediction and add unscaled variable - 20 precip levels to get mean
insight.repro.bgden.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_BGDensity_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 20, numerics = "all") %>% 
  arrange(Change_BGDensity_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.repro.bgden.precip$Predicted <- get_predicted(repro, insight.repro.bgden.precip)
unscaled.bgden.precip <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_BGDensity", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 20, numerics = "all") %>% 
  arrange(Change_BGDensity) %>% 
  distinct(.keep_all = TRUE)
insight.repro.bgden.precip$Change_BGDensity <- unscaled.bgden.precip$Change_BGDensity
insight.repro.bgden.precip$Prev_year_precip <- unscaled.bgden.precip$Prev_year_precip
unique(insight.repro.bgden.precip$Prev_year_precip_scaled)
insight.repro.bgden.precip <- insight.repro.bgden.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.952, 0.003, 1.641))

# Graph
repro.bgden.precip <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_ReproductiveCulms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.repro.bgden.precip,
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
  labs(y = expression(Delta ~ "Buffelgrass reproductive culm count"),
       x = expression(Delta ~ paste("Buffelgrass plot density (individuals / ", m^2, ")")))
repro.bgden.precip



# Figure S6: Total vs. aspect ---------------------------------------------

# Generate prediction & CI
gg.total.aspect <- predict_response(total, terms = "Aspect") %>% 
  rename(Aspect = x,
         Change_TotalCulms = predicted)

# Graph (ggeffects version)
total.aspect <- dat.culm %>% 
  ggplot(aes(x = Aspect, y = Change_TotalCulms)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3) +
  geom_point(data = gg.total.aspect,
             aes(x = Aspect, y = Change_TotalCulms),
             color = "purple3",
             size = 2.5,
             shape = 15) +
  theme_bw() +
  labs(y = expression(Delta ~ "Buffelgrass total culm count"),
       x = "Aspect") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
total.aspect



# Figure S7: Repro vs. aspect ---------------------------------------------

# Generate prediction
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
  labs(y = expression(Delta ~ "Buffelgrass reproductive culm count"),
       x = "Aspect") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
repro.aspect



# Figure S8: Density vs. aspect -------------------------------------------

# Generate prediction 
gg.bgden.aspect <- predict_response(bgden, terms = "Aspect") %>% 
  rename(Aspect = x,
         Change_BGDensity = predicted)

# Graph (ggeffects version)
bgden.aspect <- dat.plot %>% 
  ggplot(aes(x = Aspect, y = Change_BGDensity)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  geom_point(data = gg.bgden.aspect,
             aes(x = Aspect, y = Change_BGDensity),
             color = "purple3",
             size = 2.5,
             shape = 15) +
  theme_bw() +
  labs(y = expression(paste(Delta ~ "Buffelgrass density (individuals / ", m^2, ")")),
       x = "Aspect") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
bgden.aspect


# Figure S9: Cover vs. aspect ---------------------------------------------

# Generate prediction 
gg.bgcov.aspect <- predict_response(bgcov, terms = "Aspect") %>% 
  rename(Aspect = x,
         Change_BGCover = predicted)

# Graph 
bgcov.aspect <- dat.plot %>% 
  ggplot(aes(x = Aspect, y = Change_BGCover)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  geom_point(data = gg.bgcov.aspect,
             aes(x = Aspect, y = Change_BGCover),
             color = "purple3",
             size = 2.5,
             shape = 15) +
  theme_bw() +
  labs(y = expression(Delta ~ "Buffelgrass cover (%)"),
       x = "Aspect") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
bgcov.aspect



# Figure S10: Cover, precip * shrub ---------------------------------------

# Generate prediction and add unscaled variable - 9 levels to get mean
insight.bgcov.shrub.precip <- dat.plot.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.bgcov.shrub.precip$Predicted <- get_predicted(bgcov, insight.bgcov.shrub.precip)
unscaled.shrub.precip <- dat.plot.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 9, numerics = "all") %>% 
  arrange(Change_ShrubCover) %>% 
  distinct(.keep_all = TRUE)
insight.bgcov.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip$Change_ShrubCover
insight.bgcov.shrub.precip$Prev_year_precip <- unscaled.shrub.precip$Prev_year_precip
unique(insight.bgcov.shrub.precip$Prev_year_precip_scaled) 
insight.bgcov.shrub.precip <- insight.bgcov.shrub.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.957, 0.005, 1.609))

# Graph
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



# Figure S11: Density vs. cover -------------------------------------------

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



# Figure S12: Total vs. cover ---------------------------------------------

total.cover <- culm.change %>% 
  ggplot(aes(x = Change_BGCover, y = Change_TotalCulms)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(y = expression(Delta ~ "Buffelgrass total culm count"),
       x = expression(Delta ~ "Buffelgrass cover (%)"))
total.cover



# Write out figures -------------------------------------------------------

# Figure S1
tiff("figures/publish2-figures/FigureS1_600dpi.tiff",
     units = "in", height = 4, width = 6, res = 600)
repro.shrub.precip
dev.off()

# Figure S2
tiff("figures/publish2-figures/FigureS2_600dpi.tiff",
     units = "in", height = 4, width = 6, res = 600)
repro.herb.precip
dev.off()

# Figure S3
tiff("figures/publish2-figures/FigureS3_600dpi.tiff",
     units = "in", height = 3.5, width = 9, res = 600)
ggarrange(shrub.change.precip, herb.change.precip,
          ncol = 2, nrow = 1,
          labels = c("(A)", "(B)"))
dev.off()

# Figure S4
tiff("figures/publish2-figures/FigureS4_600dpi.tiff",
     units = "in", height = 4, width = 6, res = 600)
total.bgden.precip
dev.off()

# Figure S5
tiff("figures/publish2-figures/FigureS5_600dpi.tiff",
     units = "in", height = 4, width = 6, res = 600)
repro.bgden.precip
dev.off()

# Figure S6
tiff("figures/publish2-figures/FigureS6_600dpi.tiff",
     units = "in", height = 4, width = 5, res = 600)
total.aspect
dev.off()

# Figure S7
tiff("figures/publish2-figures/FigureS7_600dpi.tiff",
     units = "in", height = 4, width = 5, res = 600)
repro.aspect
dev.off()

# Figure S8
tiff("figures/publish2-figures/FigureS8_600dpi.tiff",
     units = "in", height = 4, width = 5, res = 600)
bgden.aspect
dev.off()

# Figure S9
tiff("figures/publish2-figures/FigureS9_600dpi.tiff",
     units = "in", height = 4, width = 5, res = 600)
bgcov.aspect
dev.off()

# Figure S10
tiff("figures/publish2-figures/FigureS10_600dpi.tiff",
     units = "in", height = 4, width = 6, res = 600)
bgcov.shrub.precip
dev.off()

# Figure S11
tiff("figures/publish2-figures/FigureS11_600dpi.tiff",
     units = "in", height = 4, width = 5, res = 600)
density.cover
dev.off()

# Figure S12
tiff("figures/publish2-figures/FigureS12_600dpi.tiff",
     units = "in", height = 4, width = 5, res = 600)
total.cover
dev.off()



# Save --------------------------------------------------------------------

# Graphs only
save(repro.shrub.precip, repro.herb.precip, 
     shrub.change.precip, herb.change.precip,
     total.bgden.precip, repro.bgden.precip,
     total.aspect, repro.aspect, 
     bgden.aspect, bgcov.aspect,
     bgcov.shrub.precip, density.cover, total.cover,
     file = "RData/publish2_figsS1-S9.RData")


save.image("RData/publish2_supp-figures.RData")

