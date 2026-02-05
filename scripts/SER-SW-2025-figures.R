# Ceated: 2025-11-11
# Updated: 2025-11-11

# Purpose: Create figures for SER-SW 2025 conference presentation.

library(tidyverse)
library(insight)
library(viridis)
library(modelbased)

# Load data ---------------------------------------------------------------

load("RData/publish1_data-and-best-models.RData")


# Precip range ------------------------------------------------------------

summary(culm.change$Prev_year_precip) # 238 to 375 mm


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



# Total culm, precip * shrub ----------------------------------------------

# Generate prediction and add unscaled variable - 20 precip levels to get close to mean precip
insight.total.shrub.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 20, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled)
insight.total.shrub.precip$Predicted <- get_predicted(total_best.model, insight.total.shrub.precip)
unscaled.shrub.precip3200000 <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 20, numerics = "all") %>% 
  arrange(Change_ShrubCover)
insight.total.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip3200000$Change_ShrubCover
insight.total.shrub.precip$Prev_year_precip <- unscaled.shrub.precip3200000$Prev_year_precip
insight.total.shrub.precip <- insight.total.shrub.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.952, 0.003, 1.641))
insight.total.shrub.precip.avg <- insight.total.shrub.precip %>% 
  filter(Prev_year_precip_scaled == 0.003)


# Graph, gray data only
total.shrub1 <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_TotalCulms,
             color = Prev_year_precip)) +
  geom_point(color = "gray30", alpha = 0.5) +
  geom_line(data = insight.total.shrub.precip.avg,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5, alpha = 0) +
  theme_bw(base_size = 14) +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Buffelgrass culm count"),
       x = expression(Delta ~ "Native shrub cover (%)")) 
total.shrub1


# Graph, gray data with line
total.shrub <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_TotalCulms,
             color = Prev_year_precip)) +
  geom_point(color = "gray30", alpha = 0.5) +
  geom_line(data = insight.total.shrub.precip.avg,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5) +
  theme_bw(base_size = 14) +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Buffelgrass culm count"),
       x = expression(Delta ~ "Native shrub cover (%)")) 
total.shrub


# Graph, axes only
total.shrub.precip0 <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_TotalCulms,
             color = Prev_year_precip)) +
  geom_point(alpha = 0) +
  theme_bw(base_size = 14) +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Buffelgrass culm count"),
       x = expression(Delta ~ "Native shrub cover (%)")) 
total.shrub.precip0


# Graph, data only
total.shrub.precip1 <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_TotalCulms,
             color = Prev_year_precip)) +
  geom_point() +
  theme_bw(base_size = 14) +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Buffelgrass culm count"),
       x = expression(Delta ~ "Native shrub cover (%)")) 
total.shrub.precip1


# Graph, data and model prediction
total.shrub.precip <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_TotalCulms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.total.shrub.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5) +
  theme_bw(base_size = 14) +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Buffelgrass culm count"),
       x = expression(Delta ~ "Native shrub cover (%)")) 
total.shrub.precip



# Total culm, precip * herb -----------------------------------------------

# Generate prediction and add unscaled variable
insight.total.herb.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 20, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
insight.total.herb.precip$Predicted <- get_predicted(total_best.model, insight.total.herb.precip)
unscaled.herb.precip3200000 <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 20, numerics = "all") %>% 
  arrange(Change_HerbCover)
insight.total.herb.precip$Change_HerbCover <- unscaled.herb.precip3200000$Change_HerbCover
insight.total.herb.precip$Prev_year_precip <- unscaled.herb.precip3200000$Prev_year_precip
insight.total.herb.precip <- insight.total.herb.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.952, 0.003, 1.641))
insight.total.herb.precip.avg <- insight.total.herb.precip %>% 
  filter(Prev_year_precip_scaled == 0.003)


# Graph, gray data only
total.herb1 <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_TotalCulms,
             color = Prev_year_precip)) +
  geom_point(color = "gray30", alpha = 0.5) +
  geom_line(data = insight.total.herb.precip.avg,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5, alpha = 0) +
  theme_bw(base_size = 14) +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Buffelgrass culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)"))
total.herb1


# Graph, gray data with line
total.herb <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_TotalCulms,
             color = Prev_year_precip)) +
  geom_point(color = "gray30", alpha = 0.5) +
  geom_line(data = insight.total.herb.precip.avg,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5) +
  theme_bw(base_size = 14) +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Buffelgrass culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)"))
total.herb


# Graph, axes only
total.herb.precip0 <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_TotalCulms,
             color = Prev_year_precip)) +
  geom_point(alpha = 0) +
  theme_bw(base_size = 14) +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Buffelgrass culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)"))
total.herb.precip0


# Graph, data only
total.herb.precip1 <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_TotalCulms,
             color = Prev_year_precip)) +
  geom_point() +
  theme_bw(base_size = 14) +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Buffelgrass culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)"))
total.herb.precip1


# Graph, data and model prediction
total.herb.precip <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_TotalCulms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.total.herb.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5) +
  theme_bw(base_size = 14) +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Buffelgrass culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)"))
total.herb.precip



# Total culm, precip * density --------------------------------------------

# Generate prediction and add unscaled variable
insight.total.bgden.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_BGDensity_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 20, numerics = "all") %>% 
  arrange(Change_BGDensity_scaled)
insight.total.bgden.precip$Predicted <- get_predicted(total_best.model, insight.total.bgden.precip)
unscaled.bgden.precip3200000 <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_BGDensity", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 20, numerics = "all") %>% 
  arrange(Change_BGDensity)
insight.total.bgden.precip$Change_BGDensity <- unscaled.bgden.precip3200000$Change_BGDensity
insight.total.bgden.precip$Prev_year_precip <- unscaled.bgden.precip3200000$Prev_year_precip
insight.total.bgden.precip <- insight.total.bgden.precip %>% 
  filter(Prev_year_precip_scaled %in% c(-0.952, 0.003, 1.641))
insight.total.bgden.precip.avg <- insight.total.bgden.precip %>% 
  filter(Prev_year_precip_scaled == 0.003)


# Graph, gray data only
total.bgden1 <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_TotalCulms,
             color = Prev_year_precip)) +
  geom_point(color = "gray30", alpha = 0.5) +
  geom_line(data = insight.total.bgden.precip.avg,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5, alpha = 0) +
  theme_bw(base_size = 14) +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Buffelgrass culm count"),
       x = expression(Delta ~ paste("Buffelgrass plot density (individuals / ", m^2, ")")))
total.bgden1


# Graph, gray data with line
total.bgden <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_TotalCulms,
             color = Prev_year_precip)) +
  geom_point(color = "gray30", alpha = 0.5) +
  geom_line(data = insight.total.bgden.precip.avg,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5) +
  theme_bw(base_size = 14) +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Buffelgrass culm count"),
       x = expression(Delta ~ paste("Buffelgrass plot density (individuals / ", m^2, ")")))
total.bgden


# Graph, axes
total.bgden.precip0 <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_TotalCulms,
             color = Prev_year_precip)) +
  geom_point(alpha = 0) +
  theme_bw(base_size = 14) +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Buffelgrass culm count"),
       x = expression(Delta ~ paste("Buffelgrass plot density (individuals / ", m^2, ")")))
total.bgden.precip0


# Graph, data only
total.bgden.precip1 <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_TotalCulms,
             color = Prev_year_precip)) +
  geom_point() +
  theme_bw(base_size = 14) +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Buffelgrass culm count"),
       x = expression(Delta ~ paste("Buffelgrass plot density (individuals / ", m^2, ")")))
total.bgden.precip1


# Graph, data and model prediction
total.bgden.precip <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_TotalCulms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.total.bgden.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5) +
  theme_bw(base_size = 14) +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Buffelgrass culm count"),
       x = expression(Delta ~ paste("Buffelgrass plot density (individuals / ", m^2, ")")))
total.bgden.precip



# Shrub/herb cover vs. precip ---------------------------------------------

# Shrub cover change vs. precip
shrub.change.precip <- plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_ShrubCover)) +
  geom_point(color = "gray30", alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5) +
  theme_bw(base_size = 14) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(y = expression(Delta ~ "Native shrub cover (%)"),
       x = "Previous year precip (mm)") 
shrub.change.precip

# Herb cover change vs. precip
herb.change.precip <- plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_HerbCover)) +
  geom_point(color = "gray30", alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.5) +
  theme_bw(base_size = 13) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(y = expression(Delta ~ "Native grass & forb cover (%)"),
       x = "Previous year precip (mm)") 
herb.change.precip



# BG density vs. precip ---------------------------------------------------

# Generate prediction and add unscaled variable 
insight.bgden.precip <- get_datagrid(dat.plot.ex, by = c("Prev_year_precip_scaled"),
                                     length = 100)
insight.bgden.precip$Change_BGDensity <- get_predicted(bgden_best.model, insight.bgden.precip)
unscaled.precip100 <- get_datagrid(dat.plot.unscaled, by = "Prev_year_precip",
                                   length = 100) %>% 
  arrange(Prev_year_precip)
insight.bgden.precip$Prev_year_precip <- unscaled.precip100$Prev_year_precip

# Graph 
bgden.precip <- dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGDensity)) +
  geom_point(color = "gray30", alpha = 0.8) +
  geom_line(data = insight.bgden.precip,
            aes(y = Change_BGDensity), linewidth = 1.3,
            color = "purple3") +
  theme_bw(base_size = 13) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Previous year precip (mm)",
       y = expression(paste(Delta ~ "Density (individuals / ", m^2, ")"))) 
bgden.precip



# Total culm vs. aspect ---------------------------------------------------

# Generate prediction & CI
mb.total.aspect <- estimate_means(total_best.model, "Aspect") %>% 
  rename(Change_TotalCulms = Mean)

#   Graph (insight version)
total.aspect <- dat.culm %>% 
  ggplot(aes(x = Aspect, y = Change_TotalCulms)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(color = "gray30", alpha = 0.5) +
  geom_point(data = mb.total.aspect,
             aes(x = Aspect, y = Change_TotalCulms),
             color = "mediumorchid1",
             size = 4,
             shape = 17) +
  theme_bw(base_size = 14) +
  labs(y = expression(Delta ~ "Buffelgrass culm count"),
       x = "Aspect") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
total.aspect



# Total culm vs. slope ----------------------------------------------------

# Generate prediction and add unscaled variable
insight.total.slope <- get_datagrid(dat.culm.ex, by = c("PlotSlope_scaled"),
                                    length = 100)
insight.total.slope$Change_TotalCulms <- get_predicted(total_best.model, insight.total.slope)
insight.total.slope$SE <- get_predicted_ci(total_best.model, data = insight.total.slope)$SE
insight.total.slope$CI <- insight.total.slope$SE * 1.96
unscaled.slope100 <- get_datagrid(dat.culm.unscaled, by = "PlotSlope",
                                  length = 100) %>% 
  arrange(PlotSlope)
insight.total.slope$PlotSlope <- unscaled.slope100$PlotSlope

# Graph 
total.slope <- dat.culm %>% 
  ggplot(aes(x = PlotSlope, y = Change_TotalCulms)) +
  geom_point(color = "gray30", alpha = 0.5) +
  geom_line(data = insight.total.slope,
            aes(y = Change_TotalCulms), linewidth = 1.5,
            color = "mediumorchid1") +
  theme_bw(base_size = 14) +
  xlab("Plot slope (\u00B0)") +
  labs(y = expression(Delta ~ "Buffelgrass culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
total.slope



# Write out figures -------------------------------------------------------

# Total culm, precip * shrub
tiff("figures/SER-SW-2025-figures/Total-culm_shrub_data.tiff",
     units = "in", height = 6, width = 6, res = 600)
total.shrub1
dev.off()

tiff("figures/SER-SW-2025-figures/Total-culm_shrub.tiff",
     units = "in", height = 6, width = 6, res = 600)
total.shrub
dev.off()

tiff("figures/SER-SW-2025-figures/Total-culm_shrub-precip_axes.tiff",
     units = "in", height = 6, width = 6, res = 600)
total.shrub.precip0
dev.off()

tiff("figures/SER-SW-2025-figures/Total-culm_shrub-precip_data.tiff",
     units = "in", height = 6, width = 6, res = 600)
total.shrub.precip1
dev.off()

tiff("figures/SER-SW-2025-figures/Total-culm_shrub-precip.tiff",
     units = "in", height = 6, width = 6, res = 600)
total.shrub.precip
dev.off()


# Total culm, precip * herb
tiff("figures/SER-SW-2025-figures/Total-culm_herb_data.tiff",
     units = "in", height = 6, width = 6, res = 600)
total.herb1
dev.off()

tiff("figures/SER-SW-2025-figures/Total-culm_herb.tiff",
     units = "in", height = 6, width = 6, res = 600)
total.herb
dev.off()

tiff("figures/SER-SW-2025-figures/Total-culm_herb-precip_axes.tiff",
     units = "in", height = 6, width = 6, res = 600)
total.herb.precip0
dev.off()

tiff("figures/SER-SW-2025-figures/Total-culm_herb-precip_data.tiff",
     units = "in", height = 6, width = 6, res = 600)
total.herb.precip1
dev.off()

tiff("figures/SER-SW-2025-figures/Total-culm_herb-precip.tiff",
     units = "in", height = 6, width = 6, res = 600)
total.herb.precip
dev.off()


# Total culm, precip * density
tiff("figures/SER-SW-2025-figures/Total-culm_BG-density_data.tiff",
     units = "in", height = 6, width = 6, res = 600)
total.bgden1
dev.off()

tiff("figures/SER-SW-2025-figures/Total-culm_BG-density.tiff",
     units = "in", height = 6, width = 6, res = 600)
total.bgden
dev.off()

tiff("figures/SER-SW-2025-figures/Total-culm_BG-density-precip_axes.tiff",
     units = "in", height = 6, width = 6, res = 600)
total.bgden.precip0
dev.off()

tiff("figures/SER-SW-2025-figures/Total-culm_BG-density-precip_data.tiff",
     units = "in", height = 6, width = 6, res = 600)
total.bgden.precip1
dev.off()

tiff("figures/SER-SW-2025-figures/Total-culm_BG-density-precip.tiff",
     units = "in", height = 6, width = 6, res = 600)
total.bgden.precip
dev.off()


# Shrub vs. precip
tiff("figures/SER-SW-2025-figures/Shrub_precip.tiff",
     units = "in", height = 3, width = 4, res = 600)
shrub.change.precip
dev.off()

# Herb vs. precip
tiff("figures/SER-SW-2025-figures/Herb_precip.tiff",
     units = "in", height = 3, width = 4, res = 600)
herb.change.precip
dev.off()


# BG density vs. precip
tiff("figures/SER-SW-2025-figures/BG-density_precip.tiff",
     units = "in", height = 3, width = 4, res = 600)
bgden.precip
dev.off()


# Total culm vs. aspect
tiff("figures/SER-SW-2025-figures/Total-culm_aspect.tiff",
     units = "in", height = 4, width = 6, res = 600)
total.aspect
dev.off()


# Total culm vs. slope
tiff("figures/SER-SW-2025-figures/Total-culm_slope.tiff",
     units = "in", height = 4, width = 6, res = 600)
total.slope
dev.off()
