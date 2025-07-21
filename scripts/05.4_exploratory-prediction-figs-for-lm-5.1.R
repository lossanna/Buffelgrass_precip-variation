# Created: 2025-07-18
# Updated: 2025-07-21

# Purpose: Explore plots made with modelbased - graph predictions instead of simple
#   linear regressions.

# https://easystats.github.io/modelbased/articles/visualisation_matrix.html

# Note: get_predicted() does not work with model averaged coefficients; needs actual model object
#   (it actually worked out well that the total culm change model had only 1 top model).

# To graph back-transformed/unscale continuous exlanatory variables, need to make datagrid of unscaled variable
#   that is the same length as the datagrid with scaled variable and predictions, arranged by the variable
#   so the order will be the same. Then add that column to new dataframe.

library(tidyverse)
library(modelbased)
library(insight)
library(viridis)

# Load data ---------------------------------------------------------------

load("RData/06.6_linear-models-5.1.RData")
dat.survival.raw <- dat.survival


# Data wrangling ----------------------------------------------------------

# Dataset for graphing (all)
#   Total and repro change dataset (all)
dat.culm <- culm.change.flat.rm %>% 
  select(Change_Total_Live_Culms, Change_Reproductive_culms, Prev_year_precip_scaled,
         Aspect, PlotSlope_scaled, Change_ShrubCover_scaled, Change_HerbCover_scaled,
         Change_BGDensity_scaled, Prev_year_precip, PlotSlope, Change_ShrubCover, Change_HerbCover,
         Change_BGDensity)

#   Plot change dataset (all)
dat.plot <- plot.change %>% 
  select(Change_BGDensity, Change_BGCover, Prev_year_precip_scaled,
         Aspect, PlotSlope_scaled, Change_ShrubCover_scaled, Change_HerbCover_scaled,
         Prev_year_precip, PlotSlope, Change_ShrubCover, Change_HerbCover)

#   Survival dataset (all)
dat.survival <- dat.survival.raw %>% 
  select(Prev_year_precip_scaled,
         Aspect, PlotSlope_scaled, ShrubCover_scaled, HerbCover_scaled,
         BGDensity_scaled, Prev_year_precip, PlotSlope, ShrubCover, HerbCover,
         BGDensity)


# Explanatory variables 
  # Culm change explanatory variables (for datagrid and prediction)
dat.culm.ex <- culm.change.flat.rm %>% 
  select(Prev_year_precip_scaled,
         Aspect, PlotSlope_scaled, Change_ShrubCover_scaled, Change_HerbCover_scaled,
         Change_BGDensity_scaled)

#   Plot change explanatory variables (for datagrid and prediction) - density & cover
dat.plot.ex <- plot.change %>% 
  select(Prev_year_precip_scaled,
         Aspect, PlotSlope_scaled, Change_ShrubCover_scaled, Change_HerbCover_scaled)

#   Survival explanatory variables (for datagrid and prediction)
dat.survival.ex <- dat.survival.raw %>% 
  select(Prev_year_precip_scaled,
         Aspect, PlotSlope_scaled, ShrubCover_scaled, HerbCover_scaled,
         BGDensity_scaled)


# Unscaled variables
#   Culm change unscaled variables (for datagrid) 
dat.culm.unscaled <- culm.change.flat.rm %>% 
  select(Prev_year_precip, Aspect, PlotSlope, Change_ShrubCover, Change_HerbCover,
         Change_BGDensity)

#   Plot change unscaled variables (for datagrid)
dat.plot.unscaled <- plot.change %>% 
  select(Prev_year_precip, Aspect, PlotSlope, Change_ShrubCover, Change_HerbCover)
  

#   Survival unscaled variables (for datagrid)
dat.survival.unscaled <- dat.survival.raw %>% 
  select(Prev_year_precip, Aspect, PlotSlope, ShrubCover, HerbCover,
         BGDensity)
  

# Total change ------------------------------------------------------------

## Total change: Significant ----------------------------------------------

# Precip
#   Scaled datagrid with prediction
viz.total.precip <- get_datagrid(dat.culm.ex, by = c("Prev_year_precip_scaled"),
                                 length = 100)
viz.total.precip$Predicted <- get_predicted(total_best.model, viz.total.precip)
#   Unscaled datagrid
unscaled.precip100 <- get_datagrid(dat.culm.unscaled, by = "Prev_year_precip",
                                   length = 100) %>% 
  arrange(Prev_year_precip)
#   Data grid with prediction, unscaled variable added
viz.total.precip$Prev_year_precip <- unscaled.precip100$Prev_year_precip

#   Graph
total.precip <- dat.culm %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_line(data = viz.total.precip,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  xlab("Previous year precip (mm)") +
  ggtitle("Change in total culm count vs. precip") +
  labs(y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
total.precip


# Aspect
#   not really sure how to do a categorical variable


# Buffelgrass density
#   Scaled datagrid with prediction
viz.total.bgden <- get_datagrid(dat.culm.ex, by = c("Change_BGDensity_scaled"),
                                 length = 100)
viz.total.bgden$Predicted <- get_predicted(total_best.model, viz.total.bgden)
#   Unscaled datagrid
unscaled.bgden100 <- get_datagrid(dat.culm.unscaled, by = "Change_BGDensity",
                                   length = 100) %>% 
  arrange(Change_BGDensity)
#   Data grid with prediction, unscaled variable added
viz.total.bgden$Change_BGDensity <- unscaled.bgden100$Change_BGDensity

#   Graph
total.bgden <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_line(data = viz.total.bgden,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  ggtitle("Change in total culm count vs. plot density change") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ paste("Density (individuals / ", m^2, ")"))) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
total.bgden


# Shrub cover change
#   Scaled datagrid with prediction
viz.total.shrub <- get_datagrid(dat.culm.ex, by = c("Change_ShrubCover_scaled"),
                                length = 100)
viz.total.shrub$Predicted <- get_predicted(total_best.model, viz.total.shrub)
#   Unscaled datagrid
unscaled.shrub100 <- get_datagrid(dat.culm.unscaled, by = "Change_ShrubCover",
                                  length = 100) %>% 
  arrange(Change_ShrubCover)
#   Data grid with prediction, unscaled variable added
viz.total.shrub$Change_ShrubCover <- unscaled.shrub100$Change_ShrubCover

#   Graph
total.shrub <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_line(data = viz.total.shrub,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  ggtitle("Change in total culm count vs. shrub cover change") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native shrub cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
total.shrub


# Precip * shrub interaction
#   Scaled datagrid with prediction
viz.total.shrub.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled)
viz.total.shrub.precip$Predicted <- get_predicted(total_best.model, viz.total.shrub.precip)
#   Unscaled datagrid
unscaled.shrub.precip243 <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover)
#   Datagrid with prediction, unscaled variables added
viz.total.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip243$Change_ShrubCover
viz.total.shrub.precip$Prev_year_precip <- unscaled.shrub.precip243$Prev_year_precip

#   Graph
total.shrub.precip <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Total_Live_Culms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = viz.total.shrub.precip,
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
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in total culm count vs. shrub cover change")
total.shrub.precip


# Precip * herb interaction
#   Scaled datagrid with prediction
viz.total.herb.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
viz.total.herb.precip$Predicted <- get_predicted(total_best.model, viz.total.herb.precip)
#   Unscaled datagrid
unscaled.herb.precip243 <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover)
#   Datagrid with prediction, unscaled variables added
viz.total.herb.precip$Change_HerbCover <- unscaled.herb.precip243$Change_HerbCover
viz.total.herb.precip$Prev_year_precip <- unscaled.herb.precip243$Prev_year_precip

#   Graph
total.herb.precip <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Total_Live_Culms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = viz.total.herb.precip,
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
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in total culm count vs. herb cover change")
total.herb.precip




## Total change: Not significant ------------------------------------------

# Plot slope
#   Scaled datagrid with prediction
viz.total.slope <- get_datagrid(dat.culm.ex, by = c("PlotSlope_scaled"),
                                length = 100)
viz.total.slope$Predicted <- get_predicted(total_best.model, viz.total.slope)
#   Unscaled datagrid
unscaled.slope100 <- get_datagrid(dat.culm.unscaled, by = "PlotSlope",
                                  length = 100) %>% 
  arrange(PlotSlope)
#   Data grid with prediction, unscaled variable added
viz.total.slope$PlotSlope <- unscaled.slope100$PlotSlope

#   Graph
total.slope <- dat.culm %>% 
  ggplot(aes(x = PlotSlope, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_line(data = viz.total.slope,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in total culm count vs. slope") +
  labs(y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
total.slope


# Herb cover change
#   Scaled datagrid with prediction
viz.total.herb <- get_datagrid(dat.culm.ex, by = c("Change_HerbCover_scaled"),
                                length = 100)
viz.total.herb$Predicted <- get_predicted(total_best.model, viz.total.herb)
#   Unscaled datagrid
unscaled.herb100 <- get_datagrid(dat.culm.unscaled, by = "Change_HerbCover",
                                  length = 100) %>% 
  arrange(Change_HerbCover)
#   Data grid with prediction, unscaled variable added
viz.total.herb$Change_HerbCover <- unscaled.herb100$Change_HerbCover

#   Graph
total.herb <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_line(data = viz.total.herb,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  ggtitle("Change in total culm count vs. herb cover change") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
total.herb


# Precip * bgden interaction
#   Scaled datagrid with prediction
viz.total.bgden.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_BGDensity_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_BGDensity_scaled)
viz.total.bgden.precip$Predicted <- get_predicted(total_best.model, viz.total.bgden.precip)
#   Unscaled datagrid
unscaled.bgden.precip243 <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_BGDensity", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_BGDensity)
#   Datagrid with prediction, unscaled variables added
viz.total.bgden.precip$Change_BGDensity <- unscaled.bgden.precip243$Change_BGDensity
viz.total.bgden.precip$Prev_year_precip <- unscaled.bgden.precip243$Prev_year_precip

#   Graph
total.bgden.precip <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Total_Live_Culms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = viz.total.bgden.precip,
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
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ paste("Density (individuals / ", m^2, ")")),
       title = "Change in total culm count vs. plot density change")
total.bgden.precip



# Reproductive culm change ------------------------------------------------

## Repro culm change: Significant -----------------------------------------

# Buffelgrass density
#   Scaled datagrid with prediction
viz.repro.bgden <- get_datagrid(dat.culm.ex, by = c("Change_BGDensity_scaled"),
                                length = 100)
viz.repro.bgden$Predicted <- get_predicted(repro_best.model, viz.repro.bgden)
#   Unscaled datagrid
unscaled.bgden100 <- get_datagrid(dat.culm.unscaled, by = "Change_BGDensity",
                                  length = 100) %>% 
  arrange(Change_BGDensity)
#   Data grid with prediction, unscaled variable added
viz.repro.bgden$Change_BGDensity <- unscaled.bgden100$Change_BGDensity

#   Graph
repro.bgden <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_line(data = viz.repro.bgden,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  ggtitle("Change in repro culm count vs. plot density change") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ paste("Density (individuals / ", m^2, ")"))) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
repro.bgden

# Shrub cover change
#   Scaled datagrid with prediction
viz.repro.shrub <- get_datagrid(dat.culm.ex, by = c("Change_ShrubCover_scaled"),
                                length = 100)
viz.repro.shrub$Predicted <- get_predicted(repro_best.model, viz.repro.shrub)
#   Unscaled datagrid
unscaled.shrub100 <- get_datagrid(dat.culm.unscaled, by = "Change_ShrubCover",
                                  length = 100) %>% 
  arrange(Change_ShrubCover)
#   Data grid with prediction, unscaled variable added
viz.repro.shrub$Change_ShrubCover <- unscaled.shrub100$Change_ShrubCover

#   Graph
repro.shrub <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_line(data = viz.repro.shrub,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  ggtitle("Change in repro culm count vs. shrub cover change") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ "Native shrub cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
repro.shrub


# Herb cover change
#   Scaled datagrid with prediction
viz.repro.herb <- get_datagrid(dat.culm.ex, by = c("Change_HerbCover_scaled"),
                               length = 100)
viz.repro.herb$Predicted <- get_predicted(repro_best.model, viz.repro.herb)
#   Unscaled datagrid
unscaled.herb100 <- get_datagrid(dat.culm.unscaled, by = "Change_HerbCover",
                                 length = 100) %>% 
  arrange(Change_HerbCover)
#   Data grid with prediction, unscaled variable added
viz.repro.herb$Change_HerbCover <- unscaled.herb100$Change_HerbCover

#   Graph
repro.herb <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_line(data = viz.repro.herb,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  ggtitle("Change in repro culm count vs. herb cover change") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
repro.herb



# Write out draft figures -------------------------------------------------

## Total change -----------------------------------------------------------

# Significant
# Total change vs. Prev_year_precip
tiff("figures/2025-07_draft-figures/Total-change_prediction_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.precip
dev.off()

# Total change vs. Change_BGDensity
tiff("figures/2025-07_draft-figures/Total-change_prediction_BG-density-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.bgden
dev.off()

# Total change vs. Change_ShrubCover
tiff("figures/2025-07_draft-figures/Total-change_prediction_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.shrub
dev.off()

# Total change interaction of precip*shrub
tiff("figures/2025-07_draft-figures/Total-change_prediction_shrub-change-and-precip-interaction.tiff",
     units = "in", height = 7, width = 6, res = 150)
total.shrub.precip
dev.off()

# Total change interaction of precip*herb
tiff("figures/2025-07_draft-figures/Total-change_prediction_herb-change-and-precip-interaction.tiff",
     units = "in", height = 7, width = 6, res = 150)
total.herb.precip
dev.off()


# Not significant
# Total change vs. PlotSlope
tiff("figures/2025-07_draft-figures/Total-change_prediction_plot-slope.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.slope
dev.off()

# Total change vs. Change_HerbCover
tiff("figures/2025-07_draft-figures/Total-change_prediction_herb-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.herb
dev.off()

# Total change interaction of precip*herb
tiff("figures/2025-07_draft-figures/Total-change_prediction_BG-density-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.bgden.precip
dev.off()


## Repro change -----------------------------------------------------------

# Significant
# Repro change vs. Change_BGDensity
tiff("figures/2025-07_draft-figures/Repro-change_prediction_BG-density-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.bgden
dev.off()

# Repro change vs. Change_ShrubCover
tiff("figures/2025-07_draft-figures/Repro-change_prediction_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.shrub
dev.off()

# Repro change vs. Change_HerbCover
tiff("figures/2025-07_draft-figures/Repro-change_prediction_herb-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.herb
dev.off()

save.image("RData/05.4_exploratory-prediction-figs-for-lm-5.1.RData")
