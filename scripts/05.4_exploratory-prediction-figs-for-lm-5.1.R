# Created: 2025-07-18
# Updated: 2025-07-18

# Purpose: Explore plots made with modelbased - graph predictions instead of simple
#   linear regressions.

# https://easystats.github.io/modelbased/articles/visualisation_matrix.html

library(tidyverse)
library(modelbased)
library(insight)
library(viridis)

# Load data ---------------------------------------------------------------

load("RData/06.6_linear-models-5.1.RData")


# Data wrangling ----------------------------------------------------------

# Total change dataset
dat.total <- culm.change.flat.rm %>% 
  select(Change_Total_Live_Culms, Prev_year_precip_scaled,
         Aspect, PlotSlope_scaled, Change_ShrubCover_scaled, Change_HerbCover_scaled,
         Change_BGDensity_scaled)




# Total change ------------------------------------------------------------

# Total change: Significant -----------------------------------------------

# Construct data grid
viz.total<- get_datagrid(dat.total, by = c("Prev_year_precip_scaled",
                                           "Aspect", "PlotSlope_scaled", 
                                           "Change_ShrubCover_scaled", "Change_HerbCover_scaled",
                                           "Change_BGDensity_scaled"),
                         preserve_range = TRUE)
viz.total$Predicted <- get_predicted(total_best.model, viz.total)

# Precip * herb interaction
viz.total.herb.precip <- dat.total %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all")
viz.total.herb.precip$Predicted <- get_predicted(total_best.model, viz.total.herb.precip)

total.herb.precip <- dat.total %>% 
  ggplot(aes(x = Change_HerbCover_scaled, y = Change_Total_Live_Culms,
             color = Prev_year_precip_scaled)) +
  geom_point() +
  geom_line(data = viz.total.herb.precip,
            aes(y = Predicted, group = Prev_year_precip_scaled), linewidth = 1.5) +
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


# Precip * shrub interaction
viz.total.shrub.precip <- dat.total %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all")
viz.total.shrub.precip$Predicted <- get_predicted(total_best.model, viz.total.shrub.precip)

total.shrub.precip <- dat.total %>% 
  ggplot(aes(x = Change_ShrubCover_scaled, y = Change_Total_Live_Culms,
             color = Prev_year_precip_scaled)) +
  geom_point() +
  geom_line(data = viz.total.shrub.precip,
            aes(y = Predicted, group = Prev_year_precip_scaled), linewidth = 1.5) +
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



# Write draft figures -----------------------------------------------------

## Total change -----------------------------------------------------------

# Total change interaction of precip*shrub
tiff("figures/2025-07_draft-figures/Total-change_prediction_shrub-change-and-precip-interaction.tiff",
     units = "in", height = 6, width = 6, res = 150)
total.shrub.precip
dev.off()

# Total change interaction of precip*herb
tiff("figures/2025-07_draft-figures/Total-change_prediction_herb-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.herb.precip
dev.off()
