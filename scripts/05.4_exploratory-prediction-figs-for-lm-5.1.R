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

# For other models that have averaged coefficients, the model-predicted line using the best model is basically the
#   same as including a line from every top model (I tried this with repro.bgden and repro.shrub).
#   For simplicity, then, I will only use the best model in generating model-predicted lines.

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
  select(survival_perc, Prev_year_precip_scaled,
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
viz.repro.bgden$Predicted1 <- get_predicted(repro_best.model, viz.repro.bgden)
viz.repro.bgden$Predicted2 <- get_predicted(repro_model2, viz.repro.bgden)
viz.repro.bgden$Predicted3 <- get_predicted(repro_model3, viz.repro.bgden)
viz.repro.bgden$Predicted4 <- get_predicted(repro_model4, viz.repro.bgden)
viz.repro.bgden$Predicted5 <- get_predicted(repro_model5, viz.repro.bgden)
viz.repro.bgden$Predicted6 <- get_predicted(repro_model6, viz.repro.bgden)
viz.repro.bgden$Predicted7 <- get_predicted(repro_model7, viz.repro.bgden)
viz.repro.bgden$Predicted8 <- get_predicted(repro_model8, viz.repro.bgden)
#   Unscaled datagrid
unscaled.bgden100 <- get_datagrid(dat.culm.unscaled, by = "Change_BGDensity",
                                  length = 100) %>% 
  arrange(Change_BGDensity)
#   Data grid with prediction, unscaled variable added
viz.repro.bgden$Change_BGDensity <- unscaled.bgden100$Change_BGDensity

#   Graph (best model)
repro.bgden <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_line(data = viz.repro.bgden,
            aes(y = Predicted1), linewidth = 1.5,
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

#   Graph (all 8 top models) - basically the same
repro.bgden.alltop8 <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_line(data = viz.repro.bgden,
            aes(y = Predicted1), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.repro.bgden,
            aes(y = Predicted2), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.repro.bgden,
            aes(y = Predicted3), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.repro.bgden,
            aes(y = Predicted4), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.repro.bgden,
            aes(y = Predicted5), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.repro.bgden,
            aes(y = Predicted6), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.repro.bgden,
            aes(y = Predicted7), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.repro.bgden,
            aes(y = Predicted8), linewidth = 1,
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
repro.bgden.alltop8


# Shrub cover change
#   Scaled datagrid with prediction
viz.repro.shrub <- get_datagrid(dat.culm.ex, by = c("Change_ShrubCover_scaled"),
                                length = 100)
viz.repro.shrub$Predicted1 <- get_predicted(repro_best.model, viz.repro.shrub)
viz.repro.shrub$Predicted2 <- get_predicted(repro_model2, viz.repro.shrub)
viz.repro.shrub$Predicted3 <- get_predicted(repro_model3, viz.repro.shrub)
viz.repro.shrub$Predicted4 <- get_predicted(repro_model4, viz.repro.shrub)
viz.repro.shrub$Predicted5 <- get_predicted(repro_model5, viz.repro.shrub)
viz.repro.shrub$Predicted6 <- get_predicted(repro_model6, viz.repro.shrub)
viz.repro.shrub$Predicted7 <- get_predicted(repro_model7, viz.repro.shrub)
viz.repro.shrub$Predicted8 <- get_predicted(repro_model8, viz.repro.shrub)
#   Unscaled datagrid
unscaled.shrub100 <- get_datagrid(dat.culm.unscaled, by = "Change_ShrubCover",
                                  length = 100) %>% 
  arrange(Change_ShrubCover)
#   Data grid with prediction, unscaled variable added
viz.repro.shrub$Change_ShrubCover <- unscaled.shrub100$Change_ShrubCover

#   Graph (best model)
repro.shrub <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_line(data = viz.repro.shrub,
            aes(y = Predicted1), linewidth = 1.5,
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

#   Graph (all top 8 models) - basically the same
repro.shrub.alltop8 <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_line(data = viz.repro.shrub,
            aes(y = Predicted1), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.repro.shrub,
            aes(y = Predicted2), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.repro.shrub,
            aes(y = Predicted3), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.repro.shrub,
            aes(y = Predicted4), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.repro.shrub,
            aes(y = Predicted5), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.repro.shrub,
            aes(y = Predicted6), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.repro.shrub,
            aes(y = Predicted7), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.repro.shrub,
            aes(y = Predicted8), linewidth = 1,
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
repro.shrub.alltop8


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


## Repro change: Not significant ------------------------------------------

# Precip
#   Scaled datagrid with prediction
viz.repro.precip <- get_datagrid(dat.culm.ex, by = c("Prev_year_precip_scaled"),
                                 length = 100)
viz.repro.precip$Predicted <- get_predicted(repro_best.model, viz.repro.precip)
#   Unscaled datagrid
unscaled.precip100 <- get_datagrid(dat.culm.unscaled, by = "Prev_year_precip",
                                   length = 100) %>% 
  arrange(Prev_year_precip)
#   Data grid with prediction, unscaled variable added
viz.repro.precip$Prev_year_precip <- unscaled.precip100$Prev_year_precip

#   Graph
repro.precip <- dat.culm %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_line(data = viz.repro.precip,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  xlab("Previous year precip (mm)") +
  ggtitle("Change in repro culm count vs. precip") +
  labs(y = expression(Delta ~ "Reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
repro.precip



# Buffelgrass density change ----------------------------------------------

# BG density change: Significant ------------------------------------------

# Precip
#   Scaled datagrid with prediction
viz.bgden.precip <- get_datagrid(dat.plot.ex, by = c("Prev_year_precip_scaled"),
                                 length = 100)
viz.bgden.precip$Predicted <- get_predicted(bgden_best.model, viz.bgden.precip)
#   Unscaled datagrid
unscaled.precip100 <- get_datagrid(dat.plot.unscaled, by = "Prev_year_precip",
                                   length = 100) %>% 
  arrange(Prev_year_precip)
#   Data grid with prediction, unscaled variable added
viz.bgden.precip$Prev_year_precip <- unscaled.precip100$Prev_year_precip

#   Graph
bgden.precip <- dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGDensity)) +
  geom_point() +
  geom_line(data = viz.bgden.precip,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Previous year precip (mm)",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       title = "Change in buffelgrass density vs. precip")
bgden.precip


# Shrub cover change
#   Scaled datagrid with prediction
viz.bgden.shrub <- get_datagrid(dat.plot.ex, by = c("Change_ShrubCover_scaled"),
                                length = 100)
viz.bgden.shrub$Predicted <- get_predicted(bgden_best.model, viz.bgden.shrub)
#   Unscaled datagrid
unscaled.shrub100 <- get_datagrid(dat.plot.unscaled, by = "Change_ShrubCover",
                                  length = 100) %>% 
  arrange(Change_ShrubCover)
#   Data grid with prediction, unscaled variable added
viz.bgden.shrub$Change_ShrubCover <- unscaled.shrub100$Change_ShrubCover

#   Graph
bgden.shrub <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGDensity)) +
  geom_point() +
  geom_line(data = viz.bgden.shrub,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffelgrass density vs. shrub cover change") +
geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
bgden.shrub


# Precip * shrub interaction
#   Scaled datagrid with prediction
viz.bgden.shrub.precip <- dat.plot.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled)
viz.bgden.shrub.precip$Predicted <- get_predicted(bgden_best.model, viz.bgden.shrub.precip)
#   Unscaled datagrid
unscaled.shrub.precip243 <- dat.plot.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover)
#   Datagrid with prediction, unscaled variables added
viz.bgden.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip243$Change_ShrubCover
viz.bgden.shrub.precip$Prev_year_precip <- unscaled.shrub.precip243$Prev_year_precip

#   Graph
bgden.shrub.precip <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGDensity,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = viz.bgden.shrub.precip,
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
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffelgrass density vs. shrub cover change")
bgden.shrub.precip



# Buffelgrass cover change ------------------------------------------------

## BG cover change: Significant -------------------------------------------

# Precip
#   Scaled datagrid with prediction
viz.bgcov.precip <- get_datagrid(dat.plot.ex, by = c("Prev_year_precip_scaled"),
                                 length = 100)
viz.bgcov.precip$Predicted <- get_predicted(bgcov_best.model, viz.bgcov.precip)
#   Unscaled datagrid
unscaled.precip100 <- get_datagrid(dat.plot.unscaled, by = "Prev_year_precip",
                                   length = 100) %>% 
  arrange(Prev_year_precip)
#   Data grid with prediction, unscaled variable added
viz.bgcov.precip$Prev_year_precip <- unscaled.precip100$Prev_year_precip

#   Graph
bgcov.precip <- dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGCover)) +
  geom_point() +
  geom_line(data = viz.bgcov.precip,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Previous year precip (mm)",
       y = expression(Delta ~ "Cover (%)"),
       title = "Change in buffelgrass cover vs. precip")
bgcov.precip


# Shrub cover change
#   Scaled datagrid with prediction
viz.bgcov.shrub <- get_datagrid(dat.plot.ex, by = c("Change_ShrubCover_scaled"),
                                length = 100)
viz.bgcov.shrub$Predicted <- get_predicted(bgcov_best.model, viz.bgcov.shrub)
#   Unscaled datagrid
unscaled.shrub100 <- get_datagrid(dat.plot.unscaled, by = "Change_ShrubCover",
                                  length = 100) %>% 
  arrange(Change_ShrubCover)
#   Data grid with prediction, unscaled variable added
viz.bgcov.shrub$Change_ShrubCover <- unscaled.shrub100$Change_ShrubCover

#   Graph
bgcov.shrub <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGCover)) +
  geom_point() +
  geom_line(data = viz.bgcov.shrub,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  labs(y = expression(Delta ~ "Cover (%)"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffelgrass cover vs. shrub cover change") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
bgcov.shrub




# Survival ----------------------------------------------------------------

## Survival: Significant --------------------------------------------------

# Precip
#   Scaled datagrid with prediction
viz.survival.precip <- get_datagrid(dat.survival.ex, by = c("Prev_year_precip_scaled"),
                                    length = 50)
viz.survival.precip$Predicted <- get_predicted(survival_best.model, viz.survival.precip) # Predicting new random effect levels for terms: 1 | Transect:Site
#   Unscaled datagrid
unscaled.precip50 <- get_datagrid(dat.survival.unscaled, by = "Prev_year_precip",
                                   length = 50) %>% 
  arrange(Prev_year_precip)
#   Data grid with prediction, unscaled variable added
viz.survival.precip$Prev_year_precip <- unscaled.precip50$Prev_year_precip

#   Graph
survival.precip <- dat.survival %>% 
  ggplot(aes(x = Prev_year_precip, y = survival_perc)) +
  geom_point() +
  geom_line(data = viz.survival.precip,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  labs(x = "Previous year precip (mm)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. precip") 
survival.precip # ommited points from model prediction line, which goes beyond 100%


# BG density
#   Scaled datagrid with prediction
viz.survival.bgden <- get_datagrid(dat.survival.ex, by = c("BGDensity_scaled"),
                                    length = 50)
viz.survival.bgden$Predicted <- get_predicted(survival_best.model, viz.survival.bgden) # Predicting new random effect levels for terms: 1 | Transect:Site
#   Unscaled datagrid
unscaled.bgden50 <- get_datagrid(dat.survival.unscaled, by = "BGDensity",
                                  length = 50) %>% 
  arrange(BGDensity)
#   Data grid with prediction, unscaled variable added
viz.survival.bgden$BGDensity <- unscaled.bgden50$BGDensity

#   Graph
survival.bgden <- dat.survival %>% 
  ggplot(aes(x = BGDensity, y = survival_perc)) +
  geom_point() +
  geom_line(data = viz.survival.bgden,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  labs(x = expression(paste("Density (individuals / ", m^2, ")")),
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. plot density") 
survival.bgden



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
tiff("figures/2025-07_draft-figures/Total-change_prediction_shrub-cover-change-and-precip-interaction.tiff",
     units = "in", height = 7, width = 6, res = 150)
total.shrub.precip
dev.off()

# Total change interaction of precip*herb
tiff("figures/2025-07_draft-figures/Total-change_prediction_herb-cover-change-and-precip-interaction.tiff",
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

# Total change interaction of precip*BG density
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


# Not significant
# Repro change vs. Prev_year_precip
tiff("figures/2025-07_draft-figures/Repro-change_prediction_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.precip
dev.off()



## BG density change ------------------------------------------------------

# Significant
# BG density change vs. Prev_year_precip
tiff("figures/2025-07_draft-figures/BG-density-change_prediction_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.precip
dev.off()

# BG density change vs. Change_ShrubCover
tiff("figures/2025-07_draft-figures/BG-density-change_prediction_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.shrub
dev.off()

# BG density change interaction of precip*shrub
tiff("figures/2025-07_draft-figures/BG-density-change_prediction_shrub-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.shrub.precip
dev.off()



## BG cover change --------------------------------------------------------

# Significant
# BG density change vs. Prev_year_precip
tiff("figures/2025-07_draft-figures/BG-cover-change_prediction_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.precip
dev.off()

# BG cover change vs. Change_ShrubCover
tiff("figures/2025-07_draft-figures/BG-cover-change_prediction_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.shrub
dev.off()



## Survival ---------------------------------------------------------------

# Significant
# Survival vs. Prev_year_precip
tiff("figures/2025-07_draft-figures/Survival_prediction_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.precip
dev.off()

# Survival vs. BGDensity
tiff("figures/2025-07_draft-figures/Survival_prediction_BG-density.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.bgden
dev.off()


save.image("RData/05.4_exploratory-prediction-figs-for-lm-5.1.RData")
