# Created: 2025-07-18
# Updated: 2025-07-22

# Purpose: Explore plots made with insight & ggeffects - graph predictions instead of simple
#   linear regressions.

# https://easystats.github.io/modelbased/articles/visualisation_matrix.html#visualising-an-interaction-between-two-numeric-variables-three-way-interaction

# Note: get_predicted() does not work with model averaged coefficients; needs actual model object
#   (it actually worked out well that the total culm change model had only 1 top model).

# To graph back-transformed/unscale continuous explanatory variables, need to make datagrid of unscaled variable
#   that is the same length as the datagrid with scaled variable and predictions, arranged by the variable
#   so the order will be the same. Then add that column to new dataframe.

# To add the 95% CI to the graph, make another datagrid of unscaled variable that is the same length as the
#   table with CI (produced from predict_response() from ggeffects). 
#     (I've tried to find the CI via modelbased instead of ggeffects, but it can't seem to handle the random effect.)

# For other models that have averaged coefficients, the model-predicted line using the best model is basically the
#   same as including a line from every top model (I tried this with repro.bgden and repro.shrub).
#   For simplicity, then, I will only use the best model in generating model-predicted lines.

library(tidyverse)
library(insight)
library(ggeffects)
library(viridis)
library(modelbased)

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
  


# Total culm change -------------------------------------------------------

# Total change: with CI, scaled 
# All fixed effects alone (automatic)
total.pred <- predict_response(total_best.model)
plot(total.pred)

# Precip * BG density change
total.pred.bgden.precip <- predict_response(total_best.model, 
                                            terms = c("Change_BGDensity_scaled", "Prev_year_precip_scaled"))
plot(total.pred.bgden.precip)

# Precip * shrub change
total.pred.shrub.precip <- predict_response(total_best.model, 
                                            terms = c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"))
plot(total.pred.shrub.precip)

# Precip * herb change
total.pred.herb.precip <- predict_response(total_best.model, 
                                           terms = c("Change_HerbCover_scaled", "Prev_year_precip_scaled"))
plot(total.pred.herb.precip)



## Total: Precip ----------------------------------------------------------

# Scaled datagrid with prediction
viz.total.precip <- get_datagrid(dat.culm.ex, by = c("Prev_year_precip_scaled"),
                                 length = 100)
viz.total.precip$Predicted <- get_predicted(total_best.model, viz.total.precip)
# Unscaled datagrid
unscaled.precip100 <- get_datagrid(dat.culm.unscaled, by = "Prev_year_precip",
                                   length = 100) %>% 
  arrange(Prev_year_precip)
# Data grid with prediction, unscaled variable added
viz.total.precip$Prev_year_precip <- unscaled.precip100$Prev_year_precip

# Graph (insight version)
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


# Generate CI 
ci.total.precip <- predict_response(total_best.model, terms = "Prev_year_precip_scaled")
# Unscaled datagrid
unscaled.precip15 <- get_datagrid(dat.culm.unscaled, by = "Prev_year_precip",
                                  length = 15) %>% 
  arrange(Prev_year_precip)
# Data grid with prediction & CI, unscaled variable added
ci.total.precip$Prev_year_precip <- unscaled.precip15$Prev_year_precip
ci.total.precip$Change_Total_Live_Culms <- ci.total.precip$predicted # must be named the same as y-axis in graph

# Graph with CI (ggeffects version)
total.precip.ci <- dat.culm %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_ribbon(data = ci.total.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.total.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  xlab("Previous year precip (mm)") +
  ggtitle("Change in total culm count vs. precip") +
  labs(y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
total.precip.ci


# Graph differences in prediction line (dashed is using insight, solid is from ggeffects)
dat.culm %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_ribbon(data = ci.total.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.total.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.total.precip,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  xlab("Previous year precip (mm)") +
  ggtitle("Change in total culm count vs. precip") +
  labs(y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")




## Total: Aspect ----------------------------------------------------------


# With a boxplot, I can't really graph the prediction on top of the original data, 
#   so they will just have to stay separate graphs.

# Original data (boxplot)
total.change.aspect <- culm.change %>% 
  ggplot(aes(x = Aspect, y = Change_Total_Live_Culms)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  theme_bw() +
  labs(title = "Change in total culm count by aspect",
       y = expression(Delta ~ "Total culm count"),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
total.change.aspect


# Generate CI
total.pred.aspect <- predict_response(total_best.model, terms = "Aspect")

# Graph with CI (ggeffects version)
total.pred.aspect <- total.pred.aspect %>% 
  ggplot(aes(x, predicted)) +
  geom_point() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), linetype = "dashed") +
  theme_bw() +
  labs(title = "Prediction for total change by aspect",
       y = "Change_Total_Live_culms",
       x = NULL) +
  scale_y_continuous(limits = c(-120, 220)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
total.pred.aspect




## Total: BG density ------------------------------------------------------

# Generate prediction and add unscaled variable
viz.total.bgden <- get_datagrid(dat.culm.ex, by = c("Change_BGDensity_scaled"),
                                 length = 100)
viz.total.bgden$Predicted <- get_predicted(total_best.model, viz.total.bgden)
unscaled.bgden100 <- get_datagrid(dat.culm.unscaled, by = "Change_BGDensity",
                                   length = 100) %>% 
  arrange(Change_BGDensity)
viz.total.bgden$Change_BGDensity <- unscaled.bgden100$Change_BGDensity

# Graph (insight version)
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


# Generate CI and add unscaled variable
ci.total.bgden <- predict_response(total_best.model, terms = "Change_BGDensity_scaled")
unscaled.bgden9 <- get_datagrid(dat.culm.unscaled, by = "Change_BGDensity",
                                  length = 9) %>% 
  arrange(Change_BGDensity)
ci.total.bgden$Change_BGDensity <- unscaled.bgden9$Change_BGDensity
ci.total.bgden$Change_Total_Live_Culms <- ci.total.bgden$predicted

# Graph with CI (ggeffects version)
total.bgden.ci <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_ribbon(data = ci.total.bgden,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.total.bgden,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  ggtitle("Change in total culm count vs. plot density change") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ paste("Density (individuals / ", m^2, ")"))) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
total.bgden.ci


# Graph differences in prediction line (dashed is using insight, solid is using ggeffects)
dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_ribbon(data = ci.total.bgden,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.total.bgden,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.total.bgden,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ paste("Density (individuals / ", m^2, ")"))) +
  ggtitle("Change in total culm count vs. plot density") +
  labs(y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")



## Total: Shrub cover -----------------------------------------------------

# Generate prediction with unscaled variable added
viz.total.shrub <- get_datagrid(dat.culm.ex, by = c("Change_ShrubCover_scaled"),
                                length = 100)
viz.total.shrub$Predicted <- get_predicted(total_best.model, viz.total.shrub)
unscaled.shrub100 <- get_datagrid(dat.culm.unscaled, by = "Change_ShrubCover",
                                  length = 100) %>% 
  arrange(Change_ShrubCover)
viz.total.shrub$Change_ShrubCover <- unscaled.shrub100$Change_ShrubCover

# Graph (insight version)
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


# Generate CI and add unscaled variable
ci.total.shrub <- predict_response(total_best.model, terms = "Change_ShrubCover_scaled")
unscaled.shrub16 <- get_datagrid(dat.culm.unscaled, by = "Change_ShrubCover",
                                length = 16) %>% 
  arrange(Change_ShrubCover)
ci.total.shrub$Change_ShrubCover <- unscaled.shrub16$Change_ShrubCover
ci.total.shrub$Change_Total_Live_Culms <- ci.total.shrub$predicted

# Graph with CI
total.shrub.ci <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_ribbon(data = ci.total.shrub,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.total.shrub,
            aes(y = predicted), linewidth = 1,
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
total.shrub.ci


# Graph differences in prediction line (dashed is using modelbased, solid is using ggeffects)
dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_ribbon(data = ci.total.shrub,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.total.shrub,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.total.shrub,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
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



## Total: Precip * shrub --------------------------------------------------

# Generate prediction and add unscaled variable
viz.total.shrub.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled)
viz.total.shrub.precip$Predicted <- get_predicted(total_best.model, viz.total.shrub.precip)
unscaled.shrub.precip243 <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover)
viz.total.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip243$Change_ShrubCover
viz.total.shrub.precip$Prev_year_precip <- unscaled.shrub.precip243$Prev_year_precip

# Graph (insight version)
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


# Generate CI and add unscaled variable
ci.total.shrub.precip <- predict_response(total_best.model, terms = c("Change_ShrubCover_scaled",
                                                                      "Prev_year_precip_scaled"))
#   predict_response() generates 48 rows and I have no idea how to get datagrid of unscaled equivalent


# Generate and graph CI with scaled x variable
total.pred.shrub.precip <- predict_response(total_best.model, 
                                            terms = c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"))
total.pred.shrub.precip$group <- factor(total.pred.shrub.precip$group,
                                        levels = c("1.01", "0.01", "-0.99"))
  
total.shrub.precip.ci <- total.pred.shrub.precip %>% 
  ggplot(aes(x, predicted, group = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3) +
  geom_line(aes(color = group),
            linewidth = 1.3) +
  theme_bw() +
  scale_color_manual(values = c("#440154FF", "#1F968BFF", "#FDE725FF")) +
  scale_fill_manual(values = c("#440154FF", "#1F968BFF", "#FDE725FF")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = "Change_Total_Live_Culms (scaled)",
       x = "Change_ShrubCover (scaled)",
       title = "Change in total culm count vs. shrub cover change (scaled)",
       color = "Prev_year_\nprecip_scaled",
       fill = "Prev_year_\nprecip_scaled") +
  scale_y_continuous(limits = c(-120, 220))
total.shrub.precip.ci




## Total: Precip * herb ---------------------------------------------------

# Precip * herb interaction
#   Scaled datagrid with prediction
viz.total.herb.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
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


# Generate and graph CI with scaled x variable
total.pred.herb.precip <- predict_response(total_best.model, 
                                            terms = c("Change_HerbCover_scaled", "Prev_year_precip_scaled"))
total.pred.herb.precip$group <- factor(total.pred.herb.precip$group,
                                        levels = c("1.01", "0.01", "-0.99"))

total.herb.precip.ci <- total.pred.herb.precip %>% 
  ggplot(aes(x, predicted, group = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3) +
  geom_line(aes(color = group),
            linewidth = 1.3) +
  theme_bw() +
  scale_color_manual(values = c("#440154FF", "#1F968BFF", "#FDE725FF")) +
  scale_fill_manual(values = c("#440154FF", "#1F968BFF", "#FDE725FF")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = "Change_Total_Live_Culms (scaled)",
       x = "Change_HerbCover (scaled)",
       title = "Change in total culm count vs. herb cover change (scaled)",
       color = "Prev_year_\nprecip_scaled",
       fill = "Prev_year_\nprecip_scaled") +
  scale_y_continuous(limits = c(-120, 220))
total.herb.precip.ci



## Total: plot slope (NS) -------------------------------------------------

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




## Total: herb cover (NS) -------------------------------------------------

# Scaled datagrid with prediction
viz.total.herb <- get_datagrid(dat.culm.ex, by = c("Change_HerbCover_scaled"),
                                length = 100)
viz.total.herb$Predicted <- get_predicted(total_best.model, viz.total.herb)
# Unscaled datagrid
unscaled.herb100 <- get_datagrid(dat.culm.unscaled, by = "Change_HerbCover",
                                  length = 100) %>% 
  arrange(Change_HerbCover)
# Data grid with prediction, unscaled variable added
viz.total.herb$Change_HerbCover <- unscaled.herb100$Change_HerbCover

# Graph
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



## Total: precip * density (NS) -------------------------------------------

# Scaled datagrid with prediction
viz.total.bgden.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_BGDensity_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_BGDensity_scaled)
viz.total.bgden.precip$Predicted <- get_predicted(total_best.model, viz.total.bgden.precip)
# Unscaled datagrid
unscaled.bgden.precip243 <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_BGDensity", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_BGDensity)
# Datagrid with prediction, unscaled variables added
viz.total.bgden.precip$Change_BGDensity <- unscaled.bgden.precip243$Change_BGDensity
viz.total.bgden.precip$Prev_year_precip <- unscaled.bgden.precip243$Prev_year_precip

# Graph
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


## Repro change: with CI, scaled ------------------------------------------

# All fixed effects alone
repro.pred <- predict_response(repro_best.model)
plot(repro.pred)

# Precip * BG density change
repro.pred.bgden.precip <- predict_response(repro_best.model, 
                                            terms = c("Change_BGDensity_scaled", "Prev_year_precip_scaled"))
plot(repro.pred.bgden.precip)

# Precip * shrub change
repro.pred.shrub.precip <- predict_response(repro_best.model, 
                                            terms = c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"))
plot(repro.pred.shrub.precip)

# Precip * herb change
repro.pred.herb.precip <- predict_response(repro_best.model, 
                                           terms = c("Change_HerbCover_scaled", "Prev_year_precip_scaled"))
plot(repro.pred.herb.precip)



# Buffelgrass density change ----------------------------------------------

## BG density change: Significant -----------------------------------------

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


## BG density change: with CI, scaled -------------------------------------

# All fixed effects alone
bgden.pred <- predict_response(bgden_best.model)
plot(bgden.pred)

# Precip * shrub change
bgden.pred.shrub.precip <- predict_response(bgden_best.model, 
                                            terms = c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"))
plot(bgden.pred.shrub.precip)




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


## BG cover change: with CI, scaled ---------------------------------------

# All fixed effects alone
bgcov.pred <- predict_response(bgcov_best.model)
plot(bgcov.pred)

# Precip * shrub change
bgcov.pred.shrub.precip <- predict_response(bgcov_best.model, 
                                            terms = c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"))
plot(bgcov.pred.shrub.precip)

# Precip * herb change
bgcov.pred.herb.precip <- predict_response(bgcov_best.model, 
                                           terms = c("Change_HerbCover_scaled", "Prev_year_precip_scaled"))
plot(bgcov.pred.herb.precip)



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


## Survival: with CI, scaled ----------------------------------------------

# All fixed effects alone
survival.pred <- predict_response(survival_best.model)
plot(survival.pred)

# Precip * BG density change
survival.pred.bgden.precip <- predict_response(survival_best.model, 
                                              terms = c("BGDensity_scaled", "Prev_year_precip_scaled"))
plot(survival.pred.bgden.precip)

# Precip * shrub change
survival.pred.shrub.precip <- predict_response(survival_best.model, 
                                            terms = c("ShrubCover_scaled", "Prev_year_precip_scaled"))
plot(survival.pred.shrub.precip)




# Write out draft figures -------------------------------------------------

## Total change -----------------------------------------------------------

# Significant
# Total change vs. Prev_year_precip
tiff("figures/2025-07_draft-figures-2.0/Total-change_prediction_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.precip
dev.off()

# Total change vs. Prev_year_precip (with CI)
tiff("figures/2025-07_draft-figures-2.0/Total-change_prediction_prev-year-precip_CI.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.precip.ci
dev.off()

# Total change by Aspect (original data only)
tiff("figures/2025-07_draft-figures-2.0/Total-change-by-aspect.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.change.aspect
dev.off()

# Total change by Aspect (prediction only)
tiff("figures/2025-07_draft-figures-2.0/Total-change-aspect-predictions_CI.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.precip.ci
dev.off()

# Total change vs. Change_BGDensity
tiff("figures/2025-07_draft-figures-2.0/Total-change_prediction_BG-density-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.bgden
dev.off()

# Total change vs. Change_BGDensity (with CI)
tiff("figures/2025-07_draft-figures-2.0/Total-change_prediction_BG-density-change_CI.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.bgden.ci
dev.off()

# Total change vs. Change_ShrubCover
tiff("figures/2025-07_draft-figures-2.0/Total-change_prediction_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.shrub
dev.off()

# Total change vs. Change_ShrubCover (with CI)
tiff("figures/2025-07_draft-figures-2.0/Total-change_prediction_shrub-cover-change_CI.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.shrub.ci
dev.off()

# Total change interaction of precip*shrub
tiff("figures/2025-07_draft-figures-2.0/Total-change_prediction_shrub-cover-change-and-precip-interaction.tiff",
     units = "in", height = 7, width = 6, res = 150)
total.shrub.precip
dev.off()

# Total change interaction of precip*shrub (with CI, scaled)
tiff("figures/2025-07_draft-figures-2.0/Total-change_prediction_shrub-cover-change-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 7, width = 6, res = 150)
total.shrub.precip.ci
dev.off()

# Total change interaction of precip*herb
tiff("figures/2025-07_draft-figures-2.0/Total-change_prediction_herb-cover-change-and-precip-interaction.tiff",
     units = "in", height = 7, width = 6, res = 150)
total.herb.precip
dev.off()

# Total change interaction of precip*herb (with CI, scaled)
tiff("figures/2025-07_draft-figures-2.0/Total-change_prediction_herb-cover-change-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 7, width = 6, res = 150)
total.herb.precip.ci
dev.off()


# Not significant
# Total change vs. PlotSlope
tiff("figures/2025-07_draft-figures-2.0/Total-change_prediction_plot-slope.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.slope
dev.off()

# Total change vs. Change_HerbCover
tiff("figures/2025-07_draft-figures-2.0/Total-change_prediction_herb-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.herb
dev.off()

# Total change interaction of precip*BG density
tiff("figures/2025-07_draft-figures-2.0/Total-change_prediction_BG-density-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.bgden.precip
dev.off()





## Repro change -----------------------------------------------------------

# Significant
# Repro change vs. Change_BGDensity
tiff("figures/2025-07_draft-figures-2.0/Repro-change_prediction_BG-density-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.bgden
dev.off()

# Repro change vs. Change_ShrubCover
tiff("figures/2025-07_draft-figures-2.0/Repro-change_prediction_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.shrub
dev.off()

# Repro change vs. Change_HerbCover
tiff("figures/2025-07_draft-figures-2.0/Repro-change_prediction_herb-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.herb
dev.off()


# Not significant
# Repro change vs. Prev_year_precip
tiff("figures/2025-07_draft-figures-2.0/Repro-change_prediction_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.precip
dev.off()



## BG density change ------------------------------------------------------

# Significant
# BG density change vs. Prev_year_precip
tiff("figures/2025-07_draft-figures-2.0/BG-density-change_prediction_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.precip
dev.off()

# BG density change vs. Change_ShrubCover
tiff("figures/2025-07_draft-figures-2.0/BG-density-change_prediction_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.shrub
dev.off()

# BG density change interaction of precip*shrub
tiff("figures/2025-07_draft-figures-2.0/BG-density-change_prediction_shrub-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.shrub.precip
dev.off()



## BG cover change --------------------------------------------------------

# Significant
# BG density change vs. Prev_year_precip
tiff("figures/2025-07_draft-figures-2.0/BG-cover-change_prediction_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.precip
dev.off()

# BG cover change vs. Change_ShrubCover
tiff("figures/2025-07_draft-figures-2.0/BG-cover-change_prediction_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.shrub
dev.off()



## Survival ---------------------------------------------------------------

# Significant
# Survival vs. Prev_year_precip
tiff("figures/2025-07_draft-figures-2.0/Survival_prediction_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.precip
dev.off()

# Survival vs. BGDensity
tiff("figures/2025-07_draft-figures-2.0/Survival_prediction_BG-density.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.bgden
dev.off()


save.image("RData/05.4_draft-figs-2.0-for-lm-5.1.RData")
