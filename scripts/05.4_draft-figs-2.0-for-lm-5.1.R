# Created: 2025-07-18
# Updated: 2025-07-28

# Purpose: Explore plots made with insight & ggeffects - graph predictions instead of simple
#   linear regressions.

# https://easystats.github.io/modelbased/articles/visualisation_matrix.html#visualising-an-interaction-between-two-numeric-variables-three-way-interaction

# Note: get_predicted() does not work with model averaged coefficients; needs actual model object
#   (it actually worked out well that the total culm change model had only 1 top model).

# For other models that have averaged coefficients, the model-predicted line using the best model is basically the
#   same as including a line from every top model (I tried this with repro.bgden and repro.shrub).
#   For simplicity, then, I will only use the best model in generating model-predicted lines.

# To graph back-transformed/unscale continuous explanatory variables, need to make datagrid of unscaled variable
#   that is the same length as the datagrid with scaled variable and predictions, arranged by the variable
#   so the order will be the same. Then add that column to new dataframe.

# To add the 95% CI to the graph, make another datagrid of unscaled variable that is the same length as the
#   table with CI (produced from predict_response() from ggeffects). 

# The ggeffects and insight versions differ slightly. I've tried to find the CI via modelbased instead of ggeffects, 
#   but it can't seem to handle the random effect. 
# For the culm models, the difference is not that great, and I think it's more important to have the SE.
# But for the density & cover models, the ggeffects version seems to have a consistently higher intercept that doesn't
#   fit the original data as well, so I will go with the insight version. (I don't know why this difference is happening,
#   and tried to test another top model, but it was basically the same, so that's not it. Perhaps it is how both of the
#   packages are handling the random effects? Maybe that is why they are different?)
# For survival and single variable, ggeffects and insight versions look similar. For interactions, the insight version
#   has much higher y-intercept than ggeffects version, especially for wettest (but there aren't a lot of those original
#   data points anyway).


library(tidyverse)
library(insight)
library(ggeffects)
library(viridis)
library(modelbased)
library(ggpubr)

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

#   Plot change explanatory variables (for datagrid and prediction) - density
dat.bgden.ex <- plot.change %>% 
  select(Prev_year_precip_scaled,
         Aspect, PlotSlope_scaled, Change_ShrubCover_scaled)

#   Plot change explanatory variables (for datagrid and prediction) - cover
dat.bgcov.ex <- plot.change %>% 
  select(Prev_year_precip_scaled,
         Aspect, PlotSlope_scaled, Change_ShrubCover_scaled, Change_HerbCover_scaled)

#   Survival explanatory variables (for datagrid and prediction)
dat.survival.ex <- dat.survival.raw %>% 
  select(Prev_year_precip_scaled,
         PlotSlope_scaled, ShrubCover_scaled, HerbCover_scaled,
         BGDensity_scaled)


# Unscaled variables
#   Culm change unscaled variables (for datagrid) 
dat.culm.unscaled <- culm.change.flat.rm %>% 
  select(Prev_year_precip, Aspect, PlotSlope, Change_ShrubCover, Change_HerbCover,
         Change_BGDensity)

#   BG density, plot change unscaled variables (for datagrid)
dat.bgden.unscaled <- plot.change %>% 
  select(Prev_year_precip, Aspect, PlotSlope, Change_ShrubCover)

#   BG cover, plot change unscaled variables (for datagrid)
dat.bgcov.unscaled <- plot.change %>% 
  select(Prev_year_precip, Aspect, PlotSlope, Change_ShrubCover, Change_HerbCover)
  

#   Survival unscaled variables (for datagrid)
dat.survival.unscaled <- dat.survival.raw %>% 
  select(Prev_year_precip, PlotSlope, ShrubCover, HerbCover,
         BGDensity)
  


# Total culm change -------------------------------------------------------

# With CI, scaled (automatically generated)
# All fixed effects alone 
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
             color = "red") # insight slightly higher



## Total: Aspect ----------------------------------------------------------

# With a boxplot, I can't really graph the prediction on top of the original data, 
#   so they will just have to stay separate graphs.

# Original data (boxplot)
total.change.aspect <- dat.culm %>% 
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

# Graph with CI, prediction only (ggeffects version)
total.pred.aspect <- total.pred.aspect %>% 
  ggplot(aes(x, predicted)) +
  geom_point() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), linetype = "dashed") +
  theme_bw() +
  labs(title = "Prediction for total change by aspect",
       y = expression(Delta ~ "Total culm count"),
       x = NULL) +
  scale_y_continuous(limits = c(-120, 220)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black")) 
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
             color = "red") +
  geom_vline(xintercept = 0,
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
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") # basically the same, just slightly different slope



## Total: Shrub -----------------------------------------------------------

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

# Graph with CI (ggeffects version)
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


# Graph differences in prediction line (dashed is using insight, solid is using ggeffects)
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
             color = "red") # insight just slightly higher



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


# Generate CI with scaled x variable
total.pred.shrub.precip <- predict_response(total_best.model, 
                                            terms = c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"))
total.pred.shrub.precip$group <- factor(total.pred.shrub.precip$group,
                                        levels = c("1.01", "0.01", "-0.99"))

# Graph with CI, scaled prediction only (ggeffects version)  
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
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native shrub cover (scaled)"),
       title = "Change in total culm count vs. shrub cover change (scaled)",
       color = "Previous year \nprecip (scaled)",
       fill = "Previous year \nprecip (scaled)") +
  scale_y_continuous(limits = c(-120, 220))
total.shrub.precip.ci



## Total: Precip * herb ---------------------------------------------------

# Generate prediction and add unscaled variable
viz.total.herb.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
viz.total.herb.precip$Predicted <- get_predicted(total_best.model, viz.total.herb.precip)
unscaled.herb.precip243 <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover)
viz.total.herb.precip$Change_HerbCover <- unscaled.herb.precip243$Change_HerbCover
viz.total.herb.precip$Prev_year_precip <- unscaled.herb.precip243$Prev_year_precip

# Graph (insight version)
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


# Generate CI with scaled x variable 
total.pred.herb.precip <- predict_response(total_best.model, 
                                            terms = c("Change_HerbCover_scaled", "Prev_year_precip_scaled"))
total.pred.herb.precip$group <- factor(total.pred.herb.precip$group,
                                        levels = c("1.01", "0.01", "-0.99"))

# Graph with CI, scaled prediction only (ggeffects version)
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
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native grass & forb cover (scaled)"),
       title = "Change in total culm count vs. herb cover change (scaled)",
       color = "Previous year \nprecip (scaled)",
       fill = "Previous year \nprecip (scaled)") +
  scale_y_continuous(limits = c(-120, 220))
total.herb.precip.ci



## Total: Slope (NS) ------------------------------------------------------

# Generate prediction and add unscaled variable
viz.total.slope <- get_datagrid(dat.culm.ex, by = c("PlotSlope_scaled"),
                                length = 100)
viz.total.slope$Predicted <- get_predicted(total_best.model, viz.total.slope)
unscaled.slope100 <- get_datagrid(dat.culm.unscaled, by = "PlotSlope",
                                  length = 100) %>% 
  arrange(PlotSlope)
viz.total.slope$PlotSlope <- unscaled.slope100$PlotSlope

# Graph (insight version)
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


# Generate CI and add unscaled variable
ci.total.slope <- predict_response(total_best.model, terms = "PlotSlope_scaled")
unscaled.slope12 <- get_datagrid(dat.culm.unscaled, by = "PlotSlope",
                                 length = 12) %>% 
  arrange(PlotSlope)
ci.total.slope$PlotSlope <- unscaled.slope12$PlotSlope
ci.total.slope$Change_Total_Live_Culms <- ci.total.slope$predicted

# Graph with CI (ggeffects version)
total.slope.ci <- dat.culm %>% 
  ggplot(aes(x = PlotSlope, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_ribbon(data = ci.total.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.total.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in total culm count vs. slope") +
  labs(y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
total.slope.ci


# Graph differences in prediction line (dashed is using insight, solid is using ggeffects)
dat.culm %>% 
  ggplot(aes(x = PlotSlope, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_ribbon(data = ci.total.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.total.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.total.slope,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in total culm count vs. slope") +
  labs(y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") # insight just slightly higher



## Total: Herb (NS) -------------------------------------------------------

# Generate prediction and add unscaled variable
viz.total.herb <- get_datagrid(dat.culm.ex, by = c("Change_HerbCover_scaled"),
                                length = 100)
viz.total.herb$Predicted <- get_predicted(total_best.model, viz.total.herb)
unscaled.herb100 <- get_datagrid(dat.culm.unscaled, by = "Change_HerbCover",
                                  length = 100) %>% 
  arrange(Change_HerbCover)
viz.total.herb$Change_HerbCover <- unscaled.herb100$Change_HerbCover

# Graph (insight version)
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


# Generate CI and add unscaled variable
ci.total.herb <- predict_response(total_best.model, terms = "Change_HerbCover_scaled")
unscaled.herb8 <- get_datagrid(dat.culm.unscaled, by = "Change_HerbCover",
                                length = 8) %>% 
  arrange(Change_HerbCover)
ci.total.herb$Change_HerbCover <- unscaled.herb8$Change_HerbCover
ci.total.herb$Change_Total_Live_Culms <- ci.total.herb$predicted

# Graph with CI (ggeffects version)
total.herb.ci <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_ribbon(data = ci.total.herb,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.total.herb,
            aes(y = predicted), linewidth = 1,
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
total.herb.ci


# Graph differences in prediction line (dashed is using insight, solid is using ggeffects)
dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_ribbon(data = ci.total.herb,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.total.herb,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.total.herb,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  ggtitle("Change in total culm count vs. herb cover change") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native herb cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") # insight just slightly higher



## Total: Precip * density (NS) -------------------------------------------

# Generate prediction and add unscaled variable
viz.total.bgden.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_BGDensity_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_BGDensity_scaled)
viz.total.bgden.precip$Predicted <- get_predicted(total_best.model, viz.total.bgden.precip)
unscaled.bgden.precip243 <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_BGDensity", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_BGDensity)
viz.total.bgden.precip$Change_BGDensity <- unscaled.bgden.precip243$Change_BGDensity
viz.total.bgden.precip$Prev_year_precip <- unscaled.bgden.precip243$Prev_year_precip

# Graph (insight version)
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


# Generate CI with scaled x variable 
total.pred.bgden.precip <- predict_response(total_best.model, 
                                            terms = c("Change_BGDensity_scaled", "Prev_year_precip_scaled"))
total.pred.bgden.precip$group <- factor(total.pred.bgden.precip$group,
                                        levels = c("1.01", "0.01", "-0.99"))

# Graph with CI, scaled (ggeffects version)
total.bgden.precip.ci <- total.pred.bgden.precip %>% 
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
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Density (scaled)"),
       title = "Change in total culm count vs. plot density change (scaled)",
       color = "Previous year \nprecip (scaled)",
       fill = "Previous year \nprecip (scaled)") +
  scale_y_continuous(limits = c(-120, 220))
total.bgden.precip.ci




# Test multiple versions of model ------------------------------------------

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

#   Graph (all 8 top models)
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

# Compare graphs - they are basically the same
repro.bgden
repro.bgden.alltop8



# Shrub cover change
#   Generate prediction and add unscaled variable
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
unscaled.shrub100 <- get_datagrid(dat.culm.unscaled, by = "Change_ShrubCover",
                                  length = 100) %>% 
  arrange(Change_ShrubCover)
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

#   Graph (all top 8 models) 
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

# Compare graphs - they are basically the same
repro.shrub
repro.shrub.alltop8




# Reproductive culm change ------------------------------------------------

# With CI, scaled (automatic graphs)
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



## Repro: Aspect ----------------------------------------------------------

# Original data (boxplot)
repro.change.aspect <- dat.culm %>% 
  ggplot(aes(x = Aspect, y = Change_Reproductive_culms)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  theme_bw() +
  labs(title = "Change in repro culm count by aspect",
       y = expression(Delta ~ "Reproductive culm count"),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
repro.change.aspect


# Generate CI
repro.pred.aspect <- predict_response(repro_best.model, terms = "Aspect")

# Graph with CI, prediction only (ggeffects version)
repro.pred.aspect <- repro.pred.aspect %>% 
  ggplot(aes(x, predicted)) +
  geom_point() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), linetype = "dashed") +
  theme_bw() +
  labs(title = "Prediction for repro change by aspect",
       y = expression(Delta ~ "Reproductive culm count"),
       x = NULL) +
  scale_y_continuous(limits = c(-120, 220)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black")) 
repro.pred.aspect



## Repro: BG density -------------------------------------------------------

# Generate prediction and add unscaled variable
viz.repro.bgden <- get_datagrid(dat.culm.ex, by = c("Change_BGDensity_scaled"),
                                length = 100)
viz.repro.bgden$Predicted <- get_predicted(repro_best.model, viz.repro.bgden)
unscaled.bgden100 <- get_datagrid(dat.culm.unscaled, by = "Change_BGDensity",
                                  length = 100) %>% 
  arrange(Change_BGDensity)
viz.repro.bgden$Change_BGDensity <- unscaled.bgden100$Change_BGDensity

# Graph (insight version)
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


# Generate CI and add unscaled variable
ci.repro.bgden <- predict_response(repro_best.model, terms = "Change_BGDensity_scaled")
unscaled.bgden9 <- get_datagrid(dat.culm.unscaled, by = "Change_BGDensity",
                                length = 9) %>% 
  arrange(Change_BGDensity)
ci.repro.bgden$Change_BGDensity <- unscaled.bgden9$Change_BGDensity
ci.repro.bgden$Change_Reproductive_culms <- ci.repro.bgden$predicted

# Graph with CI (ggeffects version)
repro.bgden.ci <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_ribbon(data = ci.repro.bgden,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.repro.bgden,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  ggtitle("Change in repro culm count vs. plot density change") +
  labs(y = expression(Delta ~ "Repro culm count"),
       x = expression(Delta ~ paste("Density (individuals / ", m^2, ")"))) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
repro.bgden.ci


# Graph differences in prediction line (dashed is using insight, solid is using ggeffects)
dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_ribbon(data = ci.repro.bgden,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.repro.bgden,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.repro.bgden,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  labs(y = expression(Delta ~ "Repro culm count"),
       x = expression(Delta ~ paste("Density (individuals / ", m^2, ")"))) +
  ggtitle("Change in repro culm count vs. plot density") +
  labs(y = expression(Delta ~ "Repro culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") # basically the same



## Repro: Shrub -----------------------------------------------------------

# Generate prediction with unscaled variable added
viz.repro.shrub <- get_datagrid(dat.culm.ex, by = c("Change_ShrubCover_scaled"),
                                length = 100)
viz.repro.shrub$Predicted <- get_predicted(repro_best.model, viz.repro.shrub)
unscaled.shrub100 <- get_datagrid(dat.culm.unscaled, by = "Change_ShrubCover",
                                  length = 100) %>% 
  arrange(Change_ShrubCover)
viz.repro.shrub$Change_ShrubCover <- unscaled.shrub100$Change_ShrubCover

# Graph (insight version)
repro.shrub <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_line(data = viz.repro.shrub,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  ggtitle("Change in repro culm count vs. shrub cover change") +
  labs(y = expression(Delta ~ "Repro culm count"),
       x = expression(Delta ~ "Native shrub cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
repro.shrub


# Generate CI and add unscaled variable
ci.repro.shrub <- predict_response(repro_best.model, terms = "Change_ShrubCover_scaled")
unscaled.shrub16 <- get_datagrid(dat.culm.unscaled, by = "Change_ShrubCover",
                                 length = 16) %>% 
  arrange(Change_ShrubCover)
ci.repro.shrub$Change_ShrubCover <- unscaled.shrub16$Change_ShrubCover
ci.repro.shrub$Change_Reproductive_culms <- ci.repro.shrub$predicted

# Graph with CI (ggeffects version)
repro.shrub.ci <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_ribbon(data = ci.repro.shrub,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.repro.shrub,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  ggtitle("Change in repro culm count vs. shrub cover change") +
  labs(y = expression(Delta ~ "Repro culm count"),
       x = expression(Delta ~ "Native shrub cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
repro.shrub.ci


# Graph differences in prediction line (dashed is using insight, solid is using ggeffects)
dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_ribbon(data = ci.repro.shrub,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.repro.shrub,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.repro.shrub,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  ggtitle("Change in repro culm count vs. shrub cover change") +
  labs(y = expression(Delta ~ "Repro culm count"),
       x = expression(Delta ~ "Native shrub cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") # basically the same



## Repro: Herb ------------------------------------------------------------

# Generate prediction and add unscaled variable
viz.repro.herb <- get_datagrid(dat.culm.ex, by = c("Change_HerbCover_scaled"),
                               length = 100)
viz.repro.herb$Predicted <- get_predicted(repro_best.model, viz.repro.herb)
unscaled.herb100 <- get_datagrid(dat.culm.unscaled, by = "Change_HerbCover",
                                 length = 100) %>% 
  arrange(Change_HerbCover)
viz.repro.herb$Change_HerbCover <- unscaled.herb100$Change_HerbCover

# Graph (insight version)
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


# Generate CI and add unscaled variable
ci.repro.herb <- predict_response(repro_best.model, terms = "Change_HerbCover_scaled")
unscaled.herb8 <- get_datagrid(dat.culm.unscaled, by = "Change_HerbCover",
                               length = 8) %>% 
  arrange(Change_HerbCover)
ci.repro.herb$Change_HerbCover <- unscaled.herb8$Change_HerbCover
ci.repro.herb$Change_Reproductive_culms <- ci.repro.herb$predicted

# Graph with CI (ggeffects version)
repro.herb.ci <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_ribbon(data = ci.repro.herb,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.repro.herb,
            aes(y = predicted), linewidth = 1,
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
repro.herb.ci


# Graph differences in prediction line (dashed is using insight, solid is using ggeffects)
dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_ribbon(data = ci.repro.herb,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.repro.herb,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.repro.herb,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  ggtitle("Change in repro culm count vs. herb cover change") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ "Native herb cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") # insight just slightly higher



## Repro: Precip (NS) -----------------------------------------------------

# Generate prediction and add unscaled variable
viz.repro.precip <- get_datagrid(dat.culm.ex, by = c("Prev_year_precip_scaled"),
                                 length = 100)
viz.repro.precip$Predicted <- get_predicted(repro_best.model, viz.repro.precip)
unscaled.precip100 <- get_datagrid(dat.culm.unscaled, by = "Prev_year_precip",
                                   length = 100) %>% 
  arrange(Prev_year_precip)
viz.repro.precip$Prev_year_precip <- unscaled.precip100$Prev_year_precip

# Graph (insight version)
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


# Generate CI and add unscaled variable
ci.repro.precip <- predict_response(repro_best.model, terms = "Prev_year_precip_scaled")
unscaled.precip15 <- get_datagrid(dat.culm.unscaled, by = "Prev_year_precip",
                                  length = 15) %>% 
  arrange(Prev_year_precip)
ci.repro.precip$Prev_year_precip <- unscaled.precip15$Prev_year_precip
ci.repro.precip$Change_Reproductive_culms <- ci.repro.precip$predicted 

# Graph with CI (ggeffects version)
repro.precip.ci <- dat.culm %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_ribbon(data = ci.repro.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.repro.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  xlab("Previous year precip (mm)") +
  ggtitle("Change in repro culm count vs. precip") +
  labs(y = expression(Delta ~ "Reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
repro.precip.ci


# Graph differences in prediction line (dashed is using insight, solid is from ggeffects)
dat.culm %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_ribbon(data = ci.repro.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.repro.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.repro.precip,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  xlab("Previous year precip (mm)") +
  ggtitle("Change in repro culm count vs. precip") +
  labs(y = expression(Delta ~ "Reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") # basically the same



## Repro: Slope (NS) -----------------------------------------------------

# Generate prediction and add unscaled variable (use model2, as best model does not include slope)
viz.repro.slope <- get_datagrid(dat.culm.ex, by = c("PlotSlope_scaled"),
                                length = 100)
viz.repro.slope$Predicted <- get_predicted(repro_model2, viz.repro.slope)
unscaled.slope100 <- get_datagrid(dat.culm.unscaled, by = "PlotSlope",
                                  length = 100) %>% 
  arrange(PlotSlope)
viz.repro.slope$PlotSlope <- unscaled.slope100$PlotSlope

# Graph (insight version)
repro.slope <- dat.culm %>% 
  ggplot(aes(x = PlotSlope, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_line(data = viz.repro.slope,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in repro culm count vs. slope") +
  labs(y = expression(Delta ~ "Reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
repro.slope


# Generate CI and add unscaled variable
ci.repro.slope <- predict_response(repro_model2, terms = "PlotSlope_scaled")
unscaled.slope12 <- get_datagrid(dat.culm.unscaled, by = "PlotSlope",
                                 length = 12) %>% 
  arrange(PlotSlope)
ci.repro.slope$PlotSlope <- unscaled.slope12$PlotSlope
ci.repro.slope$Change_Reproductive_culms <- ci.repro.slope$predicted

# Graph with CI (ggeffects version)
repro.slope.ci <- dat.culm %>% 
  ggplot(aes(x = PlotSlope, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_ribbon(data = ci.repro.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.repro.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in repro culm count vs. slope") +
  labs(y = expression(Delta ~ "Reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
repro.slope.ci


# Graph differences in prediction line (dashed is using insight, solid is using ggeffects)
dat.culm %>% 
  ggplot(aes(x = PlotSlope, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_ribbon(data = ci.repro.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.repro.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.repro.slope,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in repro culm count vs. slope") +
  labs(y = expression(Delta ~ "Reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") # basically the same



## Repro: Precip * density (NS) -------------------------------------------

# Generate prediction and add unscaled variable
viz.repro.bgden.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_BGDensity_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_BGDensity_scaled)
viz.repro.bgden.precip$Predicted <- get_predicted(repro_best.model, viz.repro.bgden.precip)
unscaled.bgden.precip243 <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_BGDensity", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_BGDensity)
viz.repro.bgden.precip$Change_BGDensity <- unscaled.bgden.precip243$Change_BGDensity
viz.repro.bgden.precip$Prev_year_precip <- unscaled.bgden.precip243$Prev_year_precip

# Graph (insight version)
repro.bgden.precip <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Reproductive_culms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = viz.repro.bgden.precip,
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
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ paste("Density (individuals / ", m^2, ")")),
       title = "Change in repro culm count vs. plot density change")
repro.bgden.precip


# Generate CI with scaled x variable 
repro.pred.bgden.precip <- predict_response(repro_best.model, 
                                            terms = c("Change_BGDensity_scaled", "Prev_year_precip_scaled"))
repro.pred.bgden.precip$group <- factor(repro.pred.bgden.precip$group,
                                        levels = c("1.01", "0.01", "-0.99"))

# Graph with CI, scaled (ggeffects version)
repro.bgden.precip.ci <- repro.pred.bgden.precip %>% 
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
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ "Density (scaled)"),
       title = "Change in repro culm count vs. plot density change (scaled)",
       color = "Previous year \nprecip (scaled)",
       fill = "Previous year \nprecip (scaled)") +
  scale_y_continuous(limits = c(-120, 220))
repro.bgden.precip.ci



## Repro: Precip * shrub (NS) ---------------------------------------------

# Generate prediction and add unscaled variable
viz.repro.shrub.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled)
viz.repro.shrub.precip$Predicted <- get_predicted(repro_best.model, viz.repro.shrub.precip)
unscaled.shrub.precip243 <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover)
viz.repro.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip243$Change_ShrubCover
viz.repro.shrub.precip$Prev_year_precip <- unscaled.shrub.precip243$Prev_year_precip

# Graph (insight version)
repro.shrub.precip <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Reproductive_culms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = viz.repro.shrub.precip,
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
  labs(y = expression(Delta ~ "Repro culm count"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in repro culm count vs. shrub cover change")
repro.shrub.precip


# Generate CI with scaled x variable
repro.pred.shrub.precip <- predict_response(repro_best.model, 
                                            terms = c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"))
repro.pred.shrub.precip$group <- factor(repro.pred.shrub.precip$group,
                                        levels = c("1.01", "0.01", "-0.99"))

# Graph with CI, scaled prediction only (ggeffects version)  
repro.shrub.precip.ci <- repro.pred.shrub.precip %>% 
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
  labs(y = expression(Delta ~ "Repro culm count"),
       x = expression(Delta ~ "Native shrub cover (scaled)"),
       title = "Change in repro culm count vs. shrub cover change (scaled)",
       color = "Previous year \nprecip (scaled)",
       fill = "Previous year \nprecip (scaled)") +
  scale_y_continuous(limits = c(-120, 220))
repro.shrub.precip.ci



## Repro: Precip * herb (NS) ----------------------------------------------

# Generate prediction and add unscaled variable
viz.repro.herb.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
viz.repro.herb.precip$Predicted <- get_predicted(repro_best.model, viz.repro.herb.precip)
unscaled.herb.precip243 <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover)
viz.repro.herb.precip$Change_HerbCover <- unscaled.herb.precip243$Change_HerbCover
viz.repro.herb.precip$Prev_year_precip <- unscaled.herb.precip243$Prev_year_precip

# Graph (insight version)
repro.herb.precip <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Reproductive_culms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = viz.repro.herb.precip,
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
  labs(y = expression(Delta ~ "Repro culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in repro culm count vs. herb cover change")
repro.herb.precip


# Generate CI with scaled x variable 
repro.pred.herb.precip <- predict_response(repro_best.model, 
                                           terms = c("Change_HerbCover_scaled", "Prev_year_precip_scaled"))
repro.pred.herb.precip$group <- factor(repro.pred.herb.precip$group,
                                       levels = c("1.01", "0.01", "-0.99"))

# Graph with CI, scaled prediction only (ggeffects version)
repro.herb.precip.ci <- repro.pred.herb.precip %>% 
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
  labs(y = expression(Delta ~ "Repro culm count"),
       x = expression(Delta ~ "Native grass & forb cover (scaled)"),
       title = "Change in repro culm count vs. herb cover change (scaled)",
       color = "Previous year \nprecip (scaled)",
       fill = "Previous year \nprecip (scaled)") +
  scale_y_continuous(limits = c(-120, 220))
repro.herb.precip.ci




# Buffelgrass density change ----------------------------------------------

# With CI, scaled (automatically generated)
# All fixed effects alone
bgden.pred <- predict_response(bgden_best.model)
plot(bgden.pred)

# Precip * shrub change
bgden.pred.shrub.precip <- predict_response(bgden_best.model, 
                                            terms = c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"))
plot(bgden.pred.shrub.precip)



## BG density: Precip -----------------------------------------------------

# Best model
# Generate prediction and add unscaled variable - best model
viz.bgden.precip <- get_datagrid(dat.bgden.ex, by = c("Prev_year_precip_scaled"),
                                 length = 100)
viz.bgden.precip$Predicted <- get_predicted(bgden_best.model, viz.bgden.precip)
unscaled.precip100 <- get_datagrid(dat.bgden.unscaled, by = "Prev_year_precip",
                                   length = 100) %>% 
  arrange(Prev_year_precip)
viz.bgden.precip$Prev_year_precip <- unscaled.precip100$Prev_year_precip

# Graph (insight version) - best model
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


# Generate CI and add unscaled variable - best model
ci.bgden.precip <- predict_response(bgden_best.model, terms = "Prev_year_precip_scaled")
unscaled.precip15 <- get_datagrid(dat.bgden.unscaled, by = "Prev_year_precip",
                                  length = 15) %>% 
  arrange(Prev_year_precip)
ci.bgden.precip$Prev_year_precip <- unscaled.precip15$Prev_year_precip
ci.bgden.precip$Change_BGDensity <- ci.bgden.precip$predicted 

# Graph with CI (ggeffects version) - best model
bgden.precip.ci <- dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGDensity)) +
  geom_point() +
  geom_ribbon(data = ci.bgden.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.bgden.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  labs(x = "Previous year precip (mm)",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       title = "Change in buffelgrass density vs. precip") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
bgden.precip.ci


# Graph differences in prediction line (dashed is using insight, solid is from ggeffects) - best model
dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGDensity)) +
  geom_point() +
  geom_ribbon(data = ci.bgden.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.bgden.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.bgden.precip,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  labs(x = "Previous year precip (mm)",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       title = "Change in buffelgrass density vs. precip") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") # insight is much lower (outside of ggeffects SE); insight seems to fit data better


# Model 2
# Generate prediction and add unscaled variable - model 2
viz.bgden.precip2 <- get_datagrid(dat.bgden.ex, by = c("Prev_year_precip_scaled"),
                                 length = 100)
viz.bgden.precip2$Predicted <- get_predicted(bgden_model2, viz.bgden.precip)
unscaled.precip100 <- get_datagrid(dat.bgden.unscaled, by = "Prev_year_precip",
                                   length = 100) %>% 
  arrange(Prev_year_precip)
viz.bgden.precip2$Prev_year_precip <- unscaled.precip100$Prev_year_precip

# Graph (insight version) - model 2
bgden.precip2 <- dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGDensity)) +
  geom_point() +
  geom_line(data = viz.bgden.precip2,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Previous year precip (mm)",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       title = "Change in buffelgrass density vs. precip (model 2)")
bgden.precip2 # (lol it is basically the same)


# Generate CI and add unscaled variable - model 2
ci.bgden.precip2 <- predict_response(bgden_model2, terms = "Prev_year_precip_scaled")
unscaled.precip15 <- get_datagrid(dat.bgden.unscaled, by = "Prev_year_precip",
                                  length = 15) %>% 
  arrange(Prev_year_precip)
ci.bgden.precip2$Prev_year_precip <- unscaled.precip15$Prev_year_precip
ci.bgden.precip2$Change_BGDensity <- ci.bgden.precip2$predicted 

# Graph with CI (ggeffects version) - model 2
bgden.precip.ci2 <- dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGDensity)) +
  geom_point() +
  geom_ribbon(data = ci.bgden.precip2,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.bgden.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  labs(x = "Previous year precip (mm)",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       title = "Change in buffelgrass density vs. precip (model 2)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
bgden.precip.ci2


# Graph differences in prediction line (dashed is using insight, solid is from ggeffects) - model 2
dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGDensity)) +
  geom_point() +
  geom_ribbon(data = ci.bgden.precip2,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.bgden.precip2,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.bgden.precip2,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  labs(x = "Previous year precip (mm)",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       title = "Change in buffelgrass density vs. precip (model 2)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") # differences are still the same as model 1



## BG density: Aspect -----------------------------------------------------

# Original data (boxplot)
bgden.change.aspect <- dat.plot %>% 
  ggplot(aes(x = Aspect, y = Change_BGDensity)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  theme_bw() +
  labs(title = "Change in plot density by aspect",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
bgden.change.aspect


# Generate CI
bgden.pred.aspect <- predict_response(bgden_best.model, terms = "Aspect")
bgden.pred.aspect <- as.data.frame(bgden.pred.aspect)

# Graph with CI, prediction only (ggeffects version)
bgden.pred.aspect <- bgden.pred.aspect %>% 
  ggplot(aes(x, predicted)) +
  geom_point() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), linetype = "dashed") +
  theme_bw() +
  labs(title = "Prediction for plot density change by aspect",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = NULL) +
  scale_y_continuous(limits = c(-24, 32),
                     breaks = c(-20, 0, 20)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black")) 
bgden.pred.aspect



## BG density: Shrub ------------------------------------------------------

# Generate prediction and add unscaled variable
viz.bgden.shrub <- get_datagrid(dat.bgden.ex, by = c("Change_ShrubCover_scaled"),
                                length = 100)
viz.bgden.shrub$Predicted <- get_predicted(bgden_best.model, viz.bgden.shrub)
unscaled.shrub100 <- get_datagrid(dat.bgden.unscaled, by = "Change_ShrubCover",
                                  length = 100) %>% 
  arrange(Change_ShrubCover)
viz.bgden.shrub$Change_ShrubCover <- unscaled.shrub100$Change_ShrubCover

# Graph (insight version)
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


# Generate CI and add unscaled variable
ci.bgden.shrub <- predict_response(bgden_best.model, terms = "Change_ShrubCover_scaled")
unscaled.shrub16 <- get_datagrid(dat.bgden.unscaled, by = "Change_ShrubCover",
                                 length = 16) %>% 
  arrange(Change_ShrubCover)
ci.bgden.shrub$Change_ShrubCover <- unscaled.shrub16$Change_ShrubCover
ci.bgden.shrub$Change_BGDensity <- ci.bgden.shrub$predicted

# Graph with CI (ggeffects version)
bgden.shrub.ci <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGDensity)) +
  geom_point() +
  geom_ribbon(data = ci.bgden.shrub,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.bgden.shrub,
            aes(y = predicted), linewidth = 1,
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
bgden.shrub.ci


# Graph differences in prediction line (dashed is using insight, solid is using ggeffects)
dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGDensity)) +
  geom_point() +
  geom_ribbon(data = ci.bgden.shrub,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.bgden.shrub,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.bgden.shrub,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffelgrass density vs. shrub cover change") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") # insight much lower



## BG density: Precip * shrub ---------------------------------------------

# Generate prediction and add unscaled variable
viz.bgden.shrub.precip <- dat.bgden.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled)
viz.bgden.shrub.precip$Predicted <- get_predicted(bgden_best.model, viz.bgden.shrub.precip)
unscaled.shrub.precip243 <- dat.bgden.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover)
viz.bgden.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip243$Change_ShrubCover
viz.bgden.shrub.precip$Prev_year_precip <- unscaled.shrub.precip243$Prev_year_precip

# Graph (insight version)
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


# Generate CI with scaled x variable
bgden.pred.shrub.precip <- predict_response(bgden_best.model, 
                                            terms = c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"))
bgden.pred.shrub.precip$group <- factor(bgden.pred.shrub.precip$group,
                                        levels = c("1.24", "0.18", "-0.89"))
bgden.pred.shrub.precip <- as.data.frame(bgden.pred.shrub.precip)

# Graph with CI, scaled prediction only (ggeffects version)  
bgden.shrub.precip.ci <- bgden.pred.shrub.precip %>% 
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
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = expression(Delta ~ "Native shrub cover (scaled)"),
       title = "Change in buffelgrass density vs. shrub cover change (scaled)",
       color = "Previous year \nprecip (scaled)",
       fill = "Previous year \nprecip (scaled)") +
  scale_y_continuous(limits = c(-24, 32),
                     breaks = c(-20, 0, 20)) 
bgden.shrub.precip.ci # insight again lower and seems to fit data better?



## BG density: Slope (NS) -------------------------------------------------

# Generate prediction and add unscaled variable (use model2, as best model does not include slope)
viz.bgden.slope <- get_datagrid(dat.bgden.ex, by = c("PlotSlope_scaled"),
                                length = 100)
viz.bgden.slope$Predicted <- get_predicted(bgden_model2, viz.bgden.slope)
unscaled.slope100 <- get_datagrid(dat.bgden.unscaled, by = "PlotSlope",
                                  length = 100) %>% 
  arrange(PlotSlope)
viz.bgden.slope$PlotSlope <- unscaled.slope100$PlotSlope

# Graph (insight version)
bgden.slope <- dat.plot %>% 
  ggplot(aes(x = PlotSlope, y = Change_BGDensity)) +
  geom_point() +
  geom_line(data = viz.bgden.slope,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in buffelgrass density vs. slope") +
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")"))) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
bgden.slope


# Generate CI and add unscaled variable
ci.bgden.slope <- predict_response(bgden_model2, terms = "PlotSlope_scaled")
unscaled.slope12 <- get_datagrid(dat.bgden.unscaled, by = "PlotSlope",
                                 length = 12) %>% 
  arrange(PlotSlope)
ci.bgden.slope$PlotSlope <- unscaled.slope12$PlotSlope
ci.bgden.slope$Change_BGDensity <- ci.bgden.slope$predicted

# Graph with CI (ggeffects version)
bgden.slope.ci <- dat.plot %>% 
  ggplot(aes(x = PlotSlope, y = Change_BGDensity)) +
  geom_point() +
  geom_ribbon(data = ci.bgden.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.bgden.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in bgden culm count vs. slope") +
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")"))) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
bgden.slope.ci


# Graph differences in prediction line (dashed is using insight, solid is using ggeffects)
dat.plot %>% 
  ggplot(aes(x = PlotSlope, y = Change_BGDensity)) +
  geom_point() +
  geom_ribbon(data = ci.bgden.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.bgden.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.bgden.slope,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in bgden culm count vs. slope") +
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")"))) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") # insight again lower




# Buffelgrass cover change ------------------------------------------------

# With CI, scaled (automatic graphs)
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



## BG cover: Precip -------------------------------------------------------

# Generate prediction and add scaled variable 
viz.bgcov.precip <- get_datagrid(dat.bgcov.ex, by = c("Prev_year_precip_scaled"),
                                 length = 100)
viz.bgcov.precip$Predicted <- get_predicted(bgcov_best.model, viz.bgcov.precip)
unscaled.precip100 <- get_datagrid(dat.bgcov.unscaled, by = "Prev_year_precip",
                                   length = 100) %>% 
  arrange(Prev_year_precip)
viz.bgcov.precip$Prev_year_precip <- unscaled.precip100$Prev_year_precip

# Graph (insight version)
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


# Generate CI and add unscaled variable
ci.bgcov.precip <- predict_response(bgcov_best.model, terms = "Prev_year_precip_scaled")
unscaled.precip15 <- get_datagrid(dat.bgcov.unscaled, by = "Prev_year_precip",
                                  length = 15) %>% 
  arrange(Prev_year_precip)
ci.bgcov.precip$Prev_year_precip <- unscaled.precip15$Prev_year_precip
ci.bgcov.precip$Change_BGCover <- ci.bgcov.precip$predicted 

# Graph with CI (ggeffects version)
bgcov.precip.ci <- dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGCover)) +
  geom_point() +
  geom_ribbon(data = ci.bgcov.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.bgcov.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  labs(x = "Previous year precip (mm)",
       y = expression(Delta ~ "Cover (%)"),
       title = "Change in buffelgrass cover vs. precip") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
bgcov.precip.ci


# Graph differences in prediction line (dashed is using insight, solid is from ggeffects)
dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGCover)) +
  geom_point() +
  geom_ribbon(data = ci.bgcov.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.bgcov.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.bgcov.precip,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  labs(x = "Previous year precip (mm)",
       y = expression(Delta ~ "Cover (%)"),
       title = "Change in buffelgrass cover vs. precip") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") # insight is much lower (outside of ggeffects SE); insight seems to fit data better



## BG cover: Aspect -------------------------------------------------------

# Original data (boxplot)
bgcov.change.aspect <- dat.plot %>% 
  ggplot(aes(x = Aspect, y = Change_BGCover)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  theme_bw() +
  labs(title = "Change in plot cover by aspect",
       y = expression(Delta ~ "Cover (%)"),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
bgcov.change.aspect


# Generate CI
bgcov.pred.aspect <- predict_response(bgcov_best.model, terms = "Aspect")
bgcov.pred.aspect <- as.data.frame(bgcov.pred.aspect)

# Graph with CI, prediction only (ggeffects version)
bgcov.pred.aspect <- bgcov.pred.aspect %>% 
  ggplot(aes(x, predicted)) +
  geom_point() +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), linetype = "dashed") +
  theme_bw() +
  labs(title = "Prediction for plot cover change by aspect",
       y = expression(Delta ~ "Cover (%)"),
       x = NULL) +
  scale_y_continuous(limits = c(-50, 45),
                     breaks = c(-50, -25, 0, 25)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black")) 
bgcov.pred.aspect



## BG cover: Shrub --------------------------------------------------------

# Generate prediction and add scaled variable 
viz.bgcov.shrub <- get_datagrid(dat.bgcov.ex, by = c("Change_ShrubCover_scaled"),
                                length = 100)
viz.bgcov.shrub$Predicted <- get_predicted(bgcov_best.model, viz.bgcov.shrub)
unscaled.shrub100 <- get_datagrid(dat.bgcov.unscaled, by = "Change_ShrubCover",
                                  length = 100) %>% 
  arrange(Change_ShrubCover)
viz.bgcov.shrub$Change_ShrubCover <- unscaled.shrub100$Change_ShrubCover

# Graph (insight version)
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


# Generate CI and add unscaled variable
ci.bgcov.shrub <- predict_response(bgcov_best.model, terms = "Change_ShrubCover_scaled")
unscaled.shrub16 <- get_datagrid(dat.bgcov.unscaled, by = "Change_ShrubCover",
                                 length = 16) %>% 
  arrange(Change_ShrubCover)
ci.bgcov.shrub$Change_ShrubCover <- unscaled.shrub16$Change_ShrubCover
ci.bgcov.shrub$Change_BGCover <- ci.bgcov.shrub$predicted

# Graph with CI (ggeffects version)
bgcov.shrub.ci <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGCover)) +
  geom_point() +
  geom_ribbon(data = ci.bgcov.shrub,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.bgcov.shrub,
            aes(y = predicted), linewidth = 1,
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
bgcov.shrub.ci


# Graph differences in prediction line (dashed is using insight, solid is using ggeffects)
dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGCover)) +
  geom_point() +
  geom_ribbon(data = ci.bgcov.shrub,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.bgcov.shrub,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.bgcov.shrub,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  labs(y = expression(Delta ~ "Cover (%)"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffelgrass cover vs. shrub cover change") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") # insight again lower, seems to fit data better



## BG cover: Slope (NS) ---------------------------------------------------

# Generate prediction and add unscaled variable 
viz.bgcov.slope <- get_datagrid(dat.bgcov.ex, by = c("PlotSlope_scaled"),
                                length = 100)
viz.bgcov.slope$Predicted <- get_predicted(bgcov_best.model, viz.bgcov.slope)
unscaled.slope100 <- get_datagrid(dat.bgcov.unscaled, by = "PlotSlope",
                                  length = 100) %>% 
  arrange(PlotSlope)
viz.bgcov.slope$PlotSlope <- unscaled.slope100$PlotSlope

# Graph (insight version)
bgcov.slope <- dat.plot %>% 
  ggplot(aes(x = PlotSlope, y = Change_BGCover)) +
  geom_point() +
  geom_line(data = viz.bgcov.slope,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in buffelgrass cover vs. slope") +
  labs(y = expression(Delta ~ "Cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
bgcov.slope


# Generate CI and add unscaled variable
ci.bgcov.slope <- predict_response(bgcov_best.model, terms = "PlotSlope_scaled")
unscaled.slope12 <- get_datagrid(dat.bgcov.unscaled, by = "PlotSlope",
                                 length = 12) %>% 
  arrange(PlotSlope)
ci.bgcov.slope$PlotSlope <- unscaled.slope12$PlotSlope
ci.bgcov.slope$Change_BGCover <- ci.bgcov.slope$predicted

# Graph with CI (ggeffects version)
bgcov.slope.ci <- dat.plot %>% 
  ggplot(aes(x = PlotSlope, y = Change_BGCover)) +
  geom_point() +
  geom_ribbon(data = ci.bgcov.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.bgcov.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in buffelgrass cover vs. slope") +
  labs(y = expression(Delta ~ "Cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
bgcov.slope.ci


# Graph differences in prediction line (dashed is using insight, solid is using ggeffects)
dat.plot %>% 
  ggplot(aes(x = PlotSlope, y = Change_BGCover)) +
  geom_point() +
  geom_ribbon(data = ci.bgcov.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.bgcov.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.bgcov.slope,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in buffelgrass cover vs. slope") +
  labs(y = expression(Delta ~ "Cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") # insight again lower



## BG cover: Herb (NS) ----------------------------------------------------

# Generate prediction and add scaled variable 
viz.bgcov.herb <- get_datagrid(dat.bgcov.ex, by = c("Change_HerbCover_scaled"),
                               length = 100)
viz.bgcov.herb$Predicted <- get_predicted(bgcov_best.model, viz.bgcov.herb)
unscaled.herb100 <- get_datagrid(dat.bgcov.unscaled, by = "Change_HerbCover",
                                 length = 100) %>% 
  arrange(Change_HerbCover)
viz.bgcov.herb$Change_HerbCover <- unscaled.herb100$Change_HerbCover

# Graph (insight version)
bgcov.herb <- dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGCover)) +
  geom_point() +
  geom_line(data = viz.bgcov.herb,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  labs(y = expression(Delta ~ "Cover (%)"),
       x = expression(Delta ~ "Native herb cover (%)"),
       title = "Change in buffelgrass cover vs. herb cover change") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
bgcov.herb


# Generate CI and add unscaled variable
ci.bgcov.herb <- predict_response(bgcov_best.model, terms = "Change_HerbCover_scaled")
unscaled.herb8 <- get_datagrid(dat.bgcov.unscaled, by = "Change_HerbCover",
                                length = 8) %>% 
  arrange(Change_HerbCover)
ci.bgcov.herb$Change_HerbCover <- unscaled.herb8$Change_HerbCover
ci.bgcov.herb$Change_BGCover <- ci.bgcov.herb$predicted

# Graph with CI (ggeffects version)
bgcov.herb.ci <- dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGCover)) +
  geom_point() +
  geom_ribbon(data = ci.bgcov.herb,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.bgcov.herb,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  labs(y = expression(Delta ~ "Cover (%)"),
       x = expression(Delta ~ "Native herb cover (%)"),
       title = "Change in buffelgrass cover vs. herb cover change") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
bgcov.herb.ci


# Graph differences in prediction line (dashed is using insight, solid is using ggeffects)
dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGCover)) +
  geom_point() +
  geom_ribbon(data = ci.bgcov.herb,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.bgcov.herb,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.bgcov.herb,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  labs(y = expression(Delta ~ "Cover (%)"),
       x = expression(Delta ~ "Native herb cover (%)"),
       title = "Change in buffelgrass cover vs. herb cover change") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") # insight again lower, seems to fit data better



## BG cover: Precip * shrub (NS) ------------------------------------------

# Generate prediction and add unscaled variable
viz.bgcov.shrub.precip <- dat.bgcov.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled)
viz.bgcov.shrub.precip$Predicted <- get_predicted(bgcov_best.model, viz.bgcov.shrub.precip)
unscaled.shrub.precip243 <- dat.bgcov.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover)
viz.bgcov.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip243$Change_ShrubCover
viz.bgcov.shrub.precip$Prev_year_precip <- unscaled.shrub.precip243$Prev_year_precip

# Graph (insight version)
bgcov.shrub.precip <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGCover,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = viz.bgcov.shrub.precip,
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
  labs(y = expression(Delta ~ "Cover (%)"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffelgrass cover vs. shrub cover change")
bgcov.shrub.precip


# Generate CI with scaled x variable
bgcov.pred.shrub.precip <- predict_response(bgcov_best.model, 
                                            terms = c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"))
bgcov.pred.shrub.precip$group <- factor(bgcov.pred.shrub.precip$group,
                                        levels = c("1.24", "0.18", "-0.89"))
bgcov.pred.shrub.precip <- as.data.frame(bgcov.pred.shrub.precip)

# Graph with CI, scaled prediction only (ggeffects version)  
bgcov.shrub.precip.ci <- bgcov.pred.shrub.precip %>% 
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
  labs(y = expression(Delta ~ "Cover (%)"),
       x = expression(Delta ~ "Native shrub cover (scaled)"),
       title = "Change in buffelgrass cover vs. shrub cover change (scaled)",
       color = "Previous year \nprecip (scaled)",
       fill = "Previous year \nprecip (scaled)") +
  scale_y_continuous(limits = c(-50, 45),
                     breaks = c(-50, -25, 0, 25))
bgcov.shrub.precip.ci # insight again lower



## BG cover: Precip * herb (NS) -------------------------------------------

# Generate prediction and add unscaled variable
viz.bgcov.herb.precip <- dat.bgcov.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
viz.bgcov.herb.precip$Predicted <- get_predicted(bgcov_best.model, viz.bgcov.herb.precip)
unscaled.herb.precip243 <- dat.bgcov.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover)
viz.bgcov.herb.precip$Change_HerbCover <- unscaled.herb.precip243$Change_HerbCover
viz.bgcov.herb.precip$Prev_year_precip <- unscaled.herb.precip243$Prev_year_precip

# Graph (insight version)
bgcov.herb.precip <- dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGCover,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = viz.bgcov.herb.precip,
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
  labs(y = expression(Delta ~ "Cover (%)"),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in buffelgrass cover vs. herb cover change")
bgcov.herb.precip


# Generate CI with scaled x variable
bgcov.pred.herb.precip <- predict_response(bgcov_best.model, 
                                           terms = c("Change_HerbCover_scaled", "Prev_year_precip_scaled"))
bgcov.pred.herb.precip$group <- factor(bgcov.pred.herb.precip$group,
                                       levels = c("1.24", "0.18", "-0.89"))
bgcov.pred.herb.precip <- as.data.frame(bgcov.pred.herb.precip)

# Graph with CI, scaled prediction only (ggeffects version)  
bgcov.herb.precip.ci <- bgcov.pred.herb.precip %>% 
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
  labs(y = expression(Delta ~ "Cover (%)"),
       x = expression(Delta ~ "Native grass & forb cover (scaled)"),
       title = "Change in buffelgrass cover vs. herb cover change (scaled)",
       color = "Previous year \nprecip (scaled)",
       fill = "Previous year \nprecip (scaled)") +
  scale_y_continuous(limits = c(-50, 45),
                     breaks = c(-50, -25, 0, 25))
bgcov.herb.precip.ci # insight again lower 




# Survival ----------------------------------------------------------------

# With CI, scaled (automatically generated)
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



## Survival: Precip -------------------------------------------------------

# Generate prediction and add scaled variable 
viz.survival.precip <- get_datagrid(dat.survival.ex, by = c("Prev_year_precip_scaled"),
                                    length = 50)
viz.survival.precip$Predicted <- get_predicted(survival_best.model, viz.survival.precip) # Predicting new random effect levels for terms: 1 | Transect:Site
unscaled.precip50 <- get_datagrid(dat.survival.unscaled, by = "Prev_year_precip",
                                   length = 50) %>% 
  arrange(Prev_year_precip)
viz.survival.precip$Prev_year_precip <- unscaled.precip50$Prev_year_precip

# Graph (insight version)
survival.precip <- dat.survival %>% 
  ggplot(aes(x = Prev_year_precip, y = survival_transf)) +
  geom_point() +
  geom_line(data = viz.survival.precip,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Previous year precip (mm)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. precip") 
survival.precip 


# Generate CI and add unscaled variable
ci.survival.precip <- predict_response(survival_best.model, terms = "Prev_year_precip_scaled")
unscaled.precip11 <- get_datagrid(dat.survival.unscaled, by = "Prev_year_precip",
                                  length = 11) %>% 
  arrange(Prev_year_precip)
ci.survival.precip$Prev_year_precip <- unscaled.precip11$Prev_year_precip
ci.survival.precip$survival_transf <- ci.survival.precip$predicted 

# Graph with CI (ggeffects version)
survival.precip.ci <- dat.survival %>% 
  ggplot(aes(x = Prev_year_precip, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = ci.survival.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.survival.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Previous year precip (mm)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. precip") 
survival.precip.ci


# Graph differences in prediction line (dashed is using insight, solid is from ggeffects)
dat.survival %>% 
  ggplot(aes(x = Prev_year_precip, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = ci.survival.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.survival.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.survival.precip,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) +
  labs(x = "Previous year precip (mm)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. precip") # insight just above ggeffects



## Survival: BG density ---------------------------------------------------

# Generate prediction and add scaled variable 
viz.survival.bgden <- get_datagrid(dat.survival.ex, by = c("BGDensity_scaled"),
                                    length = 50)
viz.survival.bgden$Predicted <- get_predicted(survival_best.model, viz.survival.bgden) # Predicting new random effect levels for terms: 1 | Transect:Site
unscaled.bgden50 <- get_datagrid(dat.survival.unscaled, by = "BGDensity",
                                  length = 50) %>% 
  arrange(BGDensity)
viz.survival.bgden$BGDensity <- unscaled.bgden50$BGDensity

# Graph (insight version)
survival.bgden <- dat.survival %>% 
  ggplot(aes(x = BGDensity, y = survival_transf)) +
  geom_point() +
  geom_line(data = viz.survival.bgden,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = expression(paste("Density (individuals / ", m^2, ")")),
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. plot density") 
survival.bgden


# Generate CI and add unscaled variable
ci.survival.bgden <- predict_response(survival_best.model, terms = "BGDensity_scaled")
unscaled.bgden9 <- get_datagrid(dat.survival.unscaled, by = "BGDensity",
                                 length = 9) %>% 
  arrange(BGDensity)
ci.survival.bgden$BGDensity <- unscaled.bgden9$BGDensity
ci.survival.bgden$survival_transf <- ci.survival.bgden$predicted 

# Graph with CI (ggeffects version)
survival.bgden.ci <- dat.survival %>% 
  ggplot(aes(x = BGDensity, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = ci.survival.bgden,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.survival.bgden,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = expression(paste("Density (individuals / ", m^2, ")")),
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. plot density") 
survival.bgden.ci


# Graph differences in prediction line (dashed is using insight, solid is from ggeffects)
dat.survival %>% 
  ggplot(aes(x = BGDensity, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = ci.survival.bgden,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.survival.bgden,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.survival.bgden,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = expression(paste("Density (individuals / ", m^2, ")")),
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. plot density") # basically the same




## Survival: Slope (NS) ---------------------------------------------------

# Generate prediction and add scaled variable (use model3)
viz.survival.slope <- get_datagrid(dat.survival.ex, by = c("PlotSlope_scaled"),
                                   length = 50)
viz.survival.slope$Predicted <- get_predicted(survival_model3, viz.survival.slope) # Predicting new random effect levels for terms: 1 | Transect:Site
unscaled.slope50 <- get_datagrid(dat.survival.unscaled, by = "PlotSlope",
                                 length = 50) %>% 
  arrange(PlotSlope)
viz.survival.slope$PlotSlope <- unscaled.slope50$PlotSlope

# Graph (insight version)
survival.slope <- dat.survival %>% 
  ggplot(aes(x = PlotSlope, y = survival_transf)) +
  geom_point() +
  geom_line(data = viz.survival.slope,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Plot slope (\u00B0)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. plot slope") 
survival.slope 


# Generate CI and add unscaled variable
ci.survival.slope <- predict_response(survival_model3, terms = "PlotSlope_scaled")
unscaled.slope24 <- get_datagrid(dat.survival.unscaled, by = "PlotSlope",
                                 length = 24) %>% 
  arrange(PlotSlope)
ci.survival.slope$PlotSlope <- unscaled.slope24$PlotSlope
ci.survival.slope$survival_transf <- ci.survival.slope$predicted 

# Graph with CI (ggeffects version)
survival.slope.ci <- dat.survival %>% 
  ggplot(aes(x = PlotSlope, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = ci.survival.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.survival.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Plot slope (\u00B0)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. plot slope") 
survival.slope.ci


# Graph differences in prediction line (dashed is using insight, solid is from ggeffects)
dat.survival %>% 
  ggplot(aes(x = PlotSlope, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = ci.survival.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.survival.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.survival.slope,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Plot slope (\u00B0)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. plot slope") # basically the same



## Survival: Shrub (NS) ---------------------------------------------------

# Generate prediction and add scaled variable 
viz.survival.shrub <- get_datagrid(dat.survival.ex, by = c("ShrubCover_scaled"),
                                   length = 50)
viz.survival.shrub$Predicted <- get_predicted(survival_best.model, viz.survival.shrub) # Predicting new random effect levels for terms: 1 | Transect:Site
unscaled.shrub50 <- get_datagrid(dat.survival.unscaled, by = "ShrubCover",
                                 length = 50) %>% 
  arrange(ShrubCover)
viz.survival.shrub$ShrubCover <- unscaled.shrub50$ShrubCover

# Graph (insight version)
survival.shrub <- dat.survival %>% 
  ggplot(aes(x = ShrubCover, y = survival_transf)) +
  geom_point() +
  geom_line(data = viz.survival.shrub,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Native shrub cover (%)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. shrub cover") 
survival.shrub 


# Generate CI and add unscaled variable
ci.survival.shrub <- predict_response(survival_best.model, terms = "ShrubCover_scaled")
unscaled.shrub11 <- get_datagrid(dat.survival.unscaled, by = "ShrubCover",
                                 length = 11) %>% 
  arrange(ShrubCover)
ci.survival.shrub$ShrubCover <- unscaled.shrub11$ShrubCover
ci.survival.shrub$survival_transf <- ci.survival.shrub$predicted 

# Graph with CI (ggeffects version)
survival.shrub.ci <- dat.survival %>% 
  ggplot(aes(x = ShrubCover, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = ci.survival.shrub,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.survival.shrub,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Native shrub cover (%)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. shrub") 
survival.shrub.ci


# Graph differences in prediction line (dashed is using insight, solid is from ggeffects)
dat.survival %>% 
  ggplot(aes(x = ShrubCover, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = ci.survival.shrub,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.survival.shrub,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.survival.shrub,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Native shrub cover (%)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. shrub cover") # basically the same



## Survival: Herb (NS) ----------------------------------------------------

# Generate prediction and add scaled variable 
viz.survival.herb <- get_datagrid(dat.survival.ex, by = c("HerbCover_scaled"),
                                  length = 50)
viz.survival.herb$Predicted <- get_predicted(survival_best.model, viz.survival.herb) # Predicting new random effect levels for terms: 1 | Transect:Site
unscaled.herb50 <- get_datagrid(dat.survival.unscaled, by = "HerbCover",
                                length = 50) %>% 
  arrange(HerbCover)
viz.survival.herb$HerbCover <- unscaled.herb50$HerbCover

# Graph (insight version)
survival.herb <- dat.survival %>% 
  ggplot(aes(x =HerbCover, y = survival_transf)) +
  geom_point() +
  geom_line(data = viz.survival.herb,
            aes(y = Predicted), linewidth = 1.5,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Native forb & grass cover (%)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. herb cover") 
survival.herb 


# Generate CI and add unscaled variable
ci.survival.herb <- predict_response(survival_best.model, terms = "HerbCover_scaled")
unscaled.herb24 <- get_datagrid(dat.survival.unscaled, by = "HerbCover",
                                length = 24) %>% 
  arrange(HerbCover)
ci.survival.herb$HerbCover <- unscaled.herb24$HerbCover
ci.survival.herb$survival_transf <- ci.survival.herb$predicted 

# Graph with CI (ggeffects version)
survival.herb.ci <- dat.survival %>% 
  ggplot(aes(x = HerbCover, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = ci.survival.herb,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.survival.herb,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Native forb & grass cover (%)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. herb cover") 
survival.herb.ci


# Graph differences in prediction line (dashed is using insight, solid is from ggeffects)
dat.survival %>% 
  ggplot(aes(x = HerbCover, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = ci.survival.herb,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = ci.survival.herb,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = viz.survival.herb,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Native forb & grass cover (%)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. herb cover") # basically the same



## Survival: Precip * density (NS) ----------------------------------------

# Generate prediction and add unscaled variable
viz.survival.bgden.precip <- dat.survival.ex %>% 
  get_datagrid(c("BGDensity_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(BGDensity_scaled)
viz.survival.bgden.precip$Predicted <- get_predicted(survival_best.model, viz.survival.bgden.precip) # Warning: Predicting new random effect levels for terms: 1 | Transect:Site
unscaled.bgden.precip243 <- dat.survival.unscaled %>% 
  get_datagrid(c("BGDensity", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(BGDensity)
viz.survival.bgden.precip$BGDensity <- unscaled.bgden.precip243$BGDensity
viz.survival.bgden.precip$Prev_year_precip <- unscaled.bgden.precip243$Prev_year_precip

# Graph (insight version)
survival.bgden.precip <- dat.survival %>% 
  ggplot(aes(x = BGDensity, y = survival_transf,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = viz.survival.bgden.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  labs(y = "Seedling survival (%)",
       x = expression(paste("Density (individuals / ", m^2, ")")),
       title = "Buffelgrass seedling survival vs. density") +
  scale_y_continuous(labels = scales::percent)
survival.bgden.precip # lines are kind of weird and not smooth


# Generate CI with scaled x variable
survival.pred.bgden.precip <- predict_response(survival_best.model, 
                                               terms = c("BGDensity_scaled", "Prev_year_precip_scaled"))
survival.pred.bgden.precip$group <- factor(survival.pred.bgden.precip$group,
                                           levels = c("1.07", "0.06", "-0.95"))
survival.pred.bgden.precip <- as.data.frame(survival.pred.bgden.precip)

# Graph with CI, scaled prediction only (ggeffects version)  
survival.bgden.precip.ci <- survival.pred.bgden.precip %>% 
  ggplot(aes(x, predicted, group = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3) +
  geom_line(aes(color = group),
            linewidth = 1.3) +
  theme_bw() +
  scale_color_manual(values = c("#440154FF", "#1F968BFF", "#FDE725FF")) +
  scale_fill_manual(values = c("#440154FF", "#1F968BFF", "#FDE725FF")) +
  labs(y = "Seedling survival (%)",
       x = "Density (scaled)",
       title = "Buffelgrass seedling survival vs. density (scaled)",
       color = "Previous year \nprecip (scaled)",
       fill = "Previous year \nprecip (scaled)") +
  scale_y_continuous(labels = scales::percent) 
survival.bgden.precip.ci # insight higher than ggeffects



## Survival: Precip * shrub (NS) ------------------------------------------

# Generate prediction and add unscaled variable
viz.survival.shrub.precip <- dat.survival.ex %>% 
  get_datagrid(c("ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(ShrubCover_scaled)
viz.survival.shrub.precip$Predicted <- get_predicted(survival_best.model, viz.survival.shrub.precip) # Warning: Predicting new random effect levels for terms: 1 | Transect:Site
unscaled.shrub.precip243 <- dat.survival.unscaled %>% 
  get_datagrid(c("ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(ShrubCover)
viz.survival.shrub.precip$ShrubCover <- unscaled.shrub.precip243$ShrubCover
viz.survival.shrub.precip$Prev_year_precip <- unscaled.shrub.precip243$Prev_year_precip

# Graph (insight version)
survival.shrub.precip <- dat.survival %>% 
  ggplot(aes(x = ShrubCover, y = survival_transf,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = viz.survival.shrub.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  labs(y = "Seedling survival (%)",
       x = "Native shrub cover (%)",
       title = "Buffelgrass seedling survival vs. shrub cover") +
  scale_y_continuous(labels = scales::percent)
survival.shrub.precip # lol all the lines are just flat


# Generate CI with scaled x variable
survival.pred.shrub.precip <- predict_response(survival_best.model, 
                                               terms = c("ShrubCover_scaled", "Prev_year_precip_scaled"))
survival.pred.shrub.precip$group <- factor(survival.pred.shrub.precip$group,
                                           levels = c("1.07", "0.06", "-0.95"))
survival.pred.shrub.precip <- as.data.frame(survival.pred.shrub.precip)

# Graph with CI, scaled prediction only (ggeffects version)  
survival.shrub.precip.ci <- survival.pred.shrub.precip %>% 
  ggplot(aes(x, predicted, group = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3) +
  geom_line(aes(color = group),
            linewidth = 1.3) +
  theme_bw() +
  scale_color_manual(values = c("#440154FF", "#1F968BFF", "#FDE725FF")) +
  scale_fill_manual(values = c("#440154FF", "#1F968BFF", "#FDE725FF")) +
  labs(y = "Seedling survival (%)",
       x = "Native shrub cover (scaled)",
       title = "Buffelgrass seedling survival vs. shrub cover (scaled)",
       color = "Previous year \nprecip (scaled)",
       fill = "Previous year \nprecip (scaled)") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) 
survival.shrub.precip.ci # wettest CI goes above 100%; insight version much higher than ggeffects



# Survival: Precip * herb (NS) --------------------------------------------

# Generate prediction and add unscaled variable
viz.survival.herb.precip <- dat.survival.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(HerbCover_scaled)
viz.survival.herb.precip$Predicted <- get_predicted(survival_best.model, viz.survival.herb.precip) # Warning: Predicting new random effect levels for terms: 1 | Transect:Site
unscaled.herb.precip243 <- dat.survival.unscaled %>% 
  get_datagrid(c("HerbCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(HerbCover)
viz.survival.herb.precip$HerbCover <- unscaled.herb.precip243$HerbCover
viz.survival.herb.precip$Prev_year_precip <- unscaled.herb.precip243$Prev_year_precip

# Graph (insight version)
survival.herb.precip <- dat.survival %>% 
  ggplot(aes(x = HerbCover, y = survival_transf,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = viz.survival.herb.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  labs(y = "Seedling survival (%)",
       x = "Native grass & forb cover (%)",
       title = "Buffelgrass seedling survival vs. herb cover") +
  scale_y_continuous(labels = scales::percent)
survival.herb.precip 


# Generate CI with scaled x variable
survival.pred.herb.precip <- predict_response(survival_best.model, 
                                              terms = c("HerbCover_scaled", "Prev_year_precip_scaled"))
survival.pred.herb.precip$group <- factor(survival.pred.herb.precip$group,
                                          levels = c("1.07", "0.06", "-0.95"))
survival.pred.herb.precip <- as.data.frame(survival.pred.herb.precip)

# Graph with CI, scaled prediction only (ggeffects version)  
survival.herb.precip.ci <- survival.pred.herb.precip %>% 
  ggplot(aes(x, predicted, group = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3) +
  geom_line(aes(color = group),
            linewidth = 1.3) +
  theme_bw() +
  scale_color_manual(values = c("#440154FF", "#1F968BFF", "#FDE725FF")) +
  scale_fill_manual(values = c("#440154FF", "#1F968BFF", "#FDE725FF")) +
  labs(y = "Seedling survival (%)",
       x = "Native grass & forb cover (scaled)",
       title = "Buffelgrass seedling survival vs. herb cover (scaled)",
       color = "Previous year \nprecip (scaled)",
       fill = "Previous year \nprecip (scaled)") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 1)) 
survival.herb.precip.ci # insight version much higher than ggeffects




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
     units = "in", height = 4, width = 6, res = 150)
total.change.aspect
dev.off()
# Total change by Aspect (prediction only)
tiff("figures/2025-07_draft-figures-2.0/Total-change-aspect-predictions_CI.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.pred.aspect
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
# Total change vs. PlotSlope (with CI)
tiff("figures/2025-07_draft-figures-2.0/Total-change_prediction_plot-slope_CI.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.slope.ci
dev.off()

# Total change vs. Change_HerbCover 
tiff("figures/2025-07_draft-figures-2.0/Total-change_prediction_herb-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.herb
dev.off()
# Total change vs. Change_HerbCover (with CI)
tiff("figures/2025-07_draft-figures-2.0/Total-change_prediction_herb-cover-change_CI.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.herb.ci
dev.off()

# Total change interaction of precip*BG density
tiff("figures/2025-07_draft-figures-2.0/Total-change_prediction_BG-density-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.bgden.precip
dev.off()
# Total change interaction of precip*BG density (with CI, scaled)
tiff("figures/2025-07_draft-figures-2.0/Total-change_prediction_BG-density-change-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.bgden.precip.ci
dev.off()



## Repro change -----------------------------------------------------------

# Significant
# Repro change by Aspect (original data only)
tiff("figures/2025-07_draft-figures-2.0/Repro-change-by-aspect.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.change.aspect
dev.off()
# Repro change by Aspect (prediction only)
tiff("figures/2025-07_draft-figures-2.0/Repro-change-aspect-predictions_CI.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.pred.aspect
dev.off()

# Repro change vs. Change_BGDensity
tiff("figures/2025-07_draft-figures-2.0/Repro-change_prediction_BG-density-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.bgden
dev.off()
# Repro change vs. Change_BGDensity (with CI)
tiff("figures/2025-07_draft-figures-2.0/Repro-change_prediction_BG-density-change_CI.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.bgden.ci
dev.off()

# Repro change vs. Change_ShrubCover
tiff("figures/2025-07_draft-figures-2.0/Repro-change_prediction_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.shrub
dev.off()
# Repro change vs. Change_ShrubCover (with CI)
tiff("figures/2025-07_draft-figures-2.0/Repro-change_prediction_shrub-cover-change_CI.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.shrub.ci
dev.off()

# Repro change vs. Change_HerbCover
tiff("figures/2025-07_draft-figures-2.0/Repro-change_prediction_herb-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.herb
dev.off()
# Repro change vs. Change_HerbCover (with CI)
tiff("figures/2025-07_draft-figures-2.0/Repro-change_prediction_herb-cover-change_CI.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.herb.ci
dev.off()


# Not significant
# Repro change vs. Prev_year_precip
tiff("figures/2025-07_draft-figures-2.0/Repro-change_prediction_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.precip
dev.off()
# Repro change vs. Prev_year_precip (with CI)
tiff("figures/2025-07_draft-figures-2.0/Repro-change_prediction_prev-year-precip_CI.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.precip.ci
dev.off()

# Repro change vs. PlotSlope
tiff("figures/2025-07_draft-figures-2.0/Repro-change_prediction_plot-slope.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.slope
dev.off()
# Repro change vs. PlotSlope (with CI)
tiff("figures/2025-07_draft-figures-2.0/Repro-change_prediction_plot-slope_CI.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.slope.ci
dev.off()

# Repro change interaction of precip*BG density
tiff("figures/2025-07_draft-figures-2.0/Repro-change_prediction_BG-density-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.bgden.precip
dev.off()
# Repro change interaction of precip*BG density (with CI, scaled)
tiff("figures/2025-07_draft-figures-2.0/Repro-change_prediction_BG-density-change-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.bgden.precip.ci
dev.off()

# Repro change interaction of precip*shrub
tiff("figures/2025-07_draft-figures-2.0/Repro-change_prediction_shrub-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.shrub.precip
dev.off()
# Repro change interaction of precip*shrub (with CI, scaled)
tiff("figures/2025-07_draft-figures-2.0/Repro-change_prediction_shrub-cover-change-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.shrub.precip.ci
dev.off()

# Repro change interaction of precip*herb
tiff("figures/2025-07_draft-figures-2.0/Repro-change_prediction_herb-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.herb.precip
dev.off()
# Repro change interaction of precip*herb (with CI, scaled)
tiff("figures/2025-07_draft-figures-2.0/Repro-change_prediction_herb-cover-change-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.herb.precip.ci
dev.off()



## BG density change ------------------------------------------------------

# Significant
# BG density change vs. Prev_year_precip
tiff("figures/2025-07_draft-figures-2.0/BG-density-change_prediction_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.precip
dev.off()

# BG density change by Aspect (original data only)
tiff("figures/2025-07_draft-figures-2.0/BG-density-change-by-aspect.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.change.aspect
dev.off()
# BG density change by Aspect (prediction only)
tiff("figures/2025-07_draft-figures-2.0/BG-density-change-aspect-predictions_CI.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.pred.aspect
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
# BG density change interaction of precip*shrub (with CI, scaled)
tiff("figures/2025-07_draft-figures-2.0/BG-density-change_prediction_shrub-cover-change-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.shrub.precip.ci
dev.off()
# BG density change interaction of precip*shrub (long)
tiff("figures/2025-07_draft-figures-2.0/BG-density-change_prediction_shrub-cover-change-and-precip-interaction_long.tiff",
     units = "in", height = 7, width = 6, res = 150)
bgden.shrub.precip
dev.off()


# Not significant
# BG density change vs. PlotSlope
tiff("figures/2025-07_draft-figures-2.0/BG-density-change_prediction_plot-slope.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.slope
dev.off()



## BG cover change --------------------------------------------------------

# Significant
# BG cover change vs. Prev_year_precip
tiff("figures/2025-07_draft-figures-2.0/BG-cover-change_prediction_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.precip
dev.off()

# BG cover change by Aspect (original data only)
tiff("figures/2025-07_draft-figures-2.0/BG-cover-change-by-aspect.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.change.aspect
dev.off()
# BG cover change by Aspect (prediction only)
tiff("figures/2025-07_draft-figures-2.0/BG-cover-change-aspect-predictions_CI.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.pred.aspect
dev.off()

# BG cover change vs. Change_ShrubCover
tiff("figures/2025-07_draft-figures-2.0/BG-cover-change_prediction_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.shrub
dev.off()


# Not significant
# BG cover change vs. PlotSlope
tiff("figures/2025-07_draft-figures-2.0/BG-cover-change_prediction_plot-slope.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.slope
dev.off()

# BG cover change vs. Change_HerbCover
tiff("figures/2025-07_draft-figures-2.0/BG-cover-change_prediction_herb-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.herb
dev.off()

# BG cover change interaction of precip*shrub
tiff("figures/2025-07_draft-figures-2.0/BG-cover-change_prediction_shrub-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.shrub.precip
dev.off()
# BG cover change interaction of precip*shrub (with CI, scaled)
tiff("figures/2025-07_draft-figures-2.0/BG-cover-change_prediction_shrub-cover-change-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.shrub.precip.ci
dev.off()

# BG cover change interaction of precip*herb
tiff("figures/2025-07_draft-figures-2.0/BG-cover-change_prediction_herb-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.herb.precip
dev.off()
# BG cover change interaction of precip*herb (with CI, scaled)
tiff("figures/2025-07_draft-figures-2.0/BG-cover-change_prediction_herb-cover-change-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.herb.precip.ci
dev.off()



## Survival ---------------------------------------------------------------

# Significant
# Survival vs. Prev_year_precip
tiff("figures/2025-07_draft-figures-2.0/Survival_prediction_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.precip
dev.off()
# Survival vs. Prev_year_precip (with CI)
tiff("figures/2025-07_draft-figures-2.0/Survival_prediction_prev-year-precip_CI.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.precip.ci
dev.off()

# Survival vs. BGDensity
tiff("figures/2025-07_draft-figures-2.0/Survival_prediction_BG-density.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.bgden
dev.off()
# Survival vs. BGDensity (with CI)
tiff("figures/2025-07_draft-figures-2.0/Survival_prediction_BG-density_CI.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.bgden.ci
dev.off()


# Not significant
# Survival vs. PlotSlope
tiff("figures/2025-07_draft-figures-2.0/Survival_prediction_plot-slope.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.slope
dev.off()
# Survival vs. PlotSlope (with CI)
tiff("figures/2025-07_draft-figures-2.0/Survival_prediction_plot-slope_CI.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.slope.ci
dev.off()

# Survival vs. ShrubCover
tiff("figures/2025-07_draft-figures-2.0/Survival_prediction_shrub-cover.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.shrub
dev.off()
# Survival vs. ShrubCover (with CI)
tiff("figures/2025-07_draft-figures-2.0/Survival_prediction_shrub-cover_CI.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.shrub.ci
dev.off()

# Survival vs. HerbCover
tiff("figures/2025-07_draft-figures-2.0/Survival_prediction_herb-cover.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.herb
dev.off()
# Survival vs. HerbCover (with CI)
tiff("figures/2025-07_draft-figures-2.0/Survival_prediction_herb-cover_CI.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.herb.ci
dev.off()

# Survival interaction of precip*density
tiff("figures/2025-07_draft-figures-2.0/Survival_prediction_BG-density-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.bgden.precip
dev.off()
# Survival interaction of precip*density (with CI, scaled)
tiff("figures/2025-07_draft-figures-2.0/Survival_prediction_BG-density-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.bgden.precip.ci
dev.off()

# Survival interaction of precip*shrub
tiff("figures/2025-07_draft-figures-2.0/Survival_prediction_shrub-cover-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.shrub.precip
dev.off()
# Survival interaction of precip*shrub (with CI, scaled)
tiff("figures/2025-07_draft-figures-2.0/Survival_prediction_shrub-cover-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.shrub.precip.ci
dev.off()

# Survival interaction of precip*herb
tiff("figures/2025-07_draft-figures-2.0/Survival_prediction_herb-cover-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.herb.precip
dev.off()
# Survival interaction of precip*herb (with CI, scaled)
tiff("figures/2025-07_draft-figures-2.0/Survival_prediction_herb-cover-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.herb.precip.ci
dev.off()



## Precip plot ------------------------------------------------------------

# Combined precip plot for density, cover, survival
tiff("figures/2025-07_draft-figures-2.0/Precip-combined_density-cover-survival.tiff",
     units = "in", height = 7, width = 9, res = 150)
ggarrange(bgden.precip, bgcov.precip, survival.precip,
          ncol = 2, nrow = 2,
          labels = c("(A)", "(B)", "(C)"))
dev.off()


save.image("RData/05.4_draft-figs-2.0-for-lm-5.1.RData")
