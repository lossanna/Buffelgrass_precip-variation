# Created: 2025-08-01
# Updated: 2025-08-05

# Purpose: Create graphs for linear models v6.3. Overlay model predictions on top of original data
#   using ggeffects and insight packages.

# insight: https://easystats.github.io/modelbased/articles/visualisation_matrix.html#visualising-an-interaction-between-two-numeric-variables-three-way-interaction
# ggeffects: https://strengejacke.github.io/ggeffects/

# Note: get_predicted() from insight and predict_response() from ggeffects do not work with model averaged 
#   coefficients; needs actual model object.
# For simplicity, I will use only the best model in generating model-predicted lines, as the lines
#   are virtually the same for all top models (see comparison from 05.4_draft-figs-2.0-for-lm-5.1.R).

# To graph back-transformed/unscaled continuous explanatory variables with model prediction (insight version):
#   1. Make datagrid of with prediction and CI with scaled variable using get_predicted() and get_predicted_CI().
#        Name the prediction column the same as the y-axis of the graph (response variable).
#   2. Make datagrid of unscaled variable that is the same length as the datagrid with predictions & gg.
#        Arrange this datagrid with unscaled variable by that variable so the order will be the same as first datagrid. 
#   3. Add the unscaled variable column to datagrid with predictions & gg.

# To graph original data with model prediction (ggeffects version):
#   1. Generate table with prediction and CI based on scaled variable using predict_response(). Unlike insight version, 
#         you cannot specify the length of the datagrid. Name the prediction column the same as the y-axis of the 
#         graph (response variable).
#   2. Make a datagrid of unscaled variable that is the same length as the table with prediction & gg.
#        Arrange this datagrid with unscaled variable by that variable so the order will be the table with predictions & gg. 
#   3. Add the unscaled variable column to the table with predictions & gg.

# Note: I don't know how to do this for interaction graphs, so those don't get graphs with original data
#       and overlaid gg. 
#   get_predicted_ci() does not work because there is only one level for Aspect so it can't calculate SE, but
#       Aspect must be included because it is included in the linear model.
#   ggeffects version doesn't work because I can't figure out how to make an equivalent datagrid for unscaled variable.

# Note: I don't know how to graph the Aspect boxplots using insight, so they will have a ggeffects version only
#   (those graphs aren't very interesting and so it's not worth trying to figure).


library(tidyverse)
library(insight)
library(ggeffects)
library(viridis)
library(ggpubr)

# Load data ---------------------------------------------------------------

load("RData/06.8_data-and-best-models-6.3.RData")
dat.survival.raw <- dat.survival


# Data wrangling ----------------------------------------------------------

# Need 3 datasets:
#   1. Data for graphing observed data
#   2. Data to construct datagrid with predictions & CI
#   3. Data to construct datagrid with unscaled explanatory variable (to be added to #2)


# 1. Dataset for graphing (response and unscaled explanatory variables)
#   Culm change - total & repro
dat.culm <- culm.change.flat.rm %>% 
  select(Change_Total_Live_Culms, Change_Reproductive_culms, 
         Prev_year_precip, Aspect, PlotSlope, Change_ShrubCover, Change_HerbCover,
         Change_BGDensity)

#   Plot change - density & cover
dat.plot <- plot.change %>% 
  select(Change_BGDensity, Change_BGCover, 
         Prev_year_precip, Aspect, PlotSlope, Change_ShrubCover, Change_HerbCover)

#   Survival  
dat.survival <- dat.survival.raw %>% 
  select(survival_perc, 
         Prev_year_precip, Aspect, BGDensity, PlotSlope, ShrubCover, HerbCover)


# 2. Dataset for constructing datagrid with prediction & CI (scaled explanatory variables only)
#   Culm change - total & repro 
dat.culm.ex <- culm.change.flat.rm %>% 
  select(Prev_year_precip_scaled, Aspect, PlotSlope_scaled, 
         Change_BGDensity_scaled, Change_ShrubCover_scaled, Change_HerbCover_scaled)

#   Plot change - density & cover
dat.plot.ex <- plot.change %>% 
  select(Prev_year_precip_scaled,
         Aspect, PlotSlope_scaled, Change_ShrubCover_scaled)

#   Survival  
dat.survival.ex <- dat.survival.raw %>% 
  select(Prev_year_precip_scaled,
         PlotSlope_scaled, ShrubCover_scaled, HerbCover_scaled,
         BGDensity_scaled)


# 3. Dataset for constructing datagrid with unscaled variables to match graph (unscaled explanatory variables only)
#   Culm change - total & repro
dat.culm.unscaled <- culm.change.flat.rm %>% 
  select(Prev_year_precip, Aspect, PlotSlope, Change_BGDensity, Change_ShrubCover, Change_HerbCover)

#   Plot change - density & cover
dat.plot.unscaled <- plot.change %>% 
  select(Prev_year_precip, Aspect, PlotSlope, Change_ShrubCover, Change_HerbCover)

#   Survival 
dat.survival.unscaled <- dat.survival.raw %>% 
  select(Prev_year_precip, PlotSlope, BGDensity, ShrubCover, HerbCover)



# Total culm change -------------------------------------------------------

# Top model includes all variables (including interactions).


## Total: Precip ----------------------------------------------------------

# insight version
#   Construct datagrid with scaled variable 
insight.total.precip <- get_datagrid(dat.culm.ex, by = c("Prev_year_precip_scaled"),
                                 length = 100)
#   Add prediction column (must be must be named the same as y-axis in graph)
insight.total.precip$Change_Total_Live_Culms <- get_predicted(total_best.model, insight.total.precip) 
#   Add SE and CI columns
insight.total.precip$SE <- get_predicted_ci(total_best.model, data = insight.total.precip)$SE
insight.total.precip$CI <- insight.total.precip$SE * 1.96
#   Construct datagrid of same length with unscaled variable
unscaled.precip100 <- get_datagrid(dat.culm.unscaled, by = "Prev_year_precip",
                                   length = 100) %>% 
  arrange(Prev_year_precip)
#   Add unscaled variable to datagrid with prediction & CI
insight.total.precip$Prev_year_precip <- unscaled.precip100$Prev_year_precip

# Graph (insight version)
total.precip <- dat.culm %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_line(data = insight.total.precip,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.total.precip,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI), 
              alpha = 0.2) +
  theme_bw() +
  xlab("Previous year precip (mm)") +
  ggtitle("Change in total culm count vs. precip") +
  labs(y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
total.precip


# ggeffects version
#   Generate table with predictions & CI
gg.total.precip <- predict_response(total_best.model, terms = "Prev_year_precip_scaled")
#   Create datagrid of same length with unscaled variable
unscaled.precip15 <- get_datagrid(dat.culm.unscaled, by = "Prev_year_precip",
                                  length = 15) %>% 
  arrange(Prev_year_precip)
#   Add unscaled variable to table with prediction & CI
gg.total.precip$Prev_year_precip <- unscaled.precip15$Prev_year_precip
gg.total.precip$Change_Total_Live_Culms <- gg.total.precip$predicted # must be named the same as y-axis in graph

# Graph (ggeffects version)
total.precip.gg <- dat.culm %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_ribbon(data = gg.total.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.total.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  xlab("Previous year precip (mm)") +
  ggtitle("Change in total culm count vs. precip") +
  labs(y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
total.precip.gg


# Graph differences in prediction line (dashed = insight, solid is from ggeffects)
dat.culm %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_ribbon(data = gg.total.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_ribbon(data = insight.total.precip,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI), 
              alpha = 0.2) +
  geom_line(data = gg.total.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.total.precip,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  xlab("Previous year precip (mm)") +
  ggtitle("Change in total culm count vs. precip") +
  labs(y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") # insight just slightly higher (basically the same)



## Total: BG density ------------------------------------------------------

# insight version
#   Generate prediction and add unscaled variable
insight.total.bgden <- get_datagrid(dat.culm.ex, by = c("Change_BGDensity_scaled"),
                                length = 100)
insight.total.bgden$Change_Total_Live_Culms <- get_predicted(total_best.model, insight.total.bgden)
insight.total.bgden$SE <- get_predicted_ci(total_best.model, data = insight.total.bgden)$SE
insight.total.bgden$CI <- insight.total.bgden$SE * 1.96
unscaled.bgden100 <- get_datagrid(dat.culm.unscaled, by = "Change_BGDensity",
                                  length = 100) %>% 
  arrange(Change_BGDensity)
insight.total.bgden$Change_BGDensity <- unscaled.bgden100$Change_BGDensity

#   Graph (insight version)
total.bgden <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_line(data = insight.total.bgden,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.total.bgden,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI),
              alpha = 0.2) +
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


# ggeffects version
#   Generate CI and add unscaled variable
gg.total.bgden <- predict_response(total_best.model, terms = "Change_BGDensity_scaled")
unscaled.bgden9 <- get_datagrid(dat.culm.unscaled, by = "Change_BGDensity",
                                length = 9) %>% 
  arrange(Change_BGDensity)
gg.total.bgden$Change_BGDensity <- unscaled.bgden9$Change_BGDensity
gg.total.bgden$Change_Total_Live_Culms <- gg.total.bgden$predicted

#   Graph (ggeffects version)
total.bgden.gg <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_ribbon(data = gg.total.bgden,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.total.bgden,
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
total.bgden.gg


# Graph differences in prediction line (dashed = insight, solid = ggeffects)
dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_ribbon(data = gg.total.bgden,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_ribbon(data = insight.total.bgden,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI),
              alpha = 0.2) +
  geom_line(data = gg.total.bgden,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.total.bgden,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
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

# insight version
#   Generate prediction with unscaled variable added
insight.total.shrub <- get_datagrid(dat.culm.ex, by = c("Change_ShrubCover_scaled"),
                                length = 100)
insight.total.shrub$Change_Total_Live_Culms <- get_predicted(total_best.model, insight.total.shrub)
insight.total.shrub$SE <- get_predicted_ci(total_best.model, data = insight.total.shrub)$SE
insight.total.shrub$CI <- insight.total.shrub$SE * 1.96
unscaled.shrub100 <- get_datagrid(dat.culm.unscaled, by = "Change_ShrubCover",
                                  length = 100) %>% 
  arrange(Change_ShrubCover)
insight.total.shrub$Change_ShrubCover <- unscaled.shrub100$Change_ShrubCover

#   Graph (insight version)
total.shrub <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_line(data = insight.total.shrub,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.total.shrub,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI),
              alpha = 0.2) +
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

# ggeffects version
#   Generate CI and add unscaled variable
gg.total.shrub <- predict_response(total_best.model, terms = "Change_ShrubCover_scaled")
unscaled.shrub16 <- get_datagrid(dat.culm.unscaled, by = "Change_ShrubCover",
                                 length = 16) %>% 
  arrange(Change_ShrubCover)
gg.total.shrub$Change_ShrubCover <- unscaled.shrub16$Change_ShrubCover
gg.total.shrub$Change_Total_Live_Culms <- gg.total.shrub$predicted

#   Graph (ggeffects version)
total.shrub.gg <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_ribbon(data = gg.total.shrub,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.total.shrub,
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
total.shrub.gg


# Graph differences in prediction line (dashed = insight, solid = ggeffects)
dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_ribbon(data = gg.total.shrub,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_ribbon(data = insight.total.shrub,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI),
              alpha = 0.2) +
  geom_line(data = gg.total.shrub,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.total.shrub,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
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
             color = "red") # insight just slightly higher (basically the same)



## Total: Precip * density ------------------------------------------------

# Generate prediction and add unscaled variable
insight.total.bgden.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_BGDensity_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_BGDensity_scaled)
insight.total.bgden.precip$Predicted <- get_predicted(total_best.model, insight.total.bgden.precip)
unscaled.bgden.precip243 <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_BGDensity", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_BGDensity)
insight.total.bgden.precip$Change_BGDensity <- unscaled.bgden.precip243$Change_BGDensity
insight.total.bgden.precip$Prev_year_precip <- unscaled.bgden.precip243$Prev_year_precip

# Graph, no CI (insight version)
total.bgden.precip <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Total_Live_Culms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.total.bgden.precip,
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
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ paste("Density (individuals / ", m^2, ")")),
       title = "Change in total culm count vs. plot density change")
total.bgden.precip


# Generate CI with scaled explanatory variable 
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



## Total: Precip * shrub --------------------------------------------------

# Generate prediction and add unscaled variable
insight.total.shrub.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled)
insight.total.shrub.precip$Predicted <- get_predicted(total_best.model, insight.total.shrub.precip)
unscaled.shrub.precip243 <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover)
insight.total.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip243$Change_ShrubCover
insight.total.shrub.precip$Prev_year_precip <- unscaled.shrub.precip243$Prev_year_precip

# Graph, no CI (insight version)
total.shrub.precip <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Total_Live_Culms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.total.shrub.precip,
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
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in total culm count vs. shrub cover change")
total.shrub.precip


# Generate CI with scaled explanatory variable
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
insight.total.herb.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
insight.total.herb.precip$Predicted <- get_predicted(total_best.model, insight.total.herb.precip)
unscaled.herb.precip243 <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover)
insight.total.herb.precip$Change_HerbCover <- unscaled.herb.precip243$Change_HerbCover
insight.total.herb.precip$Prev_year_precip <- unscaled.herb.precip243$Prev_year_precip

# Graph, no CI (insight version)
total.herb.precip <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Total_Live_Culms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.total.herb.precip,
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
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in total culm count vs. herb cover change")
total.herb.precip


# Generate CI with scaled explanatory variable 
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



## Total: Aspect (NS) -----------------------------------------------------

# ggeffects version
#   Generate prediction & CI
gg.total.aspect <- predict_response(total_best.model, terms = "Aspect") %>% 
  rename(Aspect = x,
         Change_Total_Live_Culms = predicted)

# Graph (ggeffects version)
total.aspect.gg <- dat.culm %>% 
  ggplot(aes(x = Aspect, y = Change_Total_Live_Culms)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  geom_pointrange(data = gg.total.aspect,
                  aes(ymin = conf.low, ymax = conf.high),
                  color = "purple3",
                  linewidth = 1.3) +
  geom_point(data = gg.total.aspect,
             aes(x = Aspect, y = Change_Total_Live_Culms),
             color = "purple3",
             size = 3,
             shape = 15) +
  theme_bw() +
  labs(title = "Change in total culm count by aspect",
       y = expression(Delta ~ "Total culm count"),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
total.aspect.gg



## Total: Slope (NS) ------------------------------------------------------

# insight version
#   Generate prediction and add unscaled variable
insight.total.slope <- get_datagrid(dat.culm.ex, by = c("PlotSlope_scaled"),
                                length = 100)
insight.total.slope$Change_Total_Live_Culms <- get_predicted(total_best.model, insight.total.slope)
insight.total.slope$SE <- get_predicted_ci(total_best.model, data = insight.total.slope)$SE
insight.total.slope$CI <- insight.total.slope$SE * 1.96
unscaled.slope100 <- get_datagrid(dat.culm.unscaled, by = "PlotSlope",
                                  length = 100) %>% 
  arrange(PlotSlope)
insight.total.slope$PlotSlope <- unscaled.slope100$PlotSlope

#   Graph (insight version)
total.slope <- dat.culm %>% 
  ggplot(aes(x = PlotSlope, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_line(data = insight.total.slope,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.total.slope,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI),
              alpha = 0.2) +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in total culm count vs. slope") +
  labs(y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
total.slope


# ggeffects version
#   Generate CI and add unscaled variable
gg.total.slope <- predict_response(total_best.model, terms = "PlotSlope_scaled")
unscaled.slope12 <- get_datagrid(dat.culm.unscaled, by = "PlotSlope",
                                 length = 12) %>% 
  arrange(PlotSlope)
gg.total.slope$PlotSlope <- unscaled.slope12$PlotSlope
gg.total.slope$Change_Total_Live_Culms <- gg.total.slope$predicted

#   Graph (ggeffects version)
total.slope.gg <- dat.culm %>% 
  ggplot(aes(x = PlotSlope, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_ribbon(data = gg.total.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.total.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in total culm count vs. slope") +
  labs(y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
total.slope.gg


# Graph differences in prediction line (dashed = insight, solid = ggeffects)
dat.culm %>% 
  ggplot(aes(x = PlotSlope, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_ribbon(data = gg.total.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_ribbon(data = insight.total.slope,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI),
              alpha = 0.2) +
  geom_line(data = gg.total.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.total.slope,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in total culm count vs. slope") +
  labs(y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") # insight just slightly higher (basically the same)



## Total: Herb (NS) -------------------------------------------------------

# insight version
#   Generate prediction and add unscaled variable
insight.total.herb <- get_datagrid(dat.culm.ex, by = c("Change_HerbCover_scaled"),
                               length = 100)
insight.total.herb$Change_Total_Live_Culms <- get_predicted(total_best.model, insight.total.herb)
insight.total.herb$SE <- get_predicted_ci(total_best.model, data = insight.total.herb)$SE
insight.total.herb$CI = insight.total.herb$SE * 1.96
unscaled.herb100 <- get_datagrid(dat.culm.unscaled, by = "Change_HerbCover",
                                 length = 100) %>% 
  arrange(Change_HerbCover)
insight.total.herb$Change_HerbCover <- unscaled.herb100$Change_HerbCover

# Graph (insight version)
total.herb <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_line(data = insight.total.herb,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.total.herb,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI),
              alpha = 0.2) +
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
gg.total.herb <- predict_response(total_best.model, terms = "Change_HerbCover_scaled")
unscaled.herb8 <- get_datagrid(dat.culm.unscaled, by = "Change_HerbCover",
                               length = 8) %>% 
  arrange(Change_HerbCover)
gg.total.herb$Change_HerbCover <- unscaled.herb8$Change_HerbCover
gg.total.herb$Change_Total_Live_Culms <- gg.total.herb$predicted

# Graph (ggeffects version)
total.herb.gg <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_ribbon(data = gg.total.herb,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.total.herb,
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
total.herb.gg


# Graph differences in prediction line (dashed = insight, solid = ggeffects)
dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_ribbon(data = gg.total.herb,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_ribbon(data = insight.total.herb,
              aes(ymin = Change_Total_Live_Culms - CI, ymax = Change_Total_Live_Culms + CI),
              alpha = 0.2) +
  geom_line(data = gg.total.herb,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.total.herb,
            aes(y = Change_Total_Live_Culms), linewidth = 1,
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
             color = "red") # insight just slightly higher (basically the same)




# Reproductive culm change ------------------------------------------------

# Top model includes all terms and interactions except for precip*shrub.
# For that graph, model2 should be used instead, which does include the 
#   precip*shrub interaction.


## Repro: Aspect ----------------------------------------------------------

# ggeffects version
#   Generate prediction & CI
gg.repro.aspect <- predict_response(repro_best.model, terms = "Aspect") %>% 
  rename(Aspect = x,
         Change_Reproductive_culms = predicted)

# Graph (ggeffects version)
repro.aspect.gg <- dat.culm %>% 
  ggplot(aes(x = Aspect, y = Change_Reproductive_culms)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  geom_pointrange(data = gg.repro.aspect,
                  aes(ymin = conf.low, ymax = conf.high),
                  color = "purple3",
                  linewidth = 1.3) +
  geom_point(data = gg.repro.aspect,
             aes(x = Aspect, y = Change_Reproductive_culms),
             color = "purple3",
             size = 3,
             shape = 15) +
  theme_bw() +
  labs(title = "Change in repro culm count by aspect",
       y = expression(Delta ~ "Reproductive culm count"),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
repro.aspect.gg



## Repro: BG density -------------------------------------------------------

# insight version
#   Generate prediction and add unscaled variable
insight.repro.bgden <- get_datagrid(dat.culm.ex, by = c("Change_BGDensity_scaled"),
                                length = 100)
insight.repro.bgden$Change_Reproductive_culms <- get_predicted(repro_best.model, insight.repro.bgden)
insight.repro.bgden$SE <- get_predicted_ci(repro_best.model, data = insight.repro.bgden)$SE
insight.repro.bgden$CI <- insight.repro.bgden$SE * 1.96
unscaled.bgden100 <- get_datagrid(dat.culm.unscaled, by = "Change_BGDensity",
                                  length = 100) %>% 
  arrange(Change_BGDensity)
insight.repro.bgden$Change_BGDensity <- unscaled.bgden100$Change_BGDensity

#   Graph (insight version)
repro.bgden <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_line(data = insight.repro.bgden,
            aes(y = Change_Reproductive_culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.repro.bgden,
              aes(ymin = Change_Reproductive_culms - CI, ymax = Change_Reproductive_culms + CI),
              alpha = 0.2) +
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


# geffects version
#   Generate CI and add unscaled variable
gg.repro.bgden <- predict_response(repro_best.model, terms = "Change_BGDensity_scaled")
unscaled.bgden9 <- get_datagrid(dat.culm.unscaled, by = "Change_BGDensity",
                                length = 9) %>% 
  arrange(Change_BGDensity)
gg.repro.bgden$Change_BGDensity <- unscaled.bgden9$Change_BGDensity
gg.repro.bgden$Change_Reproductive_culms <- gg.repro.bgden$predicted

#   Graph (ggeffects version)
repro.bgden.gg <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_ribbon(data = gg.repro.bgden,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.repro.bgden,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  ggtitle("Change in repro culm count vs. plot density change") +
  labs(y = expression(Delta ~ "Repro culm count"),
       x = expression(Delta ~ paste("Density (individuals / ", m^2, ")"))) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
repro.bgden.gg


# Graph differences in prediction line (dashed = insight, solid = ggeffects)
dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_ribbon(data = gg.repro.bgden,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_ribbon(data = insight.repro.bgden,
              aes(ymin = Change_Reproductive_culms - CI, ymax = Change_Reproductive_culms + CI),
              alpha = 0.2) +
  geom_line(data = gg.repro.bgden,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.repro.bgden,
            aes(y = Change_Reproductive_culms), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  labs(y = expression(Delta ~ "Repro culm count"),
       x = expression(Delta ~ paste("Density (individuals / ", m^2, ")"))) +
  ggtitle("Change in repro culm count vs. plot density") +
  labs(y = expression(Delta ~ "Repro culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") # insight slightly below (mostly overlapping though)



## Repro: Shrub -----------------------------------------------------------

# insight version
#   Generate prediction with unscaled variable added
insight.repro.shrub <- get_datagrid(dat.culm.ex, by = c("Change_ShrubCover_scaled"),
                                length = 100)
insight.repro.shrub$Change_Reproductive_culms <- get_predicted(repro_best.model, insight.repro.shrub)
insight.repro.shrub$SE <- get_predicted_ci(repro_best.model, data = insight.repro.shrub)$SE
insight.repro.shrub$CI <- insight.repro.shrub$SE * 1.96
unscaled.shrub100 <- get_datagrid(dat.culm.unscaled, by = "Change_ShrubCover",
                                  length = 100) %>% 
  arrange(Change_ShrubCover)
insight.repro.shrub$Change_ShrubCover <- unscaled.shrub100$Change_ShrubCover

#   Graph (insight version)
repro.shrub <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_line(data = insight.repro.shrub,
            aes(y = Change_Reproductive_culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.repro.shrub,
              aes(ymin = Change_Reproductive_culms - CI, ymax = Change_Reproductive_culms + CI),
              alpha = 0.2) +
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


# ggeffects version
#   Generate CI and add unscaled variable
gg.repro.shrub <- predict_response(repro_best.model, terms = "Change_ShrubCover_scaled")
unscaled.shrub16 <- get_datagrid(dat.culm.unscaled, by = "Change_ShrubCover",
                                 length = 16) %>% 
  arrange(Change_ShrubCover)
gg.repro.shrub$Change_ShrubCover <- unscaled.shrub16$Change_ShrubCover
gg.repro.shrub$Change_Reproductive_culms <- gg.repro.shrub$predicted

#   Graph (ggeffects version)
repro.shrub.gg <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_ribbon(data = gg.repro.shrub,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.repro.shrub,
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
repro.shrub.gg


# Graph differences in prediction line (dashed = insight, solid = ggeffects)
dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_ribbon(data = gg.repro.shrub,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_ribbon(data = insight.repro.shrub,
              aes(ymin = Change_Reproductive_culms - CI, ymax = Change_Reproductive_culms + CI),
              alpha = 0.2) +
  geom_line(data = gg.repro.shrub,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.repro.shrub,
            aes(y = Change_Reproductive_culms), linewidth = 1,
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
             color = "red") # insight just slightly below (mostly overlapping though)



## Repro: Herb ------------------------------------------------------------

# insight version
#   Generate prediction and add unscaled variable
insight.repro.herb <- get_datagrid(dat.culm.ex, by = c("Change_HerbCover_scaled"),
                               length = 100)
insight.repro.herb$Change_Reproductive_culms <- get_predicted(repro_best.model, insight.repro.herb)
insight.repro.herb$SE <- get_predicted_ci(repro_best.model, data = insight.repro.herb)$SE
insight.repro.herb$CI <- insight.repro.herb$SE * 1.96
unscaled.herb100 <- get_datagrid(dat.culm.unscaled, by = "Change_HerbCover",
                                 length = 100) %>% 
  arrange(Change_HerbCover)
insight.repro.herb$Change_HerbCover <- unscaled.herb100$Change_HerbCover

# Graph (insight version)
repro.herb <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_line(data = insight.repro.herb,
            aes(y = Change_Reproductive_culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.repro.herb,
              aes(ymin = Change_Reproductive_culms - CI, ymax = Change_Reproductive_culms + CI),
              alpha = 0.2) +
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


# ggeffects version
#   Generate CI and add unscaled variable
gg.repro.herb <- predict_response(repro_best.model, terms = "Change_HerbCover_scaled")
unscaled.herb8 <- get_datagrid(dat.culm.unscaled, by = "Change_HerbCover",
                               length = 8) %>% 
  arrange(Change_HerbCover)
gg.repro.herb$Change_HerbCover <- unscaled.herb8$Change_HerbCover
gg.repro.herb$Change_Reproductive_culms <- gg.repro.herb$predicted

#   Graph (ggeffects version)
repro.herb.gg <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_ribbon(data = gg.repro.herb,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.repro.herb,
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
repro.herb.gg


# Graph differences in prediction line (dashed = insight, solid = ggeffects)
dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_ribbon(data = gg.repro.herb,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_ribbon(data = insight.repro.herb,
              aes(ymin = Change_Reproductive_culms - CI, ymax = Change_Reproductive_culms + CI),
              alpha = 0.2) +
  geom_line(data = gg.repro.herb,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.repro.herb,
            aes(y = Change_Reproductive_culms), linewidth = 1,
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
             color = "red") # basically the same



## Repro: Precip (NS) -----------------------------------------------------

# insight version
#   Generate prediction and add unscaled variable
insight.repro.precip <- get_datagrid(dat.culm.ex, by = c("Prev_year_precip_scaled"),
                                 length = 100)
insight.repro.precip$Change_Reproductive_culms <- get_predicted(repro_best.model, insight.repro.precip)
insight.repro.precip$SE <- get_predicted_ci(repro_best.model, data = insight.repro.precip)$SE
insight.repro.precip$CI <- insight.repro.precip$SE * 1.96
unscaled.precip100 <- get_datagrid(dat.culm.unscaled, by = "Prev_year_precip",
                                   length = 100) %>% 
  arrange(Prev_year_precip)
insight.repro.precip$Prev_year_precip <- unscaled.precip100$Prev_year_precip

#   Graph (insight version)
repro.precip <- dat.culm %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_line(data = insight.repro.precip,
            aes(y = Change_Reproductive_culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.repro.precip,
              aes(ymin = Change_Reproductive_culms - CI, ymax = Change_Reproductive_culms + CI),
              alpha = 0.2) +
  theme_bw() +
  xlab("Previous year precip (mm)") +
  ggtitle("Change in repro culm count vs. precip") +
  labs(y = expression(Delta ~ "Reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
repro.precip


# ggeffects version
#   Generate CI and add unscaled variable
gg.repro.precip <- predict_response(repro_best.model, terms = "Prev_year_precip_scaled")
unscaled.precip15 <- get_datagrid(dat.culm.unscaled, by = "Prev_year_precip",
                                  length = 15) %>% 
  arrange(Prev_year_precip)
gg.repro.precip$Prev_year_precip <- unscaled.precip15$Prev_year_precip
gg.repro.precip$Change_Reproductive_culms <- gg.repro.precip$predicted 

#   Graph (ggeffects version)
repro.precip.gg <- dat.culm %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_ribbon(data = gg.repro.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.repro.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  xlab("Previous year precip (mm)") +
  ggtitle("Change in repro culm count vs. precip") +
  labs(y = expression(Delta ~ "Reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
repro.precip.gg


# Graph differences in prediction line (dashed = insight, solid is from ggeffects)
dat.culm %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_ribbon(data = gg.repro.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_ribbon(data = insight.repro.precip,
              aes(ymin = Change_Reproductive_culms - CI, ymax = Change_Reproductive_culms + CI),
              alpha = 0.2) +
  geom_line(data = gg.repro.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.repro.precip,
            aes(y = Change_Reproductive_culms), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  xlab("Previous year precip (mm)") +
  ggtitle("Change in repro culm count vs. precip") +
  labs(y = expression(Delta ~ "Reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") # insight slightly lower (mostly overlapping though)



## Repro: Slope (NS) -----------------------------------------------------

# insight version
#   Generate prediction and add unscaled variable
insight.repro.slope <- get_datagrid(dat.culm.ex, by = c("PlotSlope_scaled"),
                                length = 100)
insight.repro.slope$Change_Reproductive_culms <- get_predicted(repro_best.model, insight.repro.slope)
insight.repro.slope$SE <- get_predicted_ci(repro_best.model, data = insight.repro.slope)$SE
insight.repro.slope$CI <- insight.repro.slope$SE * 1.96
unscaled.slope100 <- get_datagrid(dat.culm.unscaled, by = "PlotSlope",
                                  length = 100) %>% 
  arrange(PlotSlope)
insight.repro.slope$PlotSlope <- unscaled.slope100$PlotSlope

#   Graph (insight version)
repro.slope <- dat.culm %>% 
  ggplot(aes(x = PlotSlope, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_line(data = insight.repro.slope,
            aes(y = Change_Reproductive_culms), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.repro.slope,
              aes(ymin = Change_Reproductive_culms - CI, ymax = Change_Reproductive_culms + CI),
              alpha = 0.2) +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in repro culm count vs. slope") +
  labs(y = expression(Delta ~ "Reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
repro.slope


# ggeffects version
#   Generate CI and add unscaled variable
gg.repro.slope <- predict_response(repro_best.model, terms = "PlotSlope_scaled")
unscaled.slope12 <- get_datagrid(dat.culm.unscaled, by = "PlotSlope",
                                 length = 12) %>% 
  arrange(PlotSlope)
gg.repro.slope$PlotSlope <- unscaled.slope12$PlotSlope
gg.repro.slope$Change_Reproductive_culms <- gg.repro.slope$predicted

#   Graph (ggeffects version)
repro.slope.gg <- dat.culm %>% 
  ggplot(aes(x = PlotSlope, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_ribbon(data = gg.repro.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.repro.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in repro culm count vs. slope") +
  labs(y = expression(Delta ~ "Reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
repro.slope.gg


# Graph differences in prediction line (dashed = insight, solid = ggeffects)
dat.culm %>% 
  ggplot(aes(x = PlotSlope, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_ribbon(data = gg.repro.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_ribbon(data = insight.repro.slope,
              aes(ymin = Change_Reproductive_culms - CI, ymax = Change_Reproductive_culms + CI),
              alpha = 0.2) +
  geom_line(data = gg.repro.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.repro.slope,
            aes(y = Change_Reproductive_culms), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in repro culm count vs. slope") +
  labs(y = expression(Delta ~ "Reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") # insight just slightly below (mostly overlapping though)



## Repro: Precip * density (NS) -------------------------------------------

# Generate prediction and add unscaled variable
insight.repro.bgden.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_BGDensity_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_BGDensity_scaled)
insight.repro.bgden.precip$Predicted <- get_predicted(repro_best.model, insight.repro.bgden.precip)
unscaled.bgden.precip243 <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_BGDensity", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_BGDensity)
insight.repro.bgden.precip$Change_BGDensity <- unscaled.bgden.precip243$Change_BGDensity
insight.repro.bgden.precip$Prev_year_precip <- unscaled.bgden.precip243$Prev_year_precip

# Graph, no CI (insight version)
repro.bgden.precip <- dat.culm %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Reproductive_culms,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.repro.bgden.precip,
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
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ paste("Density (individuals / ", m^2, ")")),
       title = "Change in repro culm count vs. plot density change") +
  scale_y_continuous(limits = c(-88, 120))
repro.bgden.precip


# Generate CI with scaled explanatory variable 
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
  scale_y_continuous(limits = c(-88, 120))
repro.bgden.precip.ci



## Repro: Precip * shrub (NS) ---------------------------------------------

# Generate prediction and add unscaled variable (use model 3, which includes precip*shrub interaction)
insight.repro.shrub.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled)
insight.repro.shrub.precip$Predicted <- get_predicted(repro_model3, insight.repro.shrub.precip)
unscaled.shrub.precip243 <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover)
insight.repro.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip243$Change_ShrubCover
insight.repro.shrub.precip$Prev_year_precip <- unscaled.shrub.precip243$Prev_year_precip

# Graph, no CI (insight version)
repro.shrub.precip <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Reproductive_culms,
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
  labs(y = expression(Delta ~ "Repro culm count"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in repro culm count vs. shrub cover change")
repro.shrub.precip


# Generate CI with scaled explanatory variable (use model 3, which includes precip*shrub interaction)
repro.pred.shrub.precip <- predict_response(repro_model3, 
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
  scale_y_continuous(limits = c(-88, 120))
repro.shrub.precip.ci



## Repro: Precip * herb (NS) ----------------------------------------------

# Generate prediction and add unscaled variable
insight.repro.herb.precip <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 3) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
insight.repro.herb.precip$Predicted <- get_predicted(repro_best.model, insight.repro.herb.precip)
unscaled.herb.precip243 <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover)
insight.repro.herb.precip$Change_HerbCover <- unscaled.herb.precip243$Change_HerbCover
insight.repro.herb.precip$Prev_year_precip <- unscaled.herb.precip243$Prev_year_precip

# Graph, no CI (insight version)
repro.herb.precip <- dat.culm %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Reproductive_culms,
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
  labs(y = expression(Delta ~ "Repro culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in repro culm count vs. herb cover change") +
  scale_y_continuous(limits = c(-88, 120))
repro.herb.precip


# Generate CI with scaled explanatory variable 
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
  scale_y_continuous(limits = c(-88, 120))
repro.herb.precip.ci




# Buffelgrass density change ----------------------------------------------

## BG density: Precip -----------------------------------------------------

# Generate prediction and add unscaled variable 
insight.bgden.precip <- get_datagrid(dat.plot.ex, by = c("Prev_year_precip_scaled"),
                                 length = 100)
insight.bgden.precip$Change_BGDensity <- get_predicted(bgden_best.model, insight.bgden.precip)
insight.bgden.precip$SE <- get_predicted_ci(bgden_best.model, data = insight.bgden.precip)$SE
insight.bgden.precip$CI <- insight.bgden.precip$SE * 1.96
unscaled.precip100 <- get_datagrid(dat.plot.unscaled, by = "Prev_year_precip",
                                   length = 100) %>% 
  arrange(Prev_year_precip)
insight.bgden.precip$Prev_year_precip <- unscaled.precip100$Prev_year_precip

# Graph (insight version) 
bgden.precip <- dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGDensity)) +
  geom_point() +
  geom_line(data = insight.bgden.precip,
            aes(y = Change_BGDensity), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.bgden.precip,
              aes(ymin = Change_BGDensity - CI, ymax = Change_BGDensity + CI), alpha = 0.2) +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Previous year precip (mm)",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       title = "Change in buffelgrass density vs. precip")
bgden.precip


# ggeffects version
#   Generate CI and add unscaled variable 
gg.bgden.precip <- predict_response(bgden_best.model, terms = "Prev_year_precip_scaled")
unscaled.precip15 <- get_datagrid(dat.plot.unscaled, by = "Prev_year_precip",
                                  length = 15) %>% 
  arrange(Prev_year_precip)
gg.bgden.precip$Prev_year_precip <- unscaled.precip15$Prev_year_precip
gg.bgden.precip$Change_BGDensity <- gg.bgden.precip$predicted 

#   Graph (ggeffects version)
bgden.precip.gg <- dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGDensity)) +
  geom_point() +
  geom_ribbon(data = gg.bgden.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.bgden.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  labs(x = "Previous year precip (mm)",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       title = "Change in buffelgrass density vs. precip") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
bgden.precip.gg


# Graph differences in prediction line (dashed = insight, solid is from ggeffects) 
dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGDensity)) +
  geom_point() +
  geom_ribbon(data = gg.bgden.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_ribbon(data = insight.bgden.precip,
              aes(ymin = Change_BGDensity - CI, ymax = Change_BGDensity + CI), alpha = 0.2) +
  geom_line(data = gg.bgden.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.bgden.precip,
            aes(y = Change_BGDensity), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  labs(x = "Previous year precip (mm)",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       title = "Change in buffelgrass density vs. precip") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") # insight is much lower (SE do not overlap much)



## BG density: Aspect -----------------------------------------------------

# ggeffects version
#   Generate prediction & CI
gg.bgden.aspect <- predict_response(bgden_best.model, terms = "Aspect") %>% 
  rename(Aspect = x,
         Change_BGDensity = predicted)

# Graph (ggeffects version)
bgden.aspect.gg <- dat.plot %>% 
  ggplot(aes(x = Aspect, y = Change_BGDensity)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  geom_pointrange(data = gg.bgden.aspect,
                  aes(ymin = conf.low, ymax = conf.high),
                  color = "purple3",
                  linewidth = 1.3) +
  geom_point(data = gg.bgden.aspect,
             aes(x = Aspect, y = Change_BGDensity),
             color = "purple3",
             size = 3,
             shape = 15) +
  theme_bw() +
  labs(title = "Change in plot density by aspect",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
bgden.aspect.gg



## BG density: Slope (NS) -------------------------------------------------

# insight version
#   Generate prediction and add unscaled variable (use model4, which includes slope)
insight.bgden.slope <- get_datagrid(dat.plot.ex, by = c("PlotSlope_scaled"),
                                length = 100)
insight.bgden.slope$Change_BGDensity <- get_predicted(bgden_best.model, insight.bgden.slope)
insight.bgden.slope$SE <- get_predicted_ci(bgden_best.model, data = insight.bgden.slope)$SE
insight.bgden.slope$CI <- insight.bgden.slope$SE * 1.96
unscaled.slope100 <- get_datagrid(dat.plot.unscaled, by = "PlotSlope",
                                  length = 100) %>% 
  arrange(PlotSlope)
insight.bgden.slope$PlotSlope <- unscaled.slope100$PlotSlope

#   Graph (insight version)
bgden.slope <- dat.plot %>% 
  ggplot(aes(x = PlotSlope, y = Change_BGDensity)) +
  geom_point() +
  geom_line(data = insight.bgden.slope,
            aes(y = Change_BGDensity), linewidth = 1,
            color = "purple3") +
  geom_ribbon(data = insight.bgden.slope,
              aes(ymin = Change_BGDensity - CI, ymax = Change_BGDensity + CI), alpha = 0.2) +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in buffelgrass density vs. slope") +
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")"))) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
bgden.slope


# ggeffects version
#   Generate CI and add unscaled variable 
gg.bgden.slope <- predict_response(bgden_best.model, terms = "PlotSlope_scaled")
unscaled.slope12 <- get_datagrid(dat.plot.unscaled, by = "PlotSlope",
                                 length = 12) %>% 
  arrange(PlotSlope)
gg.bgden.slope$PlotSlope <- unscaled.slope12$PlotSlope
gg.bgden.slope$Change_BGDensity <- gg.bgden.slope$predicted

# Graph (ggeffects version)
bgden.slope.gg <- dat.plot %>% 
  ggplot(aes(x = PlotSlope, y = Change_BGDensity)) +
  geom_point() +
  geom_ribbon(data = gg.bgden.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.bgden.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in bgden culm count vs. slope") +
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")"))) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
bgden.slope.gg


# Graph differences in prediction line (dashed = insight, solid = ggeffects)
dat.plot %>% 
  ggplot(aes(x = PlotSlope, y = Change_BGDensity)) +
  geom_point() +
  geom_ribbon(data = gg.bgden.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.bgden.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.bgden.slope,
            aes(y = Change_BGDensity), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in bgden culm count vs. slope") +
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")"))) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") # insight again lower



## BG density: Shrub (NS) -------------------------------------------------

# Generate prediction and add unscaled variable
insight.bgden.shrub <- get_datagrid(dat.plot.ex, by = c("Change_ShrubCover_scaled"),
                                length = 100)
insight.bgden.shrub$Change_BGDensity <- get_predicted(bgden_best.model, insight.bgden.shrub)
unscaled.shrub100 <- get_datagrid(dat.plot.unscaled, by = "Change_ShrubCover",
                                  length = 100) %>% 
  arrange(Change_ShrubCover)
insight.bgden.shrub$Change_ShrubCover <- unscaled.shrub100$Change_ShrubCover

# Graph (insight version)
bgden.shrub <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGDensity)) +
  geom_point() +
  geom_line(data = insight.bgden.shrub,
            aes(y = Change_BGDensity), linewidth = 1,
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
gg.bgden.shrub <- predict_response(bgden_best.model, terms = "Change_ShrubCover_scaled")
unscaled.shrub16 <- get_datagrid(dat.plot.unscaled, by = "Change_ShrubCover",
                                 length = 16) %>% 
  arrange(Change_ShrubCover)
gg.bgden.shrub$Change_ShrubCover <- unscaled.shrub16$Change_ShrubCover
gg.bgden.shrub$Change_BGDensity <- gg.bgden.shrub$predicted

# Graph (ggeffects version)
bgden.shrub.gg <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGDensity)) +
  geom_point() +
  geom_ribbon(data = gg.bgden.shrub,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.bgden.shrub,
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
bgden.shrub.gg


# Graph differences in prediction line (dashed = insight, solid = ggeffects)
dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGDensity)) +
  geom_point() +
  geom_ribbon(data = gg.bgden.shrub,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.bgden.shrub,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.bgden.shrub,
            aes(y = Change_BGDensity), linewidth = 1,
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



## BG density: Herb (NS) --------------------------------------------------

# Generate prediction and add unscaled variable (use model2, which includes herb)
insight.bgden.herb <- get_datagrid(dat.plot.ex, by = c("Change_HerbCover_scaled"),
                               length = 100)
insight.bgden.herb$Change_BGDensity <- get_predicted(bgden_best.model, insight.bgden.herb)
unscaled.herb100 <- get_datagrid(dat.plot.unscaled, by = "Change_HerbCover",
                                 length = 100) %>% 
  arrange(Change_HerbCover)
insight.bgden.herb$Change_HerbCover <- unscaled.herb100$Change_HerbCover

# Graph (insight version)
bgden.herb <- dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGDensity)) +
  geom_point() +
  geom_line(data = insight.bgden.herb,
            aes(y = Change_BGDensity), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in buffelgrass density vs. herb cover change") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
bgden.herb


# Generate CI and add unscaled variable
gg.bgden.herb <- predict_response(bgden_best.model, terms = "Change_HerbCover_scaled")
unscaled.herb16 <- get_datagrid(dat.plot.unscaled, by = "Change_HerbCover",
                                length = 16) %>% 
  arrange(Change_HerbCover)
gg.bgden.herb$Change_HerbCover <- unscaled.herb16$Change_HerbCover
gg.bgden.herb$Change_BGDensity <- gg.bgden.herb$predicted

# Graph (ggeffects version)
bgden.herb.gg <- dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGDensity)) +
  geom_point() +
  geom_ribbon(data = gg.bgden.herb,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.bgden.herb,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in buffelgrass density vs. herb cover change") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 
bgden.herb.gg


# Graph differences in prediction line (dashed = insight, solid = ggeffects)
dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGDensity)) +
  geom_point() +
  geom_ribbon(data = gg.bgden.herb,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.bgden.herb,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.bgden.herb,
            aes(y = Change_BGDensity), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in buffelgrass density vs. herb cover change") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") # insight much lower


## BG density: Precip * shrub (NS) ----------------------------------------

# Generate prediction and add unscaled variable
insight.bgden.shrub.precip <- dat.plot.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled)
insight.bgden.shrub.precip$Predicted <- get_predicted(bgden_best.model, insight.bgden.shrub.precip)
unscaled.shrub.precip243 <- dat.plot.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover)
insight.bgden.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip243$Change_ShrubCover
insight.bgden.shrub.precip$Prev_year_precip <- unscaled.shrub.precip243$Prev_year_precip

# Graph, no CI (insight version)
bgden.shrub.precip <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGDensity,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.bgden.shrub.precip,
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
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffelgrass density vs. shrub cover change")
bgden.shrub.precip


# Generate CI with scaled explanatory variable
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



## BG density: Precip * herb (NS) -----------------------------------------

# Generate prediction and add unscaled variable
insight.bgden.herb.precip <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
insight.bgden.herb.precip$Predicted <- get_predicted(bgden_best.model, insight.bgden.herb.precip)
unscaled.herb.precip243 <- dat.plot.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover)
insight.bgden.herb.precip$Change_HerbCover <- unscaled.herb.precip243$Change_HerbCover
insight.bgden.herb.precip$Prev_year_precip <- unscaled.herb.precip243$Prev_year_precip

# Graph, no CI (insight version)
bgden.herb.precip <- dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGDensity,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.bgden.herb.precip,
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
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in buffelgrass density vs. herb cover change")
bgden.herb.precip


# Generate CI with scaled explanatory variable
bgden.pred.herb.precip <- predict_response(bgden_best.model, 
                                           terms = c("Change_HerbCover_scaled", "Prev_year_precip_scaled"))
bgden.pred.herb.precip$group <- factor(bgden.pred.herb.precip$group,
                                       levels = c("1.24", "0.18", "-0.89"))
bgden.pred.herb.precip <- as.data.frame(bgden.pred.herb.precip)

# Graph with CI, scaled prediction only (ggeffects version)  
bgden.herb.precip.ci <- bgden.pred.herb.precip %>% 
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
       x = expression(Delta ~ "Native grass & forb cover (scaled)"),
       title = "Change in buffelgrass density vs. herb cover change (scaled)",
       color = "Previous year \nprecip (scaled)",
       fill = "Previous year \nprecip (scaled)") +
  scale_y_continuous(limits = c(-24, 32),
                     breaks = c(-20, 0, 20)) 
bgden.herb.precip.ci # insight again lower and seems to fit data better?


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
insight.bgcov.precip <- get_datagrid(dat.plot.ex, by = c("Prev_year_precip_scaled"),
                                 length = 100)
insight.bgcov.precip$Predicted <- get_predicted(bgcov_best.model, insight.bgcov.precip)
unscaled.precip100 <- get_datagrid(dat.plot.unscaled, by = "Prev_year_precip",
                                   length = 100) %>% 
  arrange(Prev_year_precip)
insight.bgcov.precip$Prev_year_precip <- unscaled.precip100$Prev_year_precip

# Graph (insight version)
bgcov.precip <- dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGCover)) +
  geom_point() +
  geom_line(data = insight.bgcov.precip,
            aes(y = Predicted), linewidth = 1,
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
gg.bgcov.precip <- predict_response(bgcov_best.model, terms = "Prev_year_precip_scaled")
unscaled.precip15 <- get_datagrid(dat.plot.unscaled, by = "Prev_year_precip",
                                  length = 15) %>% 
  arrange(Prev_year_precip)
gg.bgcov.precip$Prev_year_precip <- unscaled.precip15$Prev_year_precip
gg.bgcov.precip$Change_BGCover <- gg.bgcov.precip$predicted 

# Graph (ggeffects version)
bgcov.precip.gg <- dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGCover)) +
  geom_point() +
  geom_ribbon(data = gg.bgcov.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.bgcov.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  labs(x = "Previous year precip (mm)",
       y = expression(Delta ~ "Cover (%)"),
       title = "Change in buffelgrass cover vs. precip") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
bgcov.precip.gg


# Graph differences in prediction line (dashed = insight, solid is from ggeffects)
dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGCover)) +
  geom_point() +
  geom_ribbon(data = gg.bgcov.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.bgcov.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.bgcov.precip,
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

# ggeffects version
#   Generate prediction & CI
gg.bgcov.aspect <- predict_response(bgcov_best.model, terms = "Aspect") %>% 
  rename(Aspect = x,
         Change_BGCover = predicted)

# Graph (ggeffects version)
bgcov.aspect.gg <- dat.plot %>% 
  ggplot(aes(x = Aspect, y = Change_BGCover)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  geom_pointrange(data = gg.bgcov.aspect,
                  aes(ymin = conf.low, ymax = conf.high),
                  color = "purple3",
                  linewidth = 1.3) +
  geom_point(data = gg.bgcov.aspect,
             aes(x = Aspect, y = Change_BGCover),
             color = "purple3",
             size = 3,
             shape = 15) +
  theme_bw() +
  labs(title = "Change in plot cover by aspect",
       y = expression(Delta ~ "Cover (%)"),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
bgcov.aspect.gg



## BG cover: Shrub --------------------------------------------------------

# Generate prediction and add scaled variable 
insight.bgcov.shrub <- get_datagrid(dat.plot.ex, by = c("Change_ShrubCover_scaled"),
                                length = 100)
insight.bgcov.shrub$Predicted <- get_predicted(bgcov_best.model, insight.bgcov.shrub)
unscaled.shrub100 <- get_datagrid(dat.plot.unscaled, by = "Change_ShrubCover",
                                  length = 100) %>% 
  arrange(Change_ShrubCover)
insight.bgcov.shrub$Change_ShrubCover <- unscaled.shrub100$Change_ShrubCover

# Graph (insight version)
bgcov.shrub <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGCover)) +
  geom_point() +
  geom_line(data = insight.bgcov.shrub,
            aes(y = Predicted), linewidth = 1,
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
gg.bgcov.shrub <- predict_response(bgcov_best.model, terms = "Change_ShrubCover_scaled")
unscaled.shrub16 <- get_datagrid(dat.plot.unscaled, by = "Change_ShrubCover",
                                 length = 16) %>% 
  arrange(Change_ShrubCover)
gg.bgcov.shrub$Change_ShrubCover <- unscaled.shrub16$Change_ShrubCover
gg.bgcov.shrub$Change_BGCover <- gg.bgcov.shrub$predicted

# Graph (ggeffects version)
bgcov.shrub.gg <- dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGCover)) +
  geom_point() +
  geom_ribbon(data = gg.bgcov.shrub,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.bgcov.shrub,
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
bgcov.shrub.gg


# Graph differences in prediction line (dashed = insight, solid = ggeffects)
dat.plot %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGCover)) +
  geom_point() +
  geom_ribbon(data = gg.bgcov.shrub,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.bgcov.shrub,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.bgcov.shrub,
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
insight.bgcov.slope <- get_datagrid(dat.plot.ex, by = c("PlotSlope_scaled"),
                                length = 100)
insight.bgcov.slope$Predicted <- get_predicted(bgcov_best.model, insight.bgcov.slope)
unscaled.slope100 <- get_datagrid(dat.plot.unscaled, by = "PlotSlope",
                                  length = 100) %>% 
  arrange(PlotSlope)
insight.bgcov.slope$PlotSlope <- unscaled.slope100$PlotSlope

# Graph (insight version)
bgcov.slope <- dat.plot %>% 
  ggplot(aes(x = PlotSlope, y = Change_BGCover)) +
  geom_point() +
  geom_line(data = insight.bgcov.slope,
            aes(y = Predicted), linewidth = 1,
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
gg.bgcov.slope <- predict_response(bgcov_best.model, terms = "PlotSlope_scaled")
unscaled.slope12 <- get_datagrid(dat.plot.unscaled, by = "PlotSlope",
                                 length = 12) %>% 
  arrange(PlotSlope)
gg.bgcov.slope$PlotSlope <- unscaled.slope12$PlotSlope
gg.bgcov.slope$Change_BGCover <- gg.bgcov.slope$predicted

# Graph (ggeffects version)
bgcov.slope.gg <- dat.plot %>% 
  ggplot(aes(x = PlotSlope, y = Change_BGCover)) +
  geom_point() +
  geom_ribbon(data = gg.bgcov.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.bgcov.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  xlab("Plot slope (\u00B0)") +
  ggtitle("Change in buffelgrass cover vs. slope") +
  labs(y = expression(Delta ~ "Cover (%)")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") 
bgcov.slope.gg


# Graph differences in prediction line (dashed = insight, solid = ggeffects)
dat.plot %>% 
  ggplot(aes(x = PlotSlope, y = Change_BGCover)) +
  geom_point() +
  geom_ribbon(data = gg.bgcov.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.bgcov.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.bgcov.slope,
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
insight.bgcov.herb <- get_datagrid(dat.plot.ex, by = c("Change_HerbCover_scaled"),
                               length = 100)
insight.bgcov.herb$Predicted <- get_predicted(bgcov_best.model, insight.bgcov.herb)
unscaled.herb100 <- get_datagrid(dat.plot.unscaled, by = "Change_HerbCover",
                                 length = 100) %>% 
  arrange(Change_HerbCover)
insight.bgcov.herb$Change_HerbCover <- unscaled.herb100$Change_HerbCover

# Graph (insight version)
bgcov.herb <- dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGCover)) +
  geom_point() +
  geom_line(data = insight.bgcov.herb,
            aes(y = Predicted), linewidth = 1,
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
gg.bgcov.herb <- predict_response(bgcov_best.model, terms = "Change_HerbCover_scaled")
unscaled.herb8 <- get_datagrid(dat.plot.unscaled, by = "Change_HerbCover",
                               length = 8) %>% 
  arrange(Change_HerbCover)
gg.bgcov.herb$Change_HerbCover <- unscaled.herb8$Change_HerbCover
gg.bgcov.herb$Change_BGCover <- gg.bgcov.herb$predicted

# Graph (ggeffects version)
bgcov.herb.gg <- dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGCover)) +
  geom_point() +
  geom_ribbon(data = gg.bgcov.herb,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.bgcov.herb,
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
bgcov.herb.gg


# Graph differences in prediction line (dashed = insight, solid = ggeffects)
dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGCover)) +
  geom_point() +
  geom_ribbon(data = gg.bgcov.herb,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.bgcov.herb,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.bgcov.herb,
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
insight.bgcov.shrub.precip <- dat.plot.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled)
insight.bgcov.shrub.precip$Predicted <- get_predicted(bgcov_best.model, insight.bgcov.shrub.precip)
unscaled.shrub.precip243 <- dat.plot.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover)
insight.bgcov.shrub.precip$Change_ShrubCover <- unscaled.shrub.precip243$Change_ShrubCover
insight.bgcov.shrub.precip$Prev_year_precip <- unscaled.shrub.precip243$Prev_year_precip

# Graph, no CI (insight version)
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
  labs(y = expression(Delta ~ "Cover (%)"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffelgrass cover vs. shrub cover change")
bgcov.shrub.precip


# Generate CI with scaled explanatory variable
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
insight.bgcov.herb.precip <- dat.plot.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled)
insight.bgcov.herb.precip$Predicted <- get_predicted(bgcov_best.model, insight.bgcov.herb.precip)
unscaled.herb.precip243 <- dat.plot.unscaled %>% 
  get_datagrid(c("Change_HerbCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover)
insight.bgcov.herb.precip$Change_HerbCover <- unscaled.herb.precip243$Change_HerbCover
insight.bgcov.herb.precip$Prev_year_precip <- unscaled.herb.precip243$Prev_year_precip

# Graph, no CI (insight version)
bgcov.herb.precip <- dat.plot %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_BGCover,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.bgcov.herb.precip,
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
  labs(y = expression(Delta ~ "Cover (%)"),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in buffelgrass cover vs. herb cover change")
bgcov.herb.precip


# Generate CI with scaled explanatory variable
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
insight.survival.precip <- get_datagrid(dat.survival.ex, by = c("Prev_year_precip_scaled"),
                                    length = 50)
insight.survival.precip$Predicted <- get_predicted(survival_best.model, insight.survival.precip) # Predicting new random effect levels for terms: 1 | Transect:Site
unscaled.precip50 <- get_datagrid(dat.survival.unscaled, by = "Prev_year_precip",
                                  length = 50) %>% 
  arrange(Prev_year_precip)
insight.survival.precip$Prev_year_precip <- unscaled.precip50$Prev_year_precip

# Graph (insight version)
survival.precip <- dat.survival %>% 
  ggplot(aes(x = Prev_year_precip, y = survival_transf)) +
  geom_point() +
  geom_line(data = insight.survival.precip,
            aes(y = Predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Previous year precip (mm)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. precip") 
survival.precip 


# Generate CI and add unscaled variable
gg.survival.precip <- predict_response(survival_best.model, terms = "Prev_year_precip_scaled")
unscaled.precip11 <- get_datagrid(dat.survival.unscaled, by = "Prev_year_precip",
                                  length = 11) %>% 
  arrange(Prev_year_precip)
gg.survival.precip$Prev_year_precip <- unscaled.precip11$Prev_year_precip
gg.survival.precip$survival_transf <- gg.survival.precip$predicted 

# Graph (ggeffects version)
survival.precip.gg <- dat.survival %>% 
  ggplot(aes(x = Prev_year_precip, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = gg.survival.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.survival.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Previous year precip (mm)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. precip") 
survival.precip.gg


# Graph differences in prediction line (dashed = insight, solid is from ggeffects)
dat.survival %>% 
  ggplot(aes(x = Prev_year_precip, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = gg.survival.precip,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.survival.precip,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.survival.precip,
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
insight.survival.bgden <- get_datagrid(dat.survival.ex, by = c("BGDensity_scaled"),
                                   length = 50)
insight.survival.bgden$Predicted <- get_predicted(survival_best.model, insight.survival.bgden) # Predicting new random effect levels for terms: 1 | Transect:Site
unscaled.bgden50 <- get_datagrid(dat.survival.unscaled, by = "BGDensity",
                                 length = 50) %>% 
  arrange(BGDensity)
insight.survival.bgden$BGDensity <- unscaled.bgden50$BGDensity

# Graph (insight version)
survival.bgden <- dat.survival %>% 
  ggplot(aes(x = BGDensity, y = survival_transf)) +
  geom_point() +
  geom_line(data = insight.survival.bgden,
            aes(y = Predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = expression(paste("Density (individuals / ", m^2, ")")),
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. plot density") 
survival.bgden


# Generate CI and add unscaled variable
gg.survival.bgden <- predict_response(survival_best.model, terms = "BGDensity_scaled")
unscaled.bgden9 <- get_datagrid(dat.survival.unscaled, by = "BGDensity",
                                length = 9) %>% 
  arrange(BGDensity)
gg.survival.bgden$BGDensity <- unscaled.bgden9$BGDensity
gg.survival.bgden$survival_transf <- gg.survival.bgden$predicted 

# Graph (ggeffects version)
survival.bgden.gg <- dat.survival %>% 
  ggplot(aes(x = BGDensity, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = gg.survival.bgden,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.survival.bgden,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = expression(paste("Density (individuals / ", m^2, ")")),
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. plot density") 
survival.bgden.gg


# Graph differences in prediction line (dashed = insight, solid is from ggeffects)
dat.survival %>% 
  ggplot(aes(x = BGDensity, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = gg.survival.bgden,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.survival.bgden,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.survival.bgden,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = expression(paste("Density (individuals / ", m^2, ")")),
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. plot density") # basically the same




## Survival: Slope (NS) ---------------------------------------------------

# Generate prediction and add scaled variable (use model3)
insight.survival.slope <- get_datagrid(dat.survival.ex, by = c("PlotSlope_scaled"),
                                   length = 50)
insight.survival.slope$Predicted <- get_predicted(survival_model3, insight.survival.slope) # Predicting new random effect levels for terms: 1 | Transect:Site
unscaled.slope50 <- get_datagrid(dat.survival.unscaled, by = "PlotSlope",
                                 length = 50) %>% 
  arrange(PlotSlope)
insight.survival.slope$PlotSlope <- unscaled.slope50$PlotSlope

# Graph (insight version)
survival.slope <- dat.survival %>% 
  ggplot(aes(x = PlotSlope, y = survival_transf)) +
  geom_point() +
  geom_line(data = insight.survival.slope,
            aes(y = Predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Plot slope (\u00B0)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. plot slope") 
survival.slope 


# Generate CI and add unscaled variable
gg.survival.slope <- predict_response(survival_model3, terms = "PlotSlope_scaled")
unscaled.slope24 <- get_datagrid(dat.survival.unscaled, by = "PlotSlope",
                                 length = 24) %>% 
  arrange(PlotSlope)
gg.survival.slope$PlotSlope <- unscaled.slope24$PlotSlope
gg.survival.slope$survival_transf <- gg.survival.slope$predicted 

# Graph (ggeffects version)
survival.slope.gg <- dat.survival %>% 
  ggplot(aes(x = PlotSlope, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = gg.survival.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.survival.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Plot slope (\u00B0)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. plot slope") 
survival.slope.gg


# Graph differences in prediction line (dashed = insight, solid is from ggeffects)
dat.survival %>% 
  ggplot(aes(x = PlotSlope, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = gg.survival.slope,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.survival.slope,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.survival.slope,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Plot slope (\u00B0)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. plot slope") # basically the same



## Survival: Shrub (NS) ---------------------------------------------------

# Generate prediction and add scaled variable 
insight.survival.shrub <- get_datagrid(dat.survival.ex, by = c("ShrubCover_scaled"),
                                   length = 50)
insight.survival.shrub$Predicted <- get_predicted(survival_best.model, insight.survival.shrub) # Predicting new random effect levels for terms: 1 | Transect:Site
unscaled.shrub50 <- get_datagrid(dat.survival.unscaled, by = "ShrubCover",
                                 length = 50) %>% 
  arrange(ShrubCover)
insight.survival.shrub$ShrubCover <- unscaled.shrub50$ShrubCover

# Graph (insight version)
survival.shrub <- dat.survival %>% 
  ggplot(aes(x = ShrubCover, y = survival_transf)) +
  geom_point() +
  geom_line(data = insight.survival.shrub,
            aes(y = Predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Native shrub cover (%)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. shrub cover") 
survival.shrub 


# Generate CI and add unscaled variable
gg.survival.shrub <- predict_response(survival_best.model, terms = "ShrubCover_scaled")
unscaled.shrub11 <- get_datagrid(dat.survival.unscaled, by = "ShrubCover",
                                 length = 11) %>% 
  arrange(ShrubCover)
gg.survival.shrub$ShrubCover <- unscaled.shrub11$ShrubCover
gg.survival.shrub$survival_transf <- gg.survival.shrub$predicted 

# Graph (ggeffects version)
survival.shrub.gg <- dat.survival %>% 
  ggplot(aes(x = ShrubCover, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = gg.survival.shrub,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.survival.shrub,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Native shrub cover (%)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. shrub") 
survival.shrub.gg


# Graph differences in prediction line (dashed = insight, solid is from ggeffects)
dat.survival %>% 
  ggplot(aes(x = ShrubCover, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = gg.survival.shrub,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.survival.shrub,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.survival.shrub,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Native shrub cover (%)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. shrub cover") # basically the same



## Survival: Herb (NS) ----------------------------------------------------

# Generate prediction and add scaled variable 
insight.survival.herb <- get_datagrid(dat.survival.ex, by = c("HerbCover_scaled"),
                                  length = 50)
insight.survival.herb$Predicted <- get_predicted(survival_best.model, insight.survival.herb) # Predicting new random effect levels for terms: 1 | Transect:Site
unscaled.herb50 <- get_datagrid(dat.survival.unscaled, by = "HerbCover",
                                length = 50) %>% 
  arrange(HerbCover)
insight.survival.herb$HerbCover <- unscaled.herb50$HerbCover

# Graph (insight version)
survival.herb <- dat.survival %>% 
  ggplot(aes(x =HerbCover, y = survival_transf)) +
  geom_point() +
  geom_line(data = insight.survival.herb,
            aes(y = Predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Native forb & grass cover (%)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. herb cover") 
survival.herb 


# Generate CI and add unscaled variable
gg.survival.herb <- predict_response(survival_best.model, terms = "HerbCover_scaled")
unscaled.herb24 <- get_datagrid(dat.survival.unscaled, by = "HerbCover",
                                length = 24) %>% 
  arrange(HerbCover)
gg.survival.herb$HerbCover <- unscaled.herb24$HerbCover
gg.survival.herb$survival_transf <- gg.survival.herb$predicted 

# Graph (ggeffects version)
survival.herb.gg <- dat.survival %>% 
  ggplot(aes(x = HerbCover, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = gg.survival.herb,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.survival.herb,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Native forb & grass cover (%)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. herb cover") 
survival.herb.gg


# Graph differences in prediction line (dashed = insight, solid is from ggeffects)
dat.survival %>% 
  ggplot(aes(x = HerbCover, y = survival_transf)) +
  geom_point() +
  geom_ribbon(data = gg.survival.herb,
              aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(data = gg.survival.herb,
            aes(y = predicted), linewidth = 1,
            color = "purple3") +
  geom_line(data = insight.survival.herb,
            aes(y = Predicted), linewidth = 1,
            color = "purple3", linetype = "dashed") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Native forb & grass cover (%)",
       y = "Seedling survival (%)",
       title = "Buffelgrass seedling survival vs. herb cover") # basically the same



## Survival: Precip * density (NS) ----------------------------------------

# Generate prediction and add unscaled variable
insight.survival.bgden.precip <- dat.survival.ex %>% 
  get_datagrid(c("BGDensity_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(BGDensity_scaled)
insight.survival.bgden.precip$Predicted <- get_predicted(survival_best.model, insight.survival.bgden.precip) # Warning: Predicting new random effect levels for terms: 1 | Transect:Site
unscaled.bgden.precip243 <- dat.survival.unscaled %>% 
  get_datagrid(c("BGDensity", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(BGDensity)
insight.survival.bgden.precip$BGDensity <- unscaled.bgden.precip243$BGDensity
insight.survival.bgden.precip$Prev_year_precip <- unscaled.bgden.precip243$Prev_year_precip

# Graph, no CI (insight version)
survival.bgden.precip <- dat.survival %>% 
  ggplot(aes(x = BGDensity, y = survival_transf,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.survival.bgden.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  labs(y = "Seedling survival (%)",
       x = expression(paste("Density (individuals / ", m^2, ")")),
       title = "Buffelgrass seedling survival vs. density") +
  scale_y_continuous(labels = scales::percent)
survival.bgden.precip # lines are kind of weird and not smooth


# Generate CI with scaled explanatory variable
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
insight.survival.shrub.precip <- dat.survival.ex %>% 
  get_datagrid(c("ShrubCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(ShrubCover_scaled)
insight.survival.shrub.precip$Predicted <- get_predicted(survival_best.model, insight.survival.shrub.precip) # Warning: Predicting new random effect levels for terms: 1 | Transect:Site
unscaled.shrub.precip243 <- dat.survival.unscaled %>% 
  get_datagrid(c("ShrubCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(ShrubCover)
insight.survival.shrub.precip$ShrubCover <- unscaled.shrub.precip243$ShrubCover
insight.survival.shrub.precip$Prev_year_precip <- unscaled.shrub.precip243$Prev_year_precip

# Graph, no CI (insight version)
survival.shrub.precip <- dat.survival %>% 
  ggplot(aes(x = ShrubCover, y = survival_transf,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.survival.shrub.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  labs(y = "Seedling survival (%)",
       x = "Native shrub cover (%)",
       title = "Buffelgrass seedling survival vs. shrub cover") +
  scale_y_continuous(labels = scales::percent)
survival.shrub.precip # lol all the lines are just flat


# Generate CI with scaled explanatory variable
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



## Survival: Precip * herb (NS) -------------------------------------------

# Generate prediction and add unscaled variable
insight.survival.herb.precip <- dat.survival.ex %>% 
  get_datagrid(c("HerbCover_scaled", "Prev_year_precip_scaled"), length = 10) %>% 
  get_datagrid("Prev_year_precip_scaled", length = 3, numerics = "all") %>% 
  arrange(HerbCover_scaled)
insight.survival.herb.precip$Predicted <- get_predicted(survival_best.model, insight.survival.herb.precip) # Warning: Predicting new random effect levels for terms: 1 | Transect:Site
unscaled.herb.precip243 <- dat.survival.unscaled %>% 
  get_datagrid(c("HerbCover", "Prev_year_precip"), length = 10) %>% 
  get_datagrid("Prev_year_precip", length = 3, numerics = "all") %>% 
  arrange(HerbCover)
insight.survival.herb.precip$HerbCover <- unscaled.herb.precip243$HerbCover
insight.survival.herb.precip$Prev_year_precip <- unscaled.herb.precip243$Prev_year_precip

# Graph, CI (insight version)
survival.herb.precip <- dat.survival %>% 
  ggplot(aes(x = HerbCover, y = survival_transf,
             color = Prev_year_precip)) +
  geom_point() +
  geom_line(data = insight.survival.herb.precip,
            aes(y = Predicted, group = Prev_year_precip), linewidth = 1) +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  labs(y = "Seedling survival (%)",
       x = "Native grass & forb cover (%)",
       title = "Buffelgrass seedling survival vs. herb cover") +
  scale_y_continuous(labels = scales::percent)
survival.herb.precip 


# Generate CI with scaled explanatory variable
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

# Significant (insight & ggeffects all very similar)
# Total change vs. Prev_year_precip
tiff("figures/2025-08_draft-figures/Total-change_prediction_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.precip
dev.off()

# Total change vs. Change_BGDensity
tiff("figures/2025-08_draft-figures/Total-change_prediction_BG-density-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.bgden
dev.off()

# Total change vs. Change_ShrubCover
tiff("figures/2025-08_draft-figures/Total-change_prediction_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.shrub
dev.off()

# Total change interaction of precip*BG density
tiff("figures/2025-08_draft-figures/Total-change_prediction_BG-density-change-and-precip-interaction.tiff",
     units = "in", height = 7, width = 6, res = 150)
total.bgden.precip
dev.off()
# Total change interaction of precip*BG density (with CI, scaled)
tiff("figures/2025-08_draft-figures/Total-change_prediction_BG-density-change-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 7, width = 6, res = 150)
total.bgden.precip.ci
dev.off()

# Total change interaction of precip*shrub
tiff("figures/2025-08_draft-figures/Total-change_prediction_shrub-cover-change-and-precip-interaction.tiff",
     units = "in", height = 7, width = 6, res = 150)
total.shrub.precip
dev.off()
# Total change interaction of precip*shrub (with CI, scaled)
tiff("figures/2025-08_draft-figures/Total-change_prediction_shrub-cover-change-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 7, width = 6, res = 150)
total.shrub.precip.ci
dev.off()

# Total change interaction of precip*herb
tiff("figures/2025-08_draft-figures/Total-change_prediction_herb-cover-change-and-precip-interaction.tiff",
     units = "in", height = 7, width = 6, res = 150)
total.herb.precip
dev.off()
# Total change interaction of precip*herb (with CI, scaled)
tiff("figures/2025-08_draft-figures/Total-change_prediction_herb-cover-change-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 7, width = 6, res = 150)
total.herb.precip.ci
dev.off()


# Not significant (insight & ggeffects all very similar)
# Total change by Aspect
tiff("figures/2025-08_draft-figures/Total-change_prediction_aspect.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.aspect.gg
dev.off()

# Total change vs. PlotSlope
tiff("figures/2025-08_draft-figures/Total-change_prediction_plot-slope.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.slope
dev.off()

# Total change vs. Change_HerbCover 
tiff("figures/2025-08_draft-figures/Total-change_prediction_herb-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.herb
dev.off()



## Repro change -----------------------------------------------------------

# Significant
# Repro change by Aspect
tiff("figures/2025-08_draft-figures/Repro-change-by-aspect.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.aspect.gg
dev.off()

# Repro change vs. Change_BGDensity
tiff("figures/2025-08_draft-figures/Repro-change_prediction_BG-density-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.bgden
dev.off()

# Repro change vs. Change_ShrubCover
tiff("figures/2025-08_draft-figures/Repro-change_prediction_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.shrub
dev.off()

# Repro change vs. Change_HerbCover
tiff("figures/2025-08_draft-figures/Repro-change_prediction_herb-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.herb
dev.off()


# Not significant
# Repro change vs. Prev_year_precip
tiff("figures/2025-08_draft-figures/Repro-change_prediction_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.precip
dev.off()

# Repro change vs. PlotSlope
tiff("figures/2025-08_draft-figures/Repro-change_prediction_plot-slope.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.slope
dev.off()

# Repro change interaction of precip*BG density
tiff("figures/2025-08_draft-figures/Repro-change_prediction_BG-density-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.bgden.precip
dev.off()
# Repro change interaction of precip*BG density (with CI, scaled)
tiff("figures/2025-08_draft-figures/Repro-change_prediction_BG-density-change-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.bgden.precip.ci
dev.off()

# Repro change interaction of precip*shrub
tiff("figures/2025-08_draft-figures/Repro-change_prediction_shrub-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.shrub.precip
dev.off()
# Repro change interaction of precip*shrub (with CI, scaled)
tiff("figures/2025-08_draft-figures/Repro-change_prediction_shrub-cover-change-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.shrub.precip.ci
dev.off()

# Repro change interaction of precip*herb
tiff("figures/2025-08_draft-figures/Repro-change_prediction_herb-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.herb.precip
dev.off()
# Repro change interaction of precip*herb (with CI, scaled)
tiff("figures/2025-08_draft-figures/Repro-change_prediction_herb-cover-change-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.herb.precip.ci
dev.off()



## BG density change ------------------------------------------------------

# Significant
# BG density change vs. Prev_year_precip
tiff("figures/2025-08_draft-figures/BG-density-change_prediction_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.precip
dev.off()

# BG density change by Aspect (original data only)
tiff("figures/2025-08_draft-figures/BG-density-change-by-aspect.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.change.aspect
dev.off()
# BG density change by Aspect (prediction only)
tiff("figures/2025-08_draft-figures/BG-density-change-aspect-predictions_gg.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.pred.aspect
dev.off()

# BG density change vs. Change_ShrubCover
tiff("figures/2025-08_draft-figures/BG-density-change_prediction_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.shrub
dev.off()

# BG density change interaction of precip*shrub
tiff("figures/2025-08_draft-figures/BG-density-change_prediction_shrub-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.shrub.precip
dev.off()
# BG density change interaction of precip*shrub (with CI, scaled)
tiff("figures/2025-08_draft-figures/BG-density-change_prediction_shrub-cover-change-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.shrub.precip.ci
dev.off()
# BG density change interaction of precip*shrub (long)
tiff("figures/2025-08_draft-figures/BG-density-change_prediction_shrub-cover-change-and-precip-interaction_long.tiff",
     units = "in", height = 7, width = 6, res = 150)
bgden.shrub.precip
dev.off()


# Not significant
# BG density change vs. PlotSlope
tiff("figures/2025-08_draft-figures/BG-density-change_prediction_plot-slope.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.slope
dev.off()



## BG cover change --------------------------------------------------------

# Significant
# BG cover change vs. Prev_year_precip
tiff("figures/2025-08_draft-figures/BG-cover-change_prediction_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.precip
dev.off()

# BG cover change by Aspect (original data only)
tiff("figures/2025-08_draft-figures/BG-cover-change-by-aspect.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.change.aspect
dev.off()
# BG cover change by Aspect (prediction only)
tiff("figures/2025-08_draft-figures/BG-cover-change-aspect-predictions_gg.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.pred.aspect
dev.off()

# BG cover change vs. Change_ShrubCover
tiff("figures/2025-08_draft-figures/BG-cover-change_prediction_shrub-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.shrub
dev.off()


# Not significant
# BG cover change vs. PlotSlope
tiff("figures/2025-08_draft-figures/BG-cover-change_prediction_plot-slope.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.slope
dev.off()

# BG cover change vs. Change_HerbCover
tiff("figures/2025-08_draft-figures/BG-cover-change_prediction_herb-cover-change.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.herb
dev.off()

# BG cover change interaction of precip*shrub
tiff("figures/2025-08_draft-figures/BG-cover-change_prediction_shrub-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.shrub.precip
dev.off()
# BG cover change interaction of precip*shrub (with CI, scaled)
tiff("figures/2025-08_draft-figures/BG-cover-change_prediction_shrub-cover-change-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.shrub.precip.ci
dev.off()

# BG cover change interaction of precip*herb
tiff("figures/2025-08_draft-figures/BG-cover-change_prediction_herb-cover-change-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.herb.precip
dev.off()
# BG cover change interaction of precip*herb (with CI, scaled)
tiff("figures/2025-08_draft-figures/BG-cover-change_prediction_herb-cover-change-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.herb.precip.ci
dev.off()



## Survival ---------------------------------------------------------------

# Significant
# Survival vs. Prev_year_precip
tiff("figures/2025-08_draft-figures/Survival_prediction_prev-year-precip.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.precip
dev.off()
# Survival vs. Prev_year_precip (with CI)
tiff("figures/2025-08_draft-figures/Survival_prediction_prev-year-precip_gg.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.precip.gg
dev.off()

# Survival vs. BGDensity
tiff("figures/2025-08_draft-figures/Survival_prediction_BG-density.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.bgden
dev.off()
# Survival vs. BGDensity (with CI)
tiff("figures/2025-08_draft-figures/Survival_prediction_BG-density_gg.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.bgden.gg
dev.off()


# Not significant
# Survival vs. PlotSlope
tiff("figures/2025-08_draft-figures/Survival_prediction_plot-slope.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.slope
dev.off()
# Survival vs. PlotSlope (with CI)
tiff("figures/2025-08_draft-figures/Survival_prediction_plot-slope_gg.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.slope.gg
dev.off()

# Survival vs. ShrubCover
tiff("figures/2025-08_draft-figures/Survival_prediction_shrub-cover.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.shrub
dev.off()
# Survival vs. ShrubCover (with CI)
tiff("figures/2025-08_draft-figures/Survival_prediction_shrub-cover_gg.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.shrub.gg
dev.off()

# Survival vs. HerbCover
tiff("figures/2025-08_draft-figures/Survival_prediction_herb-cover.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.herb
dev.off()
# Survival vs. HerbCover (with CI)
tiff("figures/2025-08_draft-figures/Survival_prediction_herb-cover_gg.tiff",
     units = "in", height = 4, width = 5, res = 150)
survival.herb.gg
dev.off()

# Survival interaction of precip*density
tiff("figures/2025-08_draft-figures/Survival_prediction_BG-density-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.bgden.precip
dev.off()
# Survival interaction of precip*density (with CI, scaled)
tiff("figures/2025-08_draft-figures/Survival_prediction_BG-density-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.bgden.precip.ci
dev.off()

# Survival interaction of precip*shrub
tiff("figures/2025-08_draft-figures/Survival_prediction_shrub-cover-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.shrub.precip
dev.off()
# Survival interaction of precip*shrub (with CI, scaled)
tiff("figures/2025-08_draft-figures/Survival_prediction_shrub-cover-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.shrub.precip.ci
dev.off()

# Survival interaction of precip*herb
tiff("figures/2025-08_draft-figures/Survival_prediction_herb-cover-and-precip-interaction.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.herb.precip
dev.off()
# Survival interaction of precip*herb (with CI, scaled)
tiff("figures/2025-08_draft-figures/Survival_prediction_herb-cover-and-precip-interaction_CI-scaled.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival.herb.precip.ci
dev.off()



## Precip plot ------------------------------------------------------------

# Combined precip plot for density, cover, survival
tiff("figures/2025-08_draft-figures/Precip-combined_density-cover-survival.tiff",
     units = "in", height = 7, width = 9, res = 150)
ggarrange(bgden.precip, bgcov.precip, survival.precip,
          ncol = 2, nrow = 2,
          labels = c("(A)", "(B)", "(C)"))
dev.off()


save.image("RData/05.5_draft-figs-for-lm-6.3.RData")
