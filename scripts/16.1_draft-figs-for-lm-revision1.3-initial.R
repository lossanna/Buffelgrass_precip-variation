# Created: 2026-02-04
# Updated: 2026-03-03

# Purpose: Graph the slope * shrub interaction from the new test culm model 
#   from revision v1.3.


library(tidyverse)
library(insight)
library(modelbased)
library(viridis)

# Load data ---------------------------------------------------------------

load("RData/15.1_data-and-model-revision1.3-initial.RData")
dat <- read_csv("data/cleaned/11.1_demography-data_clean.csv")


# Data wrangling ----------------------------------------------------------

# Need 3 datasets:
#   1. Data for graphing observed data
#   2. Data to construct datagrid with predictions & CI
#   3. Data to construct datagrid with unscaled explanatory variable (to be added to #2)


# 1. Dataset for graphing (response and unscaled explanatory variables)
#   Culm change - total & repro
dat.culm <- culm.change.flat.rm %>% 
  select(Change_Total_Live_Culms, Change_Reproductive_culms, 
         Prev_year_precip, Aspect, PlotSlope, Change_BGDensity, Change_ShrubCover, Change_HerbCover,
         Init_BGDensity, Init_ShrubCover, Init_HerbCover)


# 2. Dataset for constructing datagrid with prediction & CI (scaled explanatory variables only)
#   Culm change - total & repro 
dat.culm.ex <- culm.change.flat.rm %>% 
  select(Prev_year_precip_scaled, Aspect, PlotSlope_scaled, 
         Change_BGDensity_scaled, Change_ShrubCover_scaled, Change_HerbCover_scaled,
         Init_BGDensity_scaled, Init_ShrubCover_scaled, Init_HerbCover_scaled)


# 3. Dataset for constructing datagrid with unscaled variables to match graph (unscaled explanatory variables only)
#   Culm change - total & repro
dat.culm.unscaled <- culm.change.flat.rm %>% 
  select(Prev_year_precip, Aspect, PlotSlope, 
         Change_BGDensity, Change_ShrubCover, Change_HerbCover,
         Init_BGDensity, Init_ShrubCover, Init_HerbCover)




# Test different slope levels ---------------------------------------------

# Slope range for culm
range(dat.culm.ex$PlotSlope_scaled) # -2.35, 2.77


## Culm models ------------------------------------------------------------

# Results: I checked 3-12 slope levels, and they were basically the same as R1.1;
#   also with more variables it starts creating huge dataframes and takes forever
#   to load, so I will just go with 10 slope levels, which is -0.073.

# 3 slope levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # 0.212

# 4 slope levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 4, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # -0.642

# 5 slope levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 5, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # -0.212

# 6 slope levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 6, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # -0.300

# 7 slope levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 7, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # 0.212

# 8 slope levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 8, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # -0.154

# 9 slope levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 5) %>% 
  get_datagrid("PlotSlope_scaled", length = 9, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # 0.212

# 10 slope levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 10, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # -0.073

# 11 slope levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 11, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # 0.212

# 12 slope levels
x <- dat.culm.ex %>% 
  get_datagrid(c("Change_HerbCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 12, numerics = "all") %>% 
  arrange(Change_HerbCover_scaled) %>% 
  distinct(.keep_all = TRUE)
unique(x$PlotSlope_scaled) # -0.021



## Total: Slope * shrub ---------------------------------------------------

# Generate prediction and add unscaled variable 
insight.total.shrub.slope <- dat.culm.ex %>% 
  get_datagrid(c("Change_ShrubCover_scaled", "PlotSlope_scaled"), length = 3) %>% 
  get_datagrid("PlotSlope_scaled", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover_scaled) %>% 
  distinct(.keep_all = TRUE)
insight.total.shrub.slope$Predicted <- get_predicted(total7, insight.total.shrub.slope)
unscaled.shrub.slope <- dat.culm.unscaled %>% 
  get_datagrid(c("Change_ShrubCover", "PlotSlope"), length = 3) %>% 
  get_datagrid("PlotSlope", length = 3, numerics = "all") %>% 
  arrange(Change_ShrubCover) %>% 
  distinct(.keep_all = TRUE)
insight.total.shrub.slope$Change_ShrubCover <- unscaled.shrub.slope$Change_ShrubCover
insight.total.shrub.slope$PlotSlope <- unscaled.shrub.slope$PlotSlope
unique(insight.total.shrub.slope$PlotSlope_scaled)
insight.total.shrub.slope <- insight.total.shrub.slope %>% 
  filter(PlotSlope_scaled %in% c(-2.349, 0.212, 2.773))

# Graph
total.shrub.slope <- dat.culm %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Total_Live_Culms,
             color = PlotSlope)) +
  geom_point(alpha = 0.7) +
  geom_line(data = insight.total.shrub.slope,
            aes(y = Predicted, group = PlotSlope), linewidth = 1.5) +
  theme_bw() +
  scale_color_viridis(option = "plasma", direction = -1,
                      name = "Plot slope (\u00B0)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in total culm count vs. change in shrub cover")
total.shrub.slope
