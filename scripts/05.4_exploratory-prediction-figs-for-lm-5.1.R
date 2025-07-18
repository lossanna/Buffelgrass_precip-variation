# Created: 2025-07-18
# Updated: 2025-07-18

# Purpose: Explore plots made with modelbased - graph predictions instead of simple
#   linear regressions.

library(tidyverse)
library(modelbased)
library(insight)

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
viz.total<- get_datagrid(dat.total, by = c("Change_Total_Live_Culms", 
                                           "Prev_year_precip_scaled",
                                           "Aspect", "PlotSlope_scaled", 
                                           "Change_ShrubCover_scaled", "Change_HerbCover_scaled",
                                           "Change_BGDensity_scaled"),
                         preserve_range = TRUE)
viz.total$Predicted <- get_predicted(total_best.model, viz.total)
viz.total <- estimate_expectation(total_best.model)
