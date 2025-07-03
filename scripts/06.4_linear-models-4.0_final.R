# Created: 2025-06-23
# Updated: 2025-06-27

# Purpose: Consolidate final results (model 7 from 06.2.R, and model 8 for survival) 
#   into single script.

library(tidyverse)
library(glmmTMB)
library(performance)
library(DHARMa)
library(MuMIn)

# Load data ---------------------------------------------------------------

dat <- read_csv("data/cleaned/04_demography-data_clean.csv")
culm.change.raw <- read_csv("data/cleaned/04_change-in-culm-density-cover_clean.csv")

# Data wrangling ----------------------------------------------------------

# Center and scale numeric variables
culm.change <- culm.change.raw %>% 
  mutate(Perc_dev_scaled = scale(Perc_dev, center = TRUE, scale = TRUE)[, ],
         Elevation_ft_scaled = scale(Elevation_ft, center = TRUE, scale = TRUE)[, 1],
         Elevation_m_scaled = scale(Elevation_m, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         Change_BGDensity_scaled = scale(Change_BGDensity, scale = TRUE)[, 1],
         Change_BGCover_scaled = scale(Change_BGCover, scale = TRUE)[, 1],
         Change_ShrubCover_scaled = scale(Change_ShrubCover, scale = TRUE)[, 1],
         Change_HerbCover_scaled = scale(Change_HerbCover, scale = TRUE)[, 1])

culm.change.flat.rm <- culm.change %>% 
  filter(Aspect != "flat")

# Separate out plot-level data
plot.change <- culm.change %>% 
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm,
         -Change_Reproductive_culms, -Change_Total_Live_Culms) %>% 
  distinct(.keep_all = TRUE)

plot.change <- plot.change %>% 
  mutate(Perc_dev_scaled = scale(Perc_dev,center = TRUE, scale = TRUE)[, 1],
         Elevation_ft_scaled = scale(Elevation_ft, center = TRUE, scale = TRUE)[, 1],
         Elevation_m_scaled = scale(Elevation_m, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1]) %>% 
  filter(Aspect != "flat")


dat.plot <- dat %>% 
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm) %>% 
  distinct(.keep_all = TRUE)
dat.plot <- dat.plot %>% 
  filter(Aspect != "flat") %>% 
  mutate(Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1])


# Prepare survival data
dat.survival <- dat %>% 
  filter(!is.na(survival_perc),
         Aspect != "flat") %>% 
  mutate(Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         ShrubCover_scaled = scale(ShrubCover, center = TRUE, scale = TRUE)[, 1],
         HerbCover_scaled = scale(HerbCover, center = TRUE, scale = TRUE)[, 1],
         Elevation_m_scaled = scale(Elevation_m, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         BGDensity_scaled = scale(BGDensity, center = TRUE, scale = TRUE)[, 1])

dat.survival.plot <- dat.survival %>% 
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm) %>% 
  distinct(.keep_all = TRUE)



# Total culms -------------------------------------------------------------

# Total 7: glmmTMB version
total7 <- glmmTMB(Change_Total_Live_Culms ~ Prev_year_precip_scaled + Elevation_m_scaled + 
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                    Change_BGDensity_scaled +
                    Prev_year_precip_scaled * Change_BGDensity_scaled +
                    Prev_year_precip_scaled * PlotSlope_scaled + 
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Prev_year_precip_scaled * Change_HerbCover_scaled +
                    Aspect * PlotSlope_scaled + (1 | Site / Transect),
                  data = culm.change.flat.rm,
                  family = gaussian) 
summary(total7)
r2(total7) # marginal: 0.235
res.total7 <- simulateResiduals(total7)
plotQQunif(res.total7)
plotResiduals(res.total7)
check_collinearity(total7)
check_model(total7)


## Total 7: Model selection -----------------------------------------------

# Model selection
options(na.action = "na.fail")
total7_set <- dredge(total7)

# Examine best model
total7_best.model <- get.models(total7_set, 1)[[1]]
summary(total7_best.model)
r2(total7_best.model) # marginal: 0.230
res.total7_best.model <- simulateResiduals(total7_best.model)
plotQQunif(res.total7_best.model)
plotResiduals(res.total7_best.model)
check_model(total7_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
total7_top <- subset(total7_set, delta <= 2) 
for (i in 1:nrow(total7_top)) {
  assign(paste0("total7_model", i), get.models(total7_top, subset = i)[[1]])
} 

# R^2 of top models
r2(total7_model1) # marginal: 0.230
r2(total7_model2) # marginal: 0.229
r2(total7_model3) # marginal: 0.230
r2(total7_model4) # marginal: 0.225

# Model averaging of top models
total7_avg <- model.avg(total7_set, subset = delta <= 2)
summary(total7_avg)
total7_importance <- sw(total7_avg)
total7_importance.df <- data.frame(variable = names(total7_importance),
                                   importance = total7_importance)
total7_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
total7_avg.all <- model.avg(total7_set)
summary(total7_avg.all)
total7_importance.all <- sw(total7_avg.all)
total7_importance.all.df <- data.frame(variable = names(total7_importance.all),
                                       importance = total7_importance.all)
total7_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()



# Reproductive culms ------------------------------------------------------

# Repro 7: glmmTMB version
repro7 <- glmmTMB(Change_Reproductive_culms ~ Prev_year_precip_scaled + Elevation_m_scaled + 
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                    Change_BGDensity_scaled +
                    Prev_year_precip_scaled * Change_BGDensity_scaled +
                    Prev_year_precip_scaled * PlotSlope_scaled + 
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Prev_year_precip_scaled * Change_HerbCover_scaled +
                    Aspect * PlotSlope_scaled + (1 | Site / Transect),
                  data = culm.change.flat.rm,
                  family = gaussian) 
summary(repro7)
r2(repro7) # marginal: 0.154
res.repro7 <- simulateResiduals(repro7)
plotQQunif(res.repro7)
plotResiduals(res.repro7)
check_collinearity(repro7)
check_model(repro7)


## Repro 7: Model selection -----------------------------------------------

# Model selection
options(na.action = "na.fail")
repro7_set <- dredge(repro7)

# Examine best model
repro7_best.model <- get.models(repro7_set, 1)[[1]]
summary(repro7_best.model)
r2(repro7_best.model) # marginal: 0.123
res.repro7_best.model <- simulateResiduals(repro7_best.model)
plotQQunif(res.repro7_best.model)
plotResiduals(res.repro7_best.model)
check_model(repro7_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
repro7_top <- subset(repro7_set, delta <= 2) 
for (i in 1:nrow(repro7_top)) {
  assign(paste0("repro7_model", i), get.models(repro7_top, subset = i)[[1]])
} 

# R^2 of top models
r2(repro7_model1) # marginal: 0.123
r2(repro7_model2) # marginal: 0.125
r2(repro7_model3) # marginal: 0.126
r2(repro7_model4) # marginal: 0.128
r2(repro7_model5) # marginal: 0.137
r2(repro7_model6) # marginal: 0.140

# Model averaging of top models
repro7_avg <- model.avg(repro7_set, subset = delta <= 2)
summary(repro7_avg)
repro7_importance <- sw(repro7_avg)
repro7_importance.df <- data.frame(variable = names(repro7_importance),
                                   importance = repro7_importance)
repro7_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
repro7_avg.all <- model.avg(repro7_set)
summary(repro7_avg.all)
repro7_importance.all <- sw(repro7_avg.all)
repro7_importance.all.df <- data.frame(variable = names(repro7_importance.all),
                                       importance = repro7_importance.all)
repro7_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()



# Buffelgrass density -----------------------------------------------------

# BG density 7: glmmTMB version
bgden7 <- glmmTMB(Change_BGDensity ~ Prev_year_precip_scaled + Elevation_m_scaled +
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                    Change_HerbCover_scaled + 
                    Prev_year_precip_scaled * PlotSlope_scaled +
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Prev_year_precip_scaled * Change_HerbCover_scaled,
                  data = plot.change,
                  family = gaussian)
summary(bgden7)
r2(bgden7) # adjusted: 0.423
res.bgden7 <- simulateResiduals(bgden7)
plotQQunif(res.bgden7)
plotResiduals(res.bgden7)
check_collinearity(bgden7) 
check_model(bgden7)


## BG density 7: Model selection ------------------------------------------

# Model selection
options(na.action = "na.fail")
bgden7_set <- dredge(bgden7)

# Examine best model
bgden7_best.model <- get.models(bgden7_set, 1)[[1]]
summary(bgden7_best.model)
r2(bgden7_best.model) # adjusted: 0.427
res.bgden7_best.model <- simulateResiduals(bgden7_best.model)
plotQQunif(res.bgden7_best.model)
plotResiduals(res.bgden7_best.model)
check_collinearity(bgden7_best.model)
check_model(bgden7_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
bgden7_top <- subset(bgden7_set, delta <= 2)
for (i in 1:nrow(bgden7_top)) {
  assign(paste0("bgden7_model", i), get.models(bgden7_top, subset = i)[[1]])
}

r2(bgden7_model1) # adjusted: 0.427
r2(bgden7_model2) # adjusted: 0.424
r2(bgden7_model3) # adjusted: 0.419
r2(bgden7_model4) # adjusted: 0.422
r2(bgden7_model5) # adjusted: 0.422
r2(bgden7_model6) # adjusted: 0.425

# Model averaging of top models
bgden7_avg <- model.avg(bgden7_set, subset = delta <= 2) 
summary(bgden7_avg)
bgden7_importance <- sw(bgden7_avg)
bgden7_importance.df <- data.frame(variable = names(bgden7_importance),
                                   importance = bgden7_importance)
bgden7_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
bgden7_avg.all <- model.avg(bgden7_set)
summary(bgden7_avg.all)
bgden7_importance.all <- sw(bgden7_avg.all)
bgden7_importance.all.df <- data.frame(variable = names(bgden7_importance.all),
                                       importance = bgden7_importance.all)
bgden7_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()



# Buffelgrass cover -------------------------------------------------------

# BG cover 7: glmmTMB version
bgcov7 <- glmmTMB(Change_BGCover ~ Prev_year_precip_scaled + Elevation_m_scaled +
                    Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                    Change_HerbCover_scaled + 
                    Prev_year_precip_scaled * PlotSlope_scaled +
                    Prev_year_precip_scaled * Change_ShrubCover_scaled +
                    Prev_year_precip_scaled * Change_HerbCover_scaled,
                  data = plot.change,
                  family = gaussian)
summary(bgcov7)
r2(bgcov7) # adjusted: 0.224
res.bgcov7 <- simulateResiduals(bgcov7)
plotQQunif(res.bgcov7)
plotResiduals(res.bgcov7)
check_collinearity(bgcov7) 
check_model(bgcov7)


## BG cover 7: Model selection --------------------------------------------

# Model selection
options(na.action = "na.fail")
bgcov7_set <- dredge(bgcov7)

# Examine best model
bgcov7_best.model <- get.models(bgcov7_set, 1)[[1]]
summary(bgcov7_best.model)
r2(bgcov7_best.model) # adjusted: 0.220
res.bgcov7_best.model <- simulateResiduals(bgcov7_best.model)
plotQQunif(res.bgcov7_best.model)
plotResiduals(res.bgcov7_best.model)
check_collinearity(bgcov7_best.model)
check_model(bgcov7_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
bgcov7_top <- subset(bgcov7_set, delta <= 2)
for (i in 1:nrow(bgcov7_top)) {
  assign(paste0("bgcov7_model", i), get.models(bgcov7_top, subset = i)[[1]])
}

r2(bgcov7_model1) # adjusted: 0.220
r2(bgcov7_model2) # adjusted: 0.231
r2(bgcov7_model3) # adjusted: 0.227
r2(bgcov7_model4) # adjusted: 0.223
r2(bgcov7_model5) # adjusted: 0.222
r2(bgcov7_model7) # adjusted: 0.230
r2(bgcov7_model7) # adjusted: 0.230

# Model averaging of top models
bgcov7_avg <- model.avg(bgcov7_set, subset = delta <= 2) 
summary(bgcov7_avg)
bgcov7_importance <- sw(bgcov7_avg)
bgcov7_importance.df <- data.frame(variable = names(bgcov7_importance),
                                   importance = bgcov7_importance)
bgcov7_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
bgcov7_avg.all <- model.avg(bgcov7_set)
summary(bgcov7_avg.all)
bgcov7_importance.all <- sw(bgcov7_avg.all)
bgcov7_importance.all.df <- data.frame(variable = names(bgcov7_importance.all),
                                       importance = bgcov7_importance.all)
bgcov7_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()



# Survival ----------------------------------------------------------------

# Survival 8: glmmTMB version
survival8 <- glmmTMB(survival_perc ~ Prev_year_precip_scaled + Elevation_m_scaled +
                       Aspect + PlotSlope_scaled + ShrubCover_scaled +
                       HerbCover_scaled + BGDensity_scaled +
                       Prev_year_precip_scaled * PlotSlope_scaled +
                       Prev_year_precip_scaled * ShrubCover_scaled +
                       Prev_year_precip_scaled * HerbCover_scaled +
                       Prev_year_precip_scaled * BGDensity_scaled,
                     data = dat.survival.plot,
                     family = gaussian)
summary(survival8)
r2(survival8) # adjusted: 0.4533
res.survival8 <- simulateResiduals(survival8)
plotQQunif(res.survival8)
plotResiduals(res.survival8)
check_collinearity(survival8) 
check_model(survival8)


## Survival 8: Model selection --------------------------------------------

# Model selection
options(na.action = "na.fail")
survival8_set <- dredge(survival8)

# Examine best model
survival8_best.model <- get.models(survival8_set, 1)[[1]]
summary(survival8_best.model)
r2(survival8_best.model) # adjusted: 0.530
res.survival8_best.model <- simulateResiduals(survival8_best.model)
plotQQunif(res.survival8_best.model)
plotResiduals(res.survival8_best.model)
check_collinearity(survival8_best.model)
check_model(survival8_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
survival8_top <- subset(survival8_set, delta <= 2)
for (i in 1:nrow(survival8_top)) {
  assign(paste0("survival8_model", i), get.models(survival8_top, subset = i)[[1]])
}

r2(survival8_model1) # adjusted: 0.530
r2(survival8_model2) # adjusted: 0.527
r2(survival8_model3) # adjusted: 0.526
r2(survival8_model4) # adjusted: 0.525
r2(survival8_model5) # adjusted: 0.523
r2(survival8_model6) # adjusted: 0.525
r2(survival8_model7) # adjusted: 0.527

# Model averaging of top models
survival8_avg <- model.avg(survival8_set, subset = delta <= 2) 
summary(survival8_avg)
survival8_importance <- sw(survival8_avg)
survival8_importance.df <- data.frame(variable = names(survival8_importance),
                                      importance = survival8_importance)
survival8_importance.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()

# Model averaging of all models
survival8_avg.all <- model.avg(survival8_set)
summary(survival8_avg.all)
survival8_importance.all <- sw(survival8_avg.all)
survival8_importance.all.df <- data.frame(variable = names(survival8_importance.all),
                                          importance = survival8_importance.all)
survival8_importance.all.df %>% 
  ggplot(aes(x = reorder(variable, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  theme_bw()


save.image("RData/06.3_linear-models-3.0_final.RData")
