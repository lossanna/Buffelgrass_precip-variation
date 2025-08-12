# Created: 2025-08-12
# Updated: 2025-08-12

# Purpose: Compile version 6.3 linear models with Change_Reproductive_culms, Change_Total_Live_Culms, 
#  Change_BGDensity, Change_BGCover, and survival_transf as response variable.

# Continuous explanatory variables are centered and scaled. 
# lme4 package used for all models except survival to use REML; survival modeled as 
#   beta regression with glmmTMB package.

# Version 3 from 06.7_linear-models-6.0.R uses (1 | Transect) as random effect. Will
#   be applying this to all models.

# Wrote out predicted vs. observed graphs (made with modelbased) for all five models.

# Differs from linear models v6.3 because:
#   1. "flat" observations were removed before centering and scaling variables for culm change models.
#   2. Centering and scaling happened for plot-level data specifically, rather than using
#        what had been centered and scaled from the culm change data.
#   3. Centering and scaling happened for survival data after replicate culm-level rows were removed.


library(tidyverse)
library(glmmTMB)
library(performance)
library(DHARMa)
library(lme4)
library(lmerTest)
library(MuMIn)
library(modelbased)

# Load data ---------------------------------------------------------------

dat <- read_csv("data/cleaned/04_demography-data_clean.csv")
culm.change.raw <- read_csv("data/cleaned/04_change-in-culm-density-cover_clean.csv")

# Data wrangling ----------------------------------------------------------

# Center and scale numeric variables
culm.change.flat.rm <- culm.change.raw %>% 
  filter(Aspect != "flat") %>% 
  mutate(PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         Change_BGDensity_scaled = scale(Change_BGDensity, scale = TRUE)[, 1],
         Change_ShrubCover_scaled = scale(Change_ShrubCover, scale = TRUE)[, 1],
         Change_HerbCover_scaled = scale(Change_HerbCover, scale = TRUE)[, 1])

# Separate out plot-level data
plot.change <- culm.change.raw %>% 
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm,
         -Change_Reproductive_culms, -Change_Total_Live_Culms, -Notes) %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(Aspect != "flat") %>% 
  mutate(PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         Change_ShrubCover_scaled = scale(Change_ShrubCover, scale = TRUE)[, 1],
         Change_HerbCover_scaled = scale(Change_HerbCover, scale = TRUE)[, 1])


# Prepare survival data
dat.survival <- dat %>% 
  filter(!is.na(survival_perc),
         Aspect != "flat") %>% 
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm,
         -Notes) %>% 
  distinct(.keep_all = TRUE)

dat.survival <- dat.survival %>% 
  mutate(Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         ShrubCover_scaled = scale(ShrubCover, center = TRUE, scale = TRUE)[, 1],
         HerbCover_scaled = scale(HerbCover, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         BGDensity_scaled = scale(BGDensity, center = TRUE, scale = TRUE)[, 1])

#   Transform 0s and 1s
dat.survival <- dat.survival %>% 
  mutate(survival_transf = pmin(pmax(survival_perc, 1e-6), 1 - 1e-6))



# Total culm change -------------------------------------------------------

# Global model
total <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                Change_BGDensity_scaled +
                Prev_year_precip_scaled * Change_BGDensity_scaled +
                Prev_year_precip_scaled * Change_ShrubCover_scaled +
                Prev_year_precip_scaled * Change_HerbCover_scaled +
                (1 | Transect),
              data = culm.change.flat.rm)

# Model selection
options(na.action = "na.fail")
total_set <- dredge(total) 

# Examine best model
total_best.model <- get.models(total_set, 1)[[1]]
summary(total_best.model)
r2(total_best.model) # marginal: 0.144; conditional: 0.391
res.total_best.model <- simulateResiduals(total_best.model)
plotQQunif(res.total_best.model)
plotResiduals(res.total_best.model) # mostly fine
check_model(total_best.model) # posterior prediction is poor around 0

# Examine models within 2 AICc units of best and assign each top model to separate object
total_top <- subset(total_set, delta <= 2) # 2 models
for (i in 1:nrow(total_top)) {
  assign(paste0("total_model", i), get.models(total_top, subset = i)[[1]])
} 

# R^2 of top models
r2(total_model1) # marginal: 0.144; conditional: 0.391
r2(total_model2) # marginal: 0.147; conditional: 0.386

# Model averaging of top models
total_avg <- model.avg(total_set, subset = delta <= 2)
summary(total_avg)

# Predicted vs. observed (best model)
total_pred <- estimate_expectation(total_best.model)
total_pred$Change_Total_Live_Culms <- culm.change.flat.rm$Change_Total_Live_Culms

total_pred.plot <- total_pred %>% 
  ggplot(aes(x = Change_Total_Live_Culms, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  ggtitle("Total culm change model, predicted vs. observed (best model)") +
  xlab(expression(Delta ~ "Total live culms [observed]")) +
  theme_bw()
total_pred.plot



# Reproductive culm change ------------------------------------------------

# Global model
repro <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                Change_BGDensity_scaled +
                Prev_year_precip_scaled * Change_BGDensity_scaled +
                Prev_year_precip_scaled * Change_ShrubCover_scaled +
                Prev_year_precip_scaled * Change_HerbCover_scaled +
                (1 | Transect),
              data = culm.change.flat.rm)

# Model selection
options(na.action = "na.fail")
repro_set <- dredge(repro) 

# Examine best model
repro_best.model <- get.models(repro_set, 1)[[1]]
summary(repro_best.model)
r2(repro_best.model) # marginal: 0.155; conditional: 0.320
res.repro_best.model <- simulateResiduals(repro_best.model)
plotQQunif(res.repro_best.model)
plotResiduals(res.repro_best.model) # a bit janky
check_model(repro_best.model) # posterior prediction is poor at 0

# Examine models within 2 AICc units of best and assign each top model to separate object
repro_top <- subset(repro_set, delta <= 2) # 5 models
for (i in 1:nrow(repro_top)) {
  assign(paste0("repro_model", i), get.models(repro_top, subset = i)[[1]])
} 

# R^2 of top models
r2(repro_model1) # marginal: 0.155; conditional: 0.320
r2(repro_model2) # marginal: 0.151; conditional: 0.322
r2(repro_model3) # marginal: 0.155; conditional: 0.318
r2(repro_model4) # marginal: 0.151; conditional: 0.321
r2(repro_model5) # marginal: 0.147; conditional: 0.333

# Model averaging of top models
repro_avg <- model.avg(repro_set, subset = delta <= 2)
summary(repro_avg)

# Predicted vs. observed (best model)
repro_pred <- estimate_expectation(repro_best.model)
repro_pred$Change_Reproductive_culms <- culm.change.flat.rm$Change_Reproductive_culms

repro_pred.plot <- repro_pred %>% 
  ggplot(aes(x = Change_Reproductive_culms, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  ggtitle("Reproductive culm change model, predicted vs. observed (best model)") +
  xlab(expression(Delta ~ "Reproductive culms [observed]")) +
  theme_bw()
repro_pred.plot



# Buffelgrass density change ----------------------------------------------

# Global model
bgden <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + 
                Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                Change_HerbCover_scaled + 
                Prev_year_precip_scaled * Change_ShrubCover_scaled +
                Prev_year_precip_scaled * Change_HerbCover_scaled +
                (1 | Transect),
              data = plot.change)

# Model selection
options(na.action = "na.fail")
bgden_set <- dredge(bgden) # no singularity issues

# Examine best model
bgden_best.model <- get.models(bgden_set, 1)[[1]]
summary(bgden_best.model)
r2(bgden_best.model) # marginal: 0.364; conditional: 0.456
res.bgden_best.model <- simulateResiduals(bgden_best.model)
plotQQunif(res.bgden_best.model)
plotResiduals(res.bgden_best.model) # mostly fine
check_model(bgden_best.model) # posterior prediction is poor around 0-1

# Examine models within 2 AICc units of best and assign each top model to separate object
bgden_top <- subset(bgden_set, delta <= 2) # 5 models
for (i in 1:nrow(bgden_top)) {
  assign(paste0("bgden_model", i), get.models(bgden_top, subset = i)[[1]])
} 

# R^2 of top models
r2(bgden_model1) # marginal: 0.364; conditional: 0.456
r2(bgden_model2) # marginal: 0.375; conditional: 0.440
r2(bgden_model3) # marginal: 0.389; conditional: 0.423
r2(bgden_model4) # marginal: 0.360; conditional: 0.458
r2(bgden_model5) # marginal: 0.370; conditional: 0.443

# Model averaging of top models
bgden_avg <- model.avg(bgden_set, subset = delta <= 2)
summary(bgden_avg)

# Predicted vs. observed (best model)
bgden_pred <- estimate_expectation(bgden_best.model)
bgden_pred$Change_BGDensity <- plot.change$Change_BGDensity

bgden_pred.plot <- bgden_pred %>% 
  ggplot(aes(x = Change_BGDensity, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  ggtitle("Plot density change model, predicted vs. observed (best model)") +
  xlab(expression(Delta ~ paste("Buffelgrass density (individuals / ", m^2, ") [observed]"))) +
  theme_bw()
bgden_pred.plot



# Buffelgrass cover change ------------------------------------------------

# Global model
bgcov <- lmer(Change_BGCover ~ Prev_year_precip_scaled + 
                Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                Change_HerbCover_scaled + 
                Prev_year_precip_scaled * Change_ShrubCover_scaled +
                Prev_year_precip_scaled * Change_HerbCover_scaled +
                (1 | Transect),
              data = plot.change)

# Model selection
options(na.action = "na.fail")
bgcov_set <- dredge(bgcov) # some models had trouble with random effects; got warning: boundary (singular) fit: see help('isSingular')

# Examine best model
bgcov_best.model <- get.models(bgcov_set, 1)[[1]]
summary(bgcov_best.model)
r2(bgcov_best.model) # marginal: 0.243; conditional: 0.284
res.bgcov_best.model <- simulateResiduals(bgcov_best.model)
plotQQunif(res.bgcov_best.model)
plotResiduals(res.bgcov_best.model) # all lines are black
check_model(bgcov_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
bgcov_top <- subset(bgcov_set, delta <= 2) # 6 models
for (i in 1:nrow(bgcov_top)) {
  assign(paste0("bgcov_model", i), get.models(bgcov_top, subset = i)[[1]])
} 

# R^2 of top models
r2(bgcov_model1) # marginal: 0.243; conditional: 0.284
r2(bgcov_model2) # marginal: 0.245; conditional: 0.285
r2(bgcov_model3) # marginal: 0.244; conditional: 0.287
r2(bgcov_model4) # marginal: 0.246; conditional: 0.288
r2(bgcov_model5) # marginal: 0.245; conditional: 0.286
r2(bgcov_model6) # marginal: 0.246; conditional: 0.289

# Check singularity of top models
bgcov_top.models <- get.models(bgcov_set, subset = delta <= 2)
lapply(bgcov_top.models, isSingular) # no top models have issues

# Model averaging of top models
bgcov_avg <- model.avg(bgcov_set, subset = delta <= 2)
summary(bgcov_avg)

# Predicted vs. observed (best model)
bgcov_pred <- estimate_expectation(bgcov_best.model)
bgcov_pred$Change_BGCover <- plot.change$Change_BGCover

bgcov_pred.plot <- bgcov_pred %>% 
  ggplot(aes(x = Change_BGCover, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  ggtitle("Plot cover change model, predicted vs. observed (best model)") +
  xlab(expression(Delta ~ "Buffelgrass cover (%) [observed]")) +
  theme_bw()
bgcov_pred.plot



# Survival ----------------------------------------------------------------

# Global model
survival <- glmmTMB(survival_transf ~ Prev_year_precip_scaled +
                      Aspect + PlotSlope_scaled + ShrubCover_scaled +
                      HerbCover_scaled + BGDensity_scaled +
                      Prev_year_precip_scaled * ShrubCover_scaled +
                      Prev_year_precip_scaled * HerbCover_scaled +
                      Prev_year_precip_scaled * BGDensity_scaled +
                      (1 | Transect),
                    data = dat.survival,
                    family = beta_family(link = "logit"))

# Model selection
options(na.action = "na.fail")
survival_set <- dredge(survival) 

# Examine best model
survival_best.model <- get.models(survival_set, 1)[[1]]
summary(survival_best.model)
r2(survival_best.model) # marginal: 0.688; conditional: 0.793
res.survival_best.model <- simulateResiduals(survival_best.model)
plotQQunif(res.survival_best.model)
plotResiduals(res.survival_best.model) # also kind of janky
check_model(survival_best.model)

# Examine models within 2 AICc units of best and assign each top model to separate object
survival_top <- subset(survival_set, delta <= 2) # 15 models
for (i in 1:nrow(survival_top)) {
  assign(paste0("survival_model", i), get.models(survival_top, subset = i)[[1]])
} 

# R^2 of top models
r2(survival_model1) # marginal: 0.688; conditional: 0.793
r2(survival_model2) # marginal: 0.683; conditional: 0.788
r2(survival_model3) # marginal: 0.677; conditional: 0.793
r2(survival_model4) # marginal: 0.666; conditional: 0.811
r2(survival_model5) # marginal: 0.690; conditional: 0.781
r2(survival_model6) # marginal: 0.683; conditional: 0.798
r2(survival_model7) # marginal: 0.675; conditional: 0.798
r2(survival_model8) # marginal: 0.692; conditional: 0.779
r2(survival_model9) # marginal: 0.676; conditional: 0.790
r2(survival_model10) # marginal: 0.688; conditional: 0.788
r2(survival_model11) # marginal: 0.695; conditional: 0.785
r2(survival_model12) # marginal: 0.693; conditional: 0.786
r2(survival_model13) # marginal: 0.684; conditional: 0.789
r2(survival_model14) # marginal: 0.674; conditional: 0.802
r2(survival_model15) # marginal: 0.691; conditional: 0.790

# Model averaging of top models
survival_avg <- model.avg(survival_set, subset = delta <= 2)
summary(survival_avg)

# Predicted vs. observed (best model)
survival_pred <- estimate_expectation(survival_best.model)
survival_pred$survival_transf <- dat.survival$survival_transf

survival_pred.plot <- survival_pred %>% 
  ggplot(aes(x = survival_transf, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  ggtitle("Seedling survival model, predicted vs. observed (best model)")  +
  xlab("Buffelgrass seedling survival (%) [observed]") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)
survival_pred.plot



# Write out predicted vs. observed graphs ---------------------------------

# Total change
tiff("figures/2025-08_draft-figures/Total-change_predicted-vs-observed.tiff",
     units = "in", height = 4, width = 6, res = 150)
total_pred.plot
dev.off()

# Repro change
tiff("figures/2025-08_draft-figures/Repro-change_predicted-vs-observed.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro_pred.plot
dev.off()

# BG density change
tiff("figures/2025-08_draft-figures/BG-density-change_predicted-vs-observed.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden_pred.plot
dev.off()

# BG cover change
tiff("figures/2025-08_draft-figures/BG-cover-change_predicted-vs-observed.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov_pred.plot
dev.off()

# Survival
tiff("figures/2025-08_draft-figures/Survival_predicted-vs-observed.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival_pred.plot
dev.off()


# Save --------------------------------------------------------------------

# Needed for graphs
save(culm.change.flat.rm, plot.change, dat.survival,
     total_best.model, 
     repro_best.model, repro_model3, 
     bgden_best.model, bgden_model2, bgden_model3, bgden_model4,
     bgcov_best.model,
     survival_best.model, survival_model5, survival_model6,
     file = "RData/06.9_data-and-best-models-6.3.1.RData")


save.image("RData/06.9_linear-models-6.3.1.RData")
