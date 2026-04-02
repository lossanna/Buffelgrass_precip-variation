# Created: 2026-03-30
# Updated: 2026-03-30

# Purpose: Write script for finalized published model results (revision1.3).

# Identical to 15.1_linear-models-revision1.3.R.

library(tidyverse)
library(performance)
library(DHARMa)
library(lme4)
library(lmerTest)
library(modelbased)

# Load data ---------------------------------------------------------------

culm.change.raw <- read_csv("data/publish2.3/culm-data.csv")
plot.change.raw <- read_csv("data/publish2.3/plot-data.csv")
survival.dat.raw <- read_csv("data/publish2.3/survival-data.csv")

# Data wrangling ----------------------------------------------------------

# Center and scale numeric variables for culm change data
culm.change <- culm.change.raw %>% 
  mutate(PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         Change_BGDensity_scaled = scale(Change_BGDensity, scale = TRUE)[, 1],
         Change_ShrubCover_scaled = scale(Change_ShrubCover, scale = TRUE)[, 1],
         Change_HerbCover_scaled = scale(Change_HerbCover, scale = TRUE)[, 1],
         Change_BGCover_scaled = scale(Change_BGCover, scale = TRUE)[, 1],
         Init_BGDensity_scaled = scale(Init_BGDensity, scale = TRUE)[, 1],
         Init_ShrubCover_scaled = scale(Init_ShrubCover, scale = TRUE)[, 1],
         Init_HerbCover_scaled = scale(Init_HerbCover, scale = TRUE)[, 1],
         Init_BGCover_scaled = scale(Init_BGCover, scale = TRUE)[, 1])

# Center and scale numeric variables for plot-level data
plot.change <- plot.change.raw %>% 
  mutate(PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         Change_ShrubCover_scaled = scale(Change_ShrubCover, scale = TRUE)[, 1],
         Change_HerbCover_scaled = scale(Change_HerbCover, scale = TRUE)[, 1],
         Change_BGCover_scaled = scale(Change_BGCover, scale = TRUE)[, 1],
         Init_BGDensity_scaled = scale(Init_BGDensity, scale = TRUE)[, 1],
         Init_ShrubCover_scaled = scale(Init_ShrubCover, scale = TRUE)[, 1],
         Init_HerbCover_scaled = scale(Init_HerbCover, scale = TRUE)[, 1],
         Init_BGCover_scaled = scale(Init_BGCover, scale = TRUE)[, 1])


# Center and scale numeric variables for survival data
dat.survival <- survival.dat.raw %>% 
  mutate(Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         ShrubCover_scaled = scale(ShrubCover, center = TRUE, scale = TRUE)[, 1],
         HerbCover_scaled = scale(HerbCover, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         BGDensity_scaled = scale(BGDensity, center = TRUE, scale = TRUE)[, 1])



# Total culm change -------------------------------------------------------

# Total culm change
total <- lmer(Change_TotalCulms ~ Prev_year_precip_scaled +  
                 PlotSlope_scaled + Change_BGDensity_scaled +
                 Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 Change_ShrubCover_scaled * PlotSlope_scaled +
                 Change_HerbCover_scaled * PlotSlope_scaled +
                 (1 | Site / Transect / Plot / Plant_ID),
               data = culm.change)
summary(total)
r2(total) # marginal: 0.137; conditional: 0.466
res.total <- simulateResiduals(total)
plotQQunif(res.total)
plotResiduals(res.total) 

#   Predicted vs. observed 
total.pred <- estimate_expectation(total)
total.pred$Change_TotalCulms <- culm.change$Change_TotalCulms

total.pred.plot <- total.pred %>% 
  ggplot(aes(x = Change_TotalCulms, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  ggtitle("Total culm change model, predicted vs. observed") +
  xlab(expression(Delta ~ "Total live culms [observed]")) +
  theme_bw()
total.pred.plot



# Reproductive culm change ------------------------------------------------

# Repro culm
repro <- lmer(Change_ReproductiveCulms ~ Prev_year_precip_scaled +  
                 PlotSlope_scaled + Change_BGDensity_scaled +
                 Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 Change_ShrubCover_scaled * PlotSlope_scaled +
                 Change_HerbCover_scaled * PlotSlope_scaled +
                 (1 | Site / Transect / Plot),
               data = culm.change)
summary(repro)
r2(repro) # marginal: 0.085; conditional: 0.291
res.repro <- simulateResiduals(repro)
plotQQunif(res.repro)
plotResiduals(res.repro) 

#   Predicted vs. observed 
repro.pred <- estimate_expectation(repro)
repro.pred$Change_ReproductiveCulms <- culm.change$Change_ReproductiveCulms

repro.pred.plot <- repro.pred %>% 
  ggplot(aes(x = Change_ReproductiveCulms, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  ggtitle("Reproductive culm change model, predicted vs. observed") +
  xlab(expression(Delta ~ "Reproductive culms [observed]")) +
  theme_bw()
repro.pred.plot



# BG density change -------------------------------------------------------

# BG density
bgden <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + 
                 PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 Change_ShrubCover_scaled * PlotSlope_scaled +
                 Change_HerbCover_scaled * PlotSlope_scaled +
                 (1 | Site / Transect),
               data = plot.change)
summary(bgden)
r2(bgden) # marginal: 0.378; conditional: 0.456
res.bgden <- simulateResiduals(bgden)
plotQQunif(res.bgden)
plotResiduals(res.bgden) 
check_collinearity(bgden)

#   Predicted vs. observed
bgden.pred <- estimate_expectation(bgden)
bgden.pred$Change_BGDensity <- plot.change$Change_BGDensity

bgden.pred.plot <- bgden.pred %>% 
  ggplot(aes(x = Change_BGDensity, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  ggtitle("Density change model, predicted vs. observed") +
  xlab(expression(Delta ~ paste("Buffelgrass density (individuals / ", m^2, ") [observed]"))) +
  theme_bw()
bgden.pred.plot



# BG cover change ---------------------------------------------------------

# BG cover change
bgcov <- lmer(Change_BGCover ~ Prev_year_precip_scaled + 
                 PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGCover_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 Change_ShrubCover_scaled * PlotSlope_scaled +
                 Change_HerbCover_scaled * PlotSlope_scaled +
                 (1 | Site),
               data = plot.change)
summary(bgcov)
r2(bgcov) # marginal: 0.298; conditional: 0.306
res.bgcov <- simulateResiduals(bgcov)
plotQQunif(res.bgcov)
plotResiduals(res.bgcov) 

#   Predicted vs. observed 
bgcov.pred <- estimate_expectation(bgcov)
bgcov.pred$Change_BGCover <- plot.change$Change_BGCover

bgcov.pred.plot <- bgcov.pred %>% 
  ggplot(aes(x = Change_BGCover, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  ggtitle("Cover change model, predicted vs. observed") +
  xlab(expression(Delta ~ "Buffelgrass cover (%) [observed]")) +
  theme_bw()
bgcov.pred.plot



# Survival ----------------------------------------------------------------

# Survival
survival <- glmer(cbind(seedlings_surviving,
                         remaining_toothpicks - seedlings_surviving) ~ Prev_year_precip_scaled + 
                     PlotSlope_scaled + BGDensity_scaled +
                     ShrubCover_scaled + HerbCover_scaled + 
                     Prev_year_precip_scaled * BGDensity_scaled + 
                     Prev_year_precip_scaled * ShrubCover_scaled +
                     Prev_year_precip_scaled * HerbCover_scaled +
                     ShrubCover_scaled * PlotSlope_scaled +
                     HerbCover_scaled * PlotSlope_scaled +
                     (1 | Site / Transect / Plot),
                   family = binomial,
                   data = dat.survival)
summary(survival)
r2(survival) # marginal: 0.492; conditional: 0.963
res.survival <- simulateResiduals(survival)
plotQQunif(res.survival)
plotResiduals(res.survival) 
check_collinearity(survival)

#   Predicted vs. observed 
survival.pred <- estimate_expectation(survival)
survival.pred$survival_prop <- dat.survival$survival_prop

survival.pred.plot <- survival.pred %>% 
  ggplot(aes(x = survival_prop, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  ggtitle("Seedling survival model, predicted vs. observed")  +
  xlab("Proportion of surviving seedlings [observed]") +
  theme_bw() 
survival.pred.plot




# Simple linear regression ------------------------------------------------

# Shrub vs. precip
summary(lm(Change_ShrubCover ~ Prev_year_precip, data = plot.change))
summary(lm(Change_ShrubCover ~ Prev_year_precip_scaled, data = plot.change))

# Herb vs. precip
summary(lm(Change_HerbCover ~ Prev_year_precip, data = plot.change))
summary(lm(Change_HerbCover ~ Prev_year_precip_scaled, data = plot.change))

# Total vs density
summary(lm(Change_TotalCulms ~ Change_BGDensity, data = culm.change))
summary(lm(Change_TotalCulms ~ Change_BGDensity_scaled, data = culm.change))

# Total vs cover
summary(lm(Change_TotalCulms ~ Change_BGCover, data = culm.change))
summary(lm(Change_TotalCulms ~ Change_BGCover_scaled, data = culm.change))

# Density vs cover
summary(lm(Change_BGDensity ~ Change_BGCover, data = culm.change))
summary(lm(Change_BGDensity ~ Change_BGCover_scaled, data = culm.change))



# Save --------------------------------------------------------------------

# Needed for graphs
save(culm.change, plot.change, dat.survival, 
     total, repro, bgden, bgcov, survival,
     file = "RData/publish2.3_data-and-models.RData")


save.image("RData/publish2.3_linear-models.RData")
