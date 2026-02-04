# Created: 2026-02-03
# Updated: 2026-02-03

# Purpose: Write script for finalized published model results (revision 1).

# Identical to 09.2_linear-models-revision1.R.


library(tidyverse)
library(glmmTMB)
library(performance)
library(DHARMa)
library(lme4)
library(lmerTest)
library(modelbased)

# Load data ---------------------------------------------------------------

culm.change.raw <- read_csv("data/publish/culm-data.csv")
plot.change.raw <- read_csv("data/publish/plot-data.csv")
survival.dat.raw <- read_csv("data/publish/survival-data.csv")

# Data wrangling ----------------------------------------------------------

# Center and scale numeric variables
culm.change <- culm.change.raw %>% 
  mutate(PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         Change_BGDensity_scaled = scale(Change_BGDensity, scale = TRUE)[, 1],
         Change_ShrubCover_scaled = scale(Change_ShrubCover, scale = TRUE)[, 1],
         Change_HerbCover_scaled = scale(Change_HerbCover, scale = TRUE)[, 1])

plot.change <- plot.change.raw %>% 
  mutate(PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         Change_ShrubCover_scaled = scale(Change_ShrubCover, scale = TRUE)[, 1],
         Change_HerbCover_scaled = scale(Change_HerbCover, scale = TRUE)[, 1])

survival.dat <- survival.dat.raw %>% 
  mutate(Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         ShrubCover_scaled = scale(ShrubCover, center = TRUE, scale = TRUE)[, 1],
         HerbCover_scaled = scale(HerbCover, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         BGDensity_scaled = scale(BGDensity, center = TRUE, scale = TRUE)[, 1])


# Transform 0s and 1s for survival data to accommodate beta regression
survival.dat <- survival.dat %>% 
  mutate(Survival_transf = pmin(pmax(Survival_perc, 1e-6), 1 - 1e-6))



# Total culm change -------------------------------------------------------

# Model
total <- lmer(Change_TotalCulms ~ Prev_year_precip_scaled +  
                Aspect + PlotSlope_scaled + Change_BGDensity_scaled +
                Change_ShrubCover_scaled + Change_HerbCover_scaled +
                Prev_year_precip_scaled * Change_BGDensity_scaled +
                Prev_year_precip_scaled * Change_ShrubCover_scaled +
                Prev_year_precip_scaled * Change_HerbCover_scaled +
                (1 | Site / Transect),
              data = culm.change)
summary(total)
r2(total) # marginal: 0.119; conditional: 0.410
res.total <- simulateResiduals(total)
plotQQunif(res.total)
plotResiduals(res.total) 

# Predicted vs. observed
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

# Model
repro <- lmer(Change_ReproductiveCulms ~ Prev_year_precip_scaled +  
                Aspect + PlotSlope_scaled + Change_BGDensity_scaled +
                Change_ShrubCover_scaled + Change_HerbCover_scaled +
                Prev_year_precip_scaled * Change_BGDensity_scaled +
                Prev_year_precip_scaled * Change_ShrubCover_scaled +
                Prev_year_precip_scaled * Change_HerbCover_scaled +
                (1 | Site / Transect),
              data = culm.change)
summary(repro)
r2(repro) # marginal: 0.098; conditional: 0.295
res.repro <- simulateResiduals(repro)
plotQQunif(res.repro)
plotResiduals(res.repro) 

# Predicted vs. observed
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



# Buffelgrass density change ----------------------------------------------

# Model
bgden <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + 
                Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                Change_HerbCover_scaled + 
                Prev_year_precip_scaled * Change_ShrubCover_scaled +
                Prev_year_precip_scaled * Change_HerbCover_scaled +
                (1 | Site / Transect),
              data = plot.change)
summary(bgden)
r2(bgden) # marginal: 0.381; conditional: 0.427
res.bgden <- simulateResiduals(bgden)
plotQQunif(res.bgden)
plotResiduals(res.bgden) 

# Predicted vs. observed
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
  ggtitle("Plot density change model, predicted vs. observed") +
  xlab(expression(Delta ~ paste("Buffelgrass density (individuals / ", m^2, ") [observed]"))) +
  theme_bw()
bgden.pred.plot



# Buffelgrass cover change ------------------------------------------------

# Model
bgcov <- lmer(Change_BGCover ~ Prev_year_precip_scaled + 
                Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                Change_HerbCover_scaled + 
                Prev_year_precip_scaled * Change_ShrubCover_scaled +
                Prev_year_precip_scaled * Change_HerbCover_scaled +
                (1 | Site / Transect),
              data = plot.change)
summary(bgcov)
r2(bgcov) # marginal: 0.248; conditional: 0.295
res.bgcov <- simulateResiduals(bgcov)
plotQQunif(res.bgcov)
plotResiduals(res.bgcov)

# Predicted vs. observed
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
  ggtitle("Plot cover change model, predicted vs. observed") +
  xlab(expression(Delta ~ "Buffelgrass cover (%) [observed]")) +
  theme_bw()
bgcov.pred.plot



# Survival ----------------------------------------------------------------

# Model
survival <- glmmTMB(Survival_transf ~ Prev_year_precip_scaled +
                      Aspect + PlotSlope_scaled + BGDensity_scaled + 
                      ShrubCover_scaled + HerbCover_scaled + 
                      Prev_year_precip_scaled * BGDensity_scaled +
                      Prev_year_precip_scaled * ShrubCover_scaled +
                      Prev_year_precip_scaled * HerbCover_scaled +
                      (1 | Site / Transect),
                    data = survival.dat,
                    family = beta_family(link = "logit"))
summary(survival)
r2(survival) # marginal: 0.819; conditional: can't compute
res.survival <- simulateResiduals(survival)
plotQQunif(res.survival)
plotResiduals(res.survival) 

# Predicted vs. observed
survival.pred <- estimate_expectation(survival)
survival.pred$Survival_transf <- survival.dat$Survival_transf

survival.pred.plot <- survival.pred %>% 
  ggplot(aes(x = Survival_transf, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  ggtitle("Seedling survival model, predicted vs. observed")  +
  xlab("Buffelgrass seedling survival (%) [observed]") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)
survival.pred.plot



# Save --------------------------------------------------------------------

# Needed for graphs
save(culm.change, plot.change, survival.dat,
     total, repro, bgden, bgcov, survival, 
     file = "RData/publish2_data-and-models.RData")


save.image("RData/publish2_linear-models.RData")
