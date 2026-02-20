# Created: 2026-02-19
# Updated: 2026-02-19

# Purpose: Write script for finalized published model results (revision1.2).

# Identical to 12.1_linear-models-revision1.2.R.

library(tidyverse)
library(performance)
library(DHARMa)
library(lme4)
library(lmerTest)
library(modelbased)

# Load data ---------------------------------------------------------------

culm.change.raw <- read_csv("data/publish2.2/culm-data.csv")
plot.change.raw <- read_csv("data/publish2.2/plot-data.csv")
survival.dat.raw <- read_csv("data/publish2.2/survival-data.csv")

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
plot.change <- culm.change.raw %>% 
  select(-Plant_ID, -Change_ReproductiveCulms, -Change_TotalCulms) %>% 
  distinct(.keep_all = TRUE) %>% 
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
         BGDensity_scaled = scale(BGDensity, center = TRUE, scale = TRUE)[, 1]) %>% 
  mutate(survival_prop = seedlings_surviving / remaining_toothpicks)


# Total culm change -------------------------------------------------------

# Initial conditions as numeric, nested Site/Transect
total <- lmer(Change_TotalCulms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_BGDensity_scaled +
                 Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 (1 | Site / Transect),
               data = culm.change)
summary(total)
r2(total) # marginal: 0.128; conditional: 0.416
res.total <- simulateResiduals(total)
plotQQunif(res.total)
plotResiduals(res.total) 
check_collinearity(total)

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

# Version 1: Initial conditions as numeric, nested Site/Transect
repro1 <- lmer(Change_ReproductiveCulms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_BGDensity_scaled +
                 Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 (1 | Site / Transect),
               data = culm.change)
summary(repro1)
r2(repro1) # marginal: 0.100; conditional: 0.305
res.repro1 <- simulateResiduals(repro1)
plotQQunif(res.repro1)
plotResiduals(res.repro1) 
check_collinearity(repro1)

#   Predicted vs. observed 
repro1.pred <- estimate_expectation(repro1)
repro1.pred$Change_ReproductiveCulms <- culm.change$Change_ReproductiveCulms

repro1.pred.plot <- repro1.pred %>% 
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
repro1.pred.plot


# Version 2: Initial conditions as numeric, nested Site/Transect/Plant_ID
repro2 <- lmer(Change_ReproductiveCulms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_BGDensity_scaled +
                 Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 (1 | Site / Transect / Plant_ID),
               data = culm.change)
summary(repro2) # singular fit issues
r2(repro2) # marginal: 0.125; conditional: can't calculate
res.repro2 <- simulateResiduals(repro2)
plotQQunif(res.repro2)
plotResiduals(res.repro2) 



# BG density change -------------------------------------------------------

# Version 1: Initial conditions as numeric, nested Site/Transect
bgden1 <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + 
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 (1 | Site / Transect),
               data = plot.change)
summary(bgden1)
r2(bgden1) # marginal: 0.422; conditional: 0.468
res.bgden1 <- simulateResiduals(bgden1)
plotQQunif(res.bgden1)
plotResiduals(res.bgden1) 
check_collinearity(bgden1)

#   Predicted vs. observed
bgden1.pred <- estimate_expectation(bgden1)
bgden1.pred$Change_BGDensity <- plot.change$Change_BGDensity

bgden1.pred.plot <- bgden1.pred %>% 
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
bgden1.pred.plot


# Version 2: Initial conditions as numeric, nested Site/Transect/Plot
bgden2 <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + 
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 (1 | Site / Transect / Plot),
               data = plot.change)
summary(bgden2) # singular fit issues
r2(bgden2) # marginal: 0.443; conditional: can't compute
res.bgden2 <- simulateResiduals(bgden2)
plotQQunif(res.bgden2)
plotResiduals(res.bgden2)



# BG cover change ---------------------------------------------------------

# Version 1: Initial conditions as numeric, nested Site/Transect
bgcov1 <- lmer(Change_BGCover ~ Prev_year_precip_scaled + 
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 (1 | Site / Transect),
               data = plot.change)
summary(bgcov1)
r2(bgcov1) # marginal: 0.261; conditional: 0.285
res.bgcov1 <- simulateResiduals(bgcov1)
plotQQunif(res.bgcov1)
plotResiduals(res.bgcov1) 

#   Predicted vs. observed 
bgcov1.pred <- estimate_expectation(bgcov1)
bgcov1.pred$Change_BGCover <- plot.change$Change_BGCover

bgcov1.pred.plot <- bgcov1.pred %>% 
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
bgcov1.pred.plot


# Version 2: Initial conditions as numeric, nested Site/Transect/Plot
bgcov2 <- lmer(Change_BGCover ~ Prev_year_precip_scaled + 
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 (1 | Site / Transect / Plot),
               data = plot.change)
summary(bgcov2) # singular fit issues
r2(bgcov2) # marginal: 0.268; conditional: can't calculate
res.bgcov2 <- simulateResiduals(bgcov2)
plotQQunif(res.bgcov2)
plotResiduals(res.bgcov2) 



# Survival ----------------------------------------------------------------

# Version 1: Initial conditions as numeric, nested Site/Transect
survival1 <- glmer(cbind(seedlings_surviving,
                         remaining_toothpicks - seedlings_surviving) ~ Prev_year_precip_scaled + 
                     Aspect + PlotSlope_scaled + BGDensity_scaled +
                     ShrubCover_scaled + HerbCover_scaled + 
                     Prev_year_precip_scaled * BGDensity_scaled + 
                     Prev_year_precip_scaled * ShrubCover_scaled +
                     Prev_year_precip_scaled * HerbCover_scaled +
                     (1 | Site / Transect),
                   family = binomial,
                   data = dat.survival)
summary(survival1)
r2(survival1) # marginal: 0.493; conditional: 0.966
res.survival1 <- simulateResiduals(survival1)
plotQQunif(res.survival1)
plotResiduals(res.survival1) 
check_collinearity(survival1)

#   Predicted vs. observed 
survival1.pred <- estimate_expectation(survival1)
survival1.pred$survival_prop <- dat.survival$survival_prop

survival1.pred.plot <- survival1.pred %>% 
  ggplot(aes(x = survival_prop, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  ggtitle("Seedling survival model, predicted vs. observed")  +
  xlab("Proportion of surviving seedlings [observed]") +
  theme_bw() 
survival1.pred.plot


# Version 2: Initial conditions as numeric, nested Site/Transect/Plot
survival2 <- glmer(cbind(seedlings_surviving,
                         remaining_toothpicks - seedlings_surviving) ~ Prev_year_precip_scaled + 
                     Aspect + PlotSlope_scaled + BGDensity_scaled +
                     ShrubCover_scaled + HerbCover_scaled + 
                     Prev_year_precip_scaled * BGDensity_scaled + 
                     Prev_year_precip_scaled * ShrubCover_scaled +
                     Prev_year_precip_scaled * HerbCover_scaled +
                     (1 | Site / Transect / Plot),
                   family = binomial,
                   data = dat.survival) # convergence issues
summary(survival2)
r2(survival2) # marginal: 0.488; conditional: 0.967
res.survival2 <- simulateResiduals(survival2)
plotQQunif(res.survival2)
plotResiduals(res.survival2) 


# Write out predicted vs. observed graphs ---------------------------------

# Total change
tiff("figures/2026-02_draft-figures-revision1.2/Total-change_predicted-vs-observed.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.pred.plot
dev.off()

# Repro change
tiff("figures/2026-02_draft-figures-revision1.2/Repro-change_predicted-vs-observed.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro1.pred.plot
dev.off()

# BG density change
tiff("figures/2026-02_draft-figures-revision1.2/BG-density-change_predicted-vs-observed.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden1.pred.plot
dev.off()

# BG cover change
tiff("figures/2026-02_draft-figures-revision1.2/BG-cover-change_predicted-vs-observed.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov1.pred.plot
dev.off()

# Survival
tiff("figures/2026-02_draft-figures-revision1.2/Survival_predicted-vs-observed.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival1.pred.plot
dev.off()


# Save --------------------------------------------------------------------

# Needed for graphs
save(culm.change, plot.change, dat.survival, 
     total, repro1, bgden1, bgcov1, survival1,
     file = "RData/12.1_data-and-models-revision1.2.RData")


save.image("RData/12.1_linear-models-revision1.2.RData")
