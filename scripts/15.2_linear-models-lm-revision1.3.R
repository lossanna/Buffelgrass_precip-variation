# Created: 2026-02-04
# Updated: 2026-04-02

# Purpose: Apply v1.2 updates and v1.3 updates (decided in 15.1.R)
#   v1.2: Include initial BG density, shrub cover, and herb cover values as explanatory variables.
#   v1.3: Remove Aspect, include slope * shrub/herb interactions, correct random effects,
#         change Init_BGDensity to Init_BGCover for cover change model.

# Random effects now vary across model to better capture study setup.
#   Maximum random structure is attempted first; levels with 0 variance are 
#   dropped (that version is not used).

library(tidyverse)
library(performance)
library(DHARMa)
library(lme4)
library(lmerTest)
library(modelbased)

# Load data ---------------------------------------------------------------

dat <- read_csv("data/cleaned/11.1_demography-data_clean.csv")
culm.change.raw <- read_csv("data/cleaned/11.1_change-in-culm-density-cover_clean.csv")
survival.dat <- read_csv("data/cleaned/11.2_survival-data_clean.csv")

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
  select(-Plant_ID, -Change_Reproductive_culms, -Change_Total_Live_Culms) %>% 
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
dat.survival <- survival.dat %>% 
  mutate(Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         ShrubCover_scaled = scale(ShrubCover, center = TRUE, scale = TRUE)[, 1],
         HerbCover_scaled = scale(HerbCover, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         BGDensity_scaled = scale(BGDensity, center = TRUE, scale = TRUE)[, 1]) %>% 
  mutate(survival_prop = seedlings_surviving / remaining_toothpicks)



# Total culm change -------------------------------------------------------

# *** Version 1: Nested Site/Transect/Plot/Plant_ID ***
total1 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
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
summary(total1)
r2(total1) # marginal: 0.137; conditional: 0.466
res.total1 <- simulateResiduals(total1)
plotQQunif(res.total1)
plotResiduals(res.total1) 

#   Predicted vs. observed 
total1.pred <- estimate_expectation(total1)
total1.pred$Change_Total_Live_Culms <- culm.change$Change_Total_Live_Culms

total1.pred.plot <- total1.pred %>% 
  ggplot(aes(x = Change_Total_Live_Culms, y = Predicted)) +
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
total1.pred.plot



# Reproductive culm change ------------------------------------------------

# Version 1: Nested Site/Transect/Plot/Plant_ID
repro1 <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                 PlotSlope_scaled + Change_BGDensity_scaled +
                 Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 Change_ShrubCover_scaled * PlotSlope_scaled +
                 Change_HerbCover_scaled * PlotSlope_scaled +
                 (1 | Site / Transect / Plot / Plant_ID),
               data = culm.change) # boundary (single) fit
VarCorr(repro1) # 0 variance for Site/Transect/Plot/Plant_ID


# *** Version 2: Nested Site/Transect/Plot ***
repro2 <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
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
summary(repro2)
r2(repro2) # marginal: 0.085; conditional: 0.291
res.repro2 <- simulateResiduals(repro2)
plotQQunif(res.repro2)
plotResiduals(res.repro2) 

#   Predicted vs. observed 
repro2.pred <- estimate_expectation(repro2)
repro2.pred$Change_Reproductive_culms <- culm.change$Change_Reproductive_culms

repro2.pred.plot <- repro2.pred %>% 
  ggplot(aes(x = Change_Reproductive_culms, y = Predicted)) +
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
repro2.pred.plot



# BG density change -------------------------------------------------------

# Version 2: Nested Site/Transect/Plot
bgden2 <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + 
                 PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 Change_ShrubCover_scaled * PlotSlope_scaled +
                 Change_HerbCover_scaled * PlotSlope_scaled +
                 (1 | Site / Transect / Plot),
               data = plot.change) # boundary (single) fit
VarCorr(bgden2) # 0 variance for Site/Transect/Plot


# *** Version 3: Nested Site/Transect ***
bgden3 <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + 
                 PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 Change_ShrubCover_scaled * PlotSlope_scaled +
                 Change_HerbCover_scaled * PlotSlope_scaled +
                 (1 | Site / Transect),
               data = plot.change)
summary(bgden3)
r2(bgden3) # marginal: 0.378; conditional: 0.456
res.bgden3 <- simulateResiduals(bgden3)
plotQQunif(res.bgden3)
plotResiduals(res.bgden3) 
check_collinearity(bgden3)

#   Predicted vs. observed
bgden3.pred <- estimate_expectation(bgden3)
bgden3.pred$Change_BGDensity <- plot.change$Change_BGDensity

bgden3.pred.plot <- bgden3.pred %>% 
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
bgden3.pred.plot



# BG cover change ---------------------------------------------------------

# Version 2: Nested Site/Transect/Plot
bgcov2 <- lmer(Change_BGCover ~ Prev_year_precip_scaled + 
                 PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGCover_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 Change_ShrubCover_scaled * PlotSlope_scaled +
                 Change_HerbCover_scaled * PlotSlope_scaled +
                 (1 | Site / Transect / Plot),
               data = plot.change) # boundary (single) fit
VarCorr(bgcov2) # 0 variance for Site/Transect/Plot and Site/Transect


# *** Version 4: Site only ***
bgcov4 <- lmer(Change_BGCover ~ Prev_year_precip_scaled + 
                 PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGCover_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 Change_ShrubCover_scaled * PlotSlope_scaled +
                 Change_HerbCover_scaled * PlotSlope_scaled +
                 (1 | Site),
               data = plot.change)
summary(bgcov4)
r2(bgcov4) # marginal: 0.298; conditional: 0.306
res.bgcov4 <- simulateResiduals(bgcov4)
plotQQunif(res.bgcov4)
plotResiduals(res.bgcov4) 

#   Predicted vs. observed 
bgcov4.pred <- estimate_expectation(bgcov4)
bgcov4.pred$Change_BGCover <- plot.change$Change_BGCover

bgcov4.pred.plot <- bgcov4.pred %>% 
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
bgcov4.pred.plot




# Survival ----------------------------------------------------------------

# *** Version 2: Nested Site/Transect/Plot ***
survival2 <- glmer(cbind(seedlings_surviving,
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
summary(survival2)
r2(survival2) # marginal: 0.492; conditional: 0.963
res.survival2 <- simulateResiduals(survival2)
plotQQunif(res.survival2)
plotResiduals(res.survival2) 
check_collinearity(survival2)

#   Predicted vs. observed 
survival2.pred <- estimate_expectation(survival2)
survival2.pred$survival_prop <- dat.survival$survival_prop

survival2.pred.plot <- survival2.pred %>% 
  ggplot(aes(x = survival_prop, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  ggtitle("Seedling survival model, predicted vs. observed")  +
  xlab("Proportion of surviving seedlings [observed]") +
  theme_bw() 
survival2.pred.plot




# Simple linear regression ------------------------------------------------

# Shrub vs. precip
summary(lm(Change_ShrubCover ~ Prev_year_precip, data = plot.change))
summary(lm(Change_ShrubCover ~ Prev_year_precip_scaled, data = plot.change))

# Herb vs. precip
summary(lm(Change_HerbCover ~ Prev_year_precip, data = plot.change))
summary(lm(Change_HerbCover ~ Prev_year_precip_scaled, data = plot.change))

# Total vs density
summary(lm(Change_Total_Live_Culms ~ Change_BGDensity, data = culm.change))
summary(lm(Change_Total_Live_Culms ~ Change_BGDensity_scaled, data = culm.change))

# Total vs cover
summary(lm(Change_Total_Live_Culms ~ Change_BGCover, data = culm.change))
summary(lm(Change_Total_Live_Culms ~ Change_BGCover_scaled, data = culm.change))

# Density vs cover
summary(lm(Change_BGDensity ~ Change_BGCover, data = culm.change))
summary(lm(Change_BGDensity ~ Change_BGCover_scaled, data = culm.change))

# Density change vs initial
summary(lm(Change_BGDensity ~ Init_BGDensity, data = culm.change))
summary(lm(Change_BGDensity ~ Init_BGDensity_scaled, data = culm.change))



# Write out predicted vs. observed graphs ---------------------------------

# Total change
tiff("figures/2026-03_draft-figures-revision1.3/Total-change_predicted-vs-observed.tiff",
     units = "in", height = 4, width = 6, res = 150)
total1.pred.plot
dev.off()

# Repro change
tiff("figures/2026-03_draft-figures-revision1.3/Repro-change_predicted-vs-observed.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro2.pred.plot
dev.off()

# BG density change
tiff("figures/2026-03_draft-figures-revision1.3/BG-density-change_predicted-vs-observed.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden3.pred.plot
dev.off()

# BG cover change
tiff("figures/2026-03_draft-figures-revision1.3/BG-cover-change_predicted-vs-observed.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov4.pred.plot
dev.off()

# Survival
tiff("figures/2026-03_draft-figures-revision1.3/Survival_predicted-vs-observed.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival2.pred.plot
dev.off()


# Save --------------------------------------------------------------------

# Needed for graphs
save(culm.change, plot.change, dat.survival, 
     total1, repro2, bgden3, bgcov4, survival2,
     file = "RData/15.2_data-and-models-revision1.3.RData")


save.image("RData/15.2_linear-models-revision1.3.RData")
