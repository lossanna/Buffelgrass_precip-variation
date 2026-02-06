# Created: 2026-02-04
# Updated: 2026-02-06

# Purpose: Rerun models and include inital BG density, shrub cover, and herb cover values
#   as explanatory variables.

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
culm.change.flat.rm <- culm.change.raw %>% 
  filter(Aspect != "flat") %>% 
  mutate(PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         Change_BGDensity_scaled = scale(Change_BGDensity, scale = TRUE)[, 1],
         Change_ShrubCover_scaled = scale(Change_ShrubCover, scale = TRUE)[, 1],
         Change_HerbCover_scaled = scale(Change_HerbCover, scale = TRUE)[, 1],
         Change_BGCover_scaled = scale(Change_BGCover, scale = TRUE)[, 1],
         Init_BGDensity_scaled = scale(Init_BGDensity, scale = TRUE)[, 1],
         Init_ShrubCover_scaled = scale(Init_ShrubCover, scale = TRUE)[, 1],
         Init_HerbCover_scaled = scale(Init_HerbCover, scale = TRUE)[, 1],
         Init_BGCover_scaled = scale(Init_BGCover, scale = TRUE)[, 1]) %>% 
  mutate(Plant_ID = as.character(Plant_ID))

# Center and scale numeric variables for plot-level data
plot.change <- culm.change.raw %>% 
  select(-Plant_ID, -Change_Reproductive_culms, -Change_Total_Live_Culms) %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(Aspect != "flat") %>% 
  mutate(PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         Change_ShrubCover_scaled = scale(Change_ShrubCover, scale = TRUE)[, 1],
         Change_HerbCover_scaled = scale(Change_HerbCover, scale = TRUE)[, 1],
         Change_BGCover_scaled = scale(Change_BGCover, scale = TRUE)[, 1],
         Init_BGDensity_scaled = scale(Init_BGDensity, scale = TRUE)[, 1],
         Init_ShrubCover_scaled = scale(Init_ShrubCover, scale = TRUE)[, 1],
         Init_HerbCover_scaled = scale(Init_HerbCover, scale = TRUE)[, 1],
         Init_BGCover_scaled = scale(Init_BGCover, scale = TRUE)[, 1]) %>% 
  mutate(Plot = as.character(Plot))


# Center and scale numeric variables for survival data
dat.survival <- survival.dat %>% 
  filter(Aspect != "flat") %>% 
  mutate(Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         ShrubCover_scaled = scale(ShrubCover, center = TRUE, scale = TRUE)[, 1],
         HerbCover_scaled = scale(HerbCover, center = TRUE, scale = TRUE)[, 1],
         PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         BGDensity_scaled = scale(BGDensity, center = TRUE, scale = TRUE)[, 1]) %>% 
  mutate(Plot = as.character(Plot)) %>% 
  mutate(survival_prop = seedlings_surviving / remaining_toothpicks)


# Total culm change -------------------------------------------------------

# Version 1: Initial conditions as numeric, nested Site/Transect
total1 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_BGDensity_scaled +
                 Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 (1 | Site / Transect),
               data = culm.change.flat.rm)
summary(total1)
r2(total1) # marginal: 0.128; conditional: 0.416
res.total1 <- simulateResiduals(total1)
plotQQunif(res.total1)
plotResiduals(res.total1) 
check_collinearity(total1)

#   Predicted vs. observed 
total1.pred <- estimate_expectation(total1)
total1.pred$Change_Total_Live_Culms <- culm.change.flat.rm$Change_Total_Live_Culms

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


# Version 2: Initial conditions as numeric, nested Site/Transect/Plant_ID
total2 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_BGDensity_scaled +
                 Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 (1 | Site / Transect / Plant_ID),
               data = culm.change.flat.rm)
summary(total2)
r2(total2) # marginal: 0.130; conditional: 0.461
res.total2 <- simulateResiduals(total2)
plotQQunif(res.total2)
plotResiduals(res.total2) 



# Reproductive culm change ------------------------------------------------

# Version 1: Initial conditions as numeric, nested Site/Transect
repro1 <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_BGDensity_scaled +
                 Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 (1 | Site / Transect),
               data = culm.change.flat.rm)
summary(repro1)
r2(repro1) # marginal: 0.100; conditional: 0.305
res.repro1 <- simulateResiduals(repro1)
plotQQunif(res.repro1)
plotResiduals(res.repro1) 
check_collinearity(repro1)

#   Predicted vs. observed 
repro1.pred <- estimate_expectation(repro1)
repro1.pred$Change_Reproductive_culms <- culm.change.flat.rm$Change_Reproductive_culms

repro1.pred.plot <- repro1.pred %>% 
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
repro1.pred.plot


# Version 2: Initial conditions as numeric, nested Site/Transect/Plant_ID
repro2 <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_BGDensity_scaled +
                 Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 Init_BGDensity_scaled + Init_ShrubCover_scaled + Init_HerbCover_scaled +
                 (1 | Site / Transect / Plant_ID),
               data = culm.change.flat.rm)
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
total1.pred.plot
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
save(culm.change.flat.rm, plot.change, dat.survival, 
     total1, repro1, bgden1, bgcov1, survival1,
     file = "RData/12.1_data-and-models-revision1.2.RData")


save.image("RData/12.1_linear-models-revision1.2.RData")
