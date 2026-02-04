# Created: 2026-01-28
# Updated: 2026-01-29

# Purpose: Run models without model selection via AICc so p-values can be used instead;
#   use year-to-year-change as response variable.

# Am going to go with Version 2 (nested Site / Transect) for all.

# Results are very similar to submitted version in terms of significance; manuscript
#   discussion and interpretation do not need to be updated.


library(tidyverse)
library(glmmTMB)
library(performance)
library(DHARMa)
library(lme4)
library(lmerTest)
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
         Change_HerbCover_scaled = scale(Change_HerbCover, scale = TRUE)[, 1],
         Change_BGCover_scaled = scale(Change_BGCover, scale = TRUE)[, 1])

# Separate out plot-level data
plot.change <- culm.change.raw %>% 
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm,
         -Change_Reproductive_culms, -Change_Total_Live_Culms, -Notes) %>% 
  distinct(.keep_all = TRUE) %>% 
  filter(Aspect != "flat") %>% 
  mutate(PlotSlope_scaled = scale(PlotSlope, center = TRUE, scale = TRUE)[, 1],
         Prev_year_precip_scaled = scale(Prev_year_precip, center = TRUE, scale = TRUE)[, 1],
         Change_ShrubCover_scaled = scale(Change_ShrubCover, scale = TRUE)[, 1],
         Change_HerbCover_scaled = scale(Change_HerbCover, scale = TRUE)[, 1],
         Change_BGCover_scaled = scale(Change_BGCover, scale = TRUE)[, 1]) %>% 
  mutate(Plot = as.character(Plot))


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

# Version 1: Transect
total1 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Transect),
              data = culm.change.flat.rm)
summary(total1)
r2(total1) # marginal: 0.144; conditional: 0.391
res.total1 <- simulateResiduals(total1)
plotQQunif(res.total1)
plotResiduals(res.total1) 
check_collinearity(total1) 


# Version 2: Nested Site/Transect
total2 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Site / Transect),
               data = culm.change.flat.rm)
summary(total2)
r2(total2) # marginal: 0.119; conditional: 0.410
res.total2 <- simulateResiduals(total2)
plotQQunif(res.total2)
plotResiduals(res.total2) 

#   Predicted vs. observed 
total2.pred <- estimate_expectation(total2)
total2.pred$Change_Total_Live_Culms <- culm.change.flat.rm$Change_Total_Live_Culms

total2.pred.plot <- total2.pred %>% 
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
total2.pred.plot


# Version 3: Nested Site/Transect/Plant_ID
total3 <- lmer(Change_Total_Live_Culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Site / Transect / Plant_ID),
               data = culm.change.flat.rm)
summary(total3)
r2(total3) # marginal: 0.121; conditional: 0.455
res.total3 <- simulateResiduals(total3)
plotQQunif(res.total3)
plotResiduals(res.total3) 



# Reproductive culms ------------------------------------------------------

# Version 1: Transect
repro1 <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Transect),
               data = culm.change.flat.rm)
summary(repro1)
r2(repro1) # marginal: 0.155; conditional: 0.318
res.repro1 <- simulateResiduals(repro1)
plotQQunif(res.repro1)
plotResiduals(res.repro1) 


# Version 2: Nested Site/Transect
repro2 <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Site / Transect),
               data = culm.change.flat.rm)
summary(repro2)
r2(repro2) # marginal: 0.098; conditional: 0.295
res.repro2 <- simulateResiduals(repro2)
plotQQunif(res.repro2)
plotResiduals(res.repro2) 

#   Predicted vs. observed 
repro2.pred <- estimate_expectation(repro2)
repro2.pred$Change_Reproductive_culms <- culm.change.flat.rm$Change_Reproductive_culms

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


# Version 3: Nested Site/Transect/Plant_ID
repro3 <- lmer(Change_Reproductive_culms ~ Prev_year_precip_scaled +  
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled + Change_HerbCover_scaled +
                 Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_BGDensity_scaled +
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Site / Transect / Plant_ID),
               data = culm.change.flat.rm)
summary(repro3)
r2(repro3) # marginal: 0.122; conditional: can't compute
res.repro3 <- simulateResiduals(repro3)
plotQQunif(res.repro3)
plotResiduals(res.repro3) 



# Buffelgrass density change ----------------------------------------------

# Version 1: Transect
bgden1 <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + 
                Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                Change_HerbCover_scaled + 
                Prev_year_precip_scaled * Change_ShrubCover_scaled +
                Prev_year_precip_scaled * Change_HerbCover_scaled +
                (1 | Transect),
              data = plot.change)
summary(bgden1)
r2(bgden1) # marginal: 0.384; conditional: 0.426
res.bgden1 <- simulateResiduals(bgden1)
plotQQunif(res.bgden1)
plotResiduals(res.bgden1) 
check_collinearity(bgden1)


# Version 2: Nested Site/Transect
bgden2 <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + 
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Site / Transect),
               data = plot.change)
summary(bgden2)
r2(bgden2) # marginal: 0.381; conditional: 0.427
res.bgden2 <- simulateResiduals(bgden2)
plotQQunif(res.bgden2)
plotResiduals(res.bgden2) 

#   Predicted vs. observed
bgden2.pred <- estimate_expectation(bgden2)
bgden2.pred$Change_BGDensity <- plot.change$Change_BGDensity

bgden2.pred.plot <- bgden2.pred %>% 
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
bgden2.pred.plot


# Version 3: Nested Site/Transect/Plot
bgden3 <- lmer(Change_BGDensity ~ Prev_year_precip_scaled + 
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Site / Transect / Plot),
               data = plot.change)
summary(bgden3)
r2(bgden3) # marginal: 0.399; conditional: can't compute
res.bgden3 <- simulateResiduals(bgden3)
plotQQunif(res.bgden3)
plotResiduals(res.bgden3) 



# Buffelgrass cover change ------------------------------------------------

# Version 1: Transect
bgcov1 <- lmer(Change_BGCover ~ Prev_year_precip_scaled + 
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Transect),
               data = plot.change)
summary(bgcov1)
r2(bgcov1) # marginal: 0.243; conditional: 0.284
res.bgcov1 <- simulateResiduals(bgcov1)
plotQQunif(res.bgcov1)
plotResiduals(res.bgcov1) 
check_collinearity(bgcov1)


# Version 2: Nested Site/Transect
bgcov2 <- lmer(Change_BGCover ~ Prev_year_precip_scaled + 
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Site / Transect),
               data = plot.change)
summary(bgcov2)
r2(bgcov2) # marginal: 0.248; conditional: 0.295
res.bgcov2 <- simulateResiduals(bgcov2)
plotQQunif(res.bgcov2)
plotResiduals(res.bgcov2) 

#   Predicted vs. observed 
bgcov2.pred <- estimate_expectation(bgcov2)
bgcov2.pred$Change_BGCover <- plot.change$Change_BGCover

bgcov2.pred.plot <- bgcov2.pred %>% 
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
bgcov2.pred.plot


# Version 3: Nested Site/Transect/Plot
bgcov3 <- lmer(Change_BGCover ~ Prev_year_precip_scaled + 
                 Aspect + PlotSlope_scaled + Change_ShrubCover_scaled +
                 Change_HerbCover_scaled + 
                 Prev_year_precip_scaled * Change_ShrubCover_scaled +
                 Prev_year_precip_scaled * Change_HerbCover_scaled +
                 (1 | Site / Transect / Plot),
               data = plot.change)
summary(bgcov3)
r2(bgcov3) # marginal: 0.260; conditional: can't compute
res.bgcov3 <- simulateResiduals(bgcov3)
plotQQunif(res.bgcov3)
plotResiduals(res.bgcov3) 



# Survival ----------------------------------------------------------------

# Version 1: Transect
survival1 <- glmmTMB(survival_transf ~ Prev_year_precip_scaled +
                      Aspect + PlotSlope_scaled + ShrubCover_scaled +
                      HerbCover_scaled + BGDensity_scaled +
                      Prev_year_precip_scaled * ShrubCover_scaled +
                      Prev_year_precip_scaled * HerbCover_scaled +
                      Prev_year_precip_scaled * BGDensity_scaled +
                      (1 | Transect),
                    data = dat.survival,
                    family = beta_family(link = "logit"))
summary(survival1)
r2(survival1) # marginal: 0.260; conditional: can't compute
res.survival1 <- simulateResiduals(survival1)
plotQQunif(res.survival1)
plotResiduals(res.survival1) 


# Version 2: Nested Site/Transect
survival2 <- glmmTMB(survival_transf ~ Prev_year_precip_scaled +
                       Aspect + PlotSlope_scaled + ShrubCover_scaled +
                       HerbCover_scaled + BGDensity_scaled +
                       Prev_year_precip_scaled * ShrubCover_scaled +
                       Prev_year_precip_scaled * HerbCover_scaled +
                       Prev_year_precip_scaled * BGDensity_scaled +
                       (1 | Site / Transect),
                     data = dat.survival,
                     family = beta_family(link = "logit"))
summary(survival2)
r2(survival2) # marginal: 0.819; conditional: can't compute
res.survival2 <- simulateResiduals(survival2)
plotQQunif(res.survival2)
plotResiduals(res.survival2) 

#   Predicted vs. observed 
survival2.pred <- estimate_expectation(survival2)
survival2.pred$survival_transf <- dat.survival$survival_transf

survival2.pred.plot <- survival2.pred %>% 
  ggplot(aes(x = survival_transf, y = Predicted)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "blue", linewidth = 1) +
  ggtitle("Seedling survival model, predicted vs. observed")  +
  xlab("Buffelgrass seedling survival (%) [observed]") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)
survival2.pred.plot




# Simple linear regression ------------------------------------------------

# Shrub vs. precip
shrub.precip <- lmer(Change_ShrubCover ~ Prev_year_precip_scaled + (1 | Site / Transect), data = plot.change)
summary(shrub.precip)
r2(shrub.precip)

# Herb vs. precip
herb.precip <- lmer(Change_HerbCover ~ Prev_year_precip_scaled + (1 | Site / Transect), data = plot.change)
summary(herb.precip)
r2(herb.precip)

# Total culm vs. plot cover
total.cover <- lmer(Change_Total_Live_Culms ~ Change_BGCover_scaled + (1 | Site / Transect), data = culm.change.flat.rm)
summary(total.cover)
r2(total.cover)

# Plot density vs. plot cover
density.cover <- lmer(Change_BGDensity ~ Change_BGCover_scaled + (1 | Site / Transect), data = plot.change)
summary(density.cover)
r2(density.cover)


# Write out predicted vs. observed graphs ---------------------------------

# Total change
tiff("figures/2026-01_draft-figures-revision1.1/Total-change_predicted-vs-observed.tiff",
     units = "in", height = 4, width = 6, res = 150)
total2.pred.plot
dev.off()

# Repro change
tiff("figures/2026-01_draft-figures-revision1.1/Repro-change_predicted-vs-observed.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro2.pred.plot
dev.off()

# BG density change
tiff("figures/2026-01_draft-figures-revision1.1/BG-density-change_predicted-vs-observed.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden2.pred.plot
dev.off()

# BG cover change
tiff("figures/2026-01_draft-figures-revision1.1/BG-cover-change_predicted-vs-observed.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov2.pred.plot
dev.off()

# Survival
tiff("figures/2026-01_draft-figures-revision1.1/Survival_predicted-vs-observed.tiff",
     units = "in", height = 4, width = 6, res = 150)
survival2.pred.plot
dev.off()


# Save --------------------------------------------------------------------

# Needed for graphs
save(culm.change.flat.rm, plot.change, dat.survival, 
     total2, repro2, bgden2, bgcov2, survival2,
     file = "RData/09.2_data-and-version2-models-revision1.1.RData")


save.image("RData/09.2_linear-models-revision1.1.RData")
