# Created: 2024-09-23
# Updated: 2025-04-03

# Purpose: Graph culm, cover and density response to precip variation.

# Have to use average precip dev to represent each year, because the actual precip dev
#   is slightly different by sampling date.


library(tidyverse)
library(scales)
library(viridis)

# Load data ---------------------------------------------------------------

dat <- read_csv("data/cleaned/04_demography-data_clean.csv")
culm.change <- read_csv("data/cleaned/04_change-in-culm-density-cover_clean.csv")


# Data wrangling ----------------------------------------------------------

# Create df of average and SE to make line graph for culm count
#   By site
culm.avg.site <- dat %>% 
  group_by(Year, StudyYear, Site) %>% 
  summarise(repro_avg = mean(Reproductive_culms),
            repro_se = sd(Reproductive_culms) / sqrt(n()),
            total_avg = mean(Total_Live_Culms),
            total_se = sd(Total_Live_Culms) / sqrt(n()),
            Perc_dev_avg = mean(Perc_dev),
            .groups = "keep")
culm.change.avg.site <- culm.change %>% 
  group_by(Year, StudyYear, Site) %>% 
  summarise(repro_avg = mean(Change_Reproductive_culms),
            repro_se = sd(Change_Reproductive_culms) / sqrt(n()),
            total_avg = mean(Change_Total_Live_Culms),
            total_se = sd(Change_Total_Live_Culms) / sqrt(n()),
            Perc_dev_avg = mean(Perc_dev),
            .groups = "keep")

#   By site and aspect
culm.avg.site.aspect <- dat %>% 
  group_by(Year, StudyYear, Site, Aspect) %>% 
  summarise(repro_avg = mean(Reproductive_culms),
            repro_se = sd(Reproductive_culms) / sqrt(n()),
            total_avg = mean(Total_Live_Culms),
            total_se = sd(Total_Live_Culms) / sqrt(n()),
            Perc_dev_avg = mean(Perc_dev),
            .groups = "keep")
culm.change.avg.site.aspect <- culm.change %>% 
  group_by(Year, StudyYear, Site, Aspect) %>% 
  summarise(repro_avg = mean(Change_Reproductive_culms),
            repro_se = sd(Change_Reproductive_culms) / sqrt(n()),
            total_avg = mean(Change_Total_Live_Culms),
            total_se = sd(Change_Total_Live_Culms) / sqrt(n()),
            Perc_dev_avg = mean(Perc_dev),
            .groups = "keep")

#   By aspect
culm.avg.aspect <- dat %>% 
  group_by(Year, Aspect) %>% 
  summarise(repro_avg = mean(Reproductive_culms),
            repro_se = sd(Reproductive_culms) / sqrt(n()),
            total_avg = mean(Total_Live_Culms),
            total_se = sd(Total_Live_Culms) / sqrt(n()),
            Perc_dev_avg = mean(Perc_dev),
            .groups = "keep")
culm.change.avg.aspect <- culm.change %>% 
  group_by(Year, Aspect) %>% 
  summarise(repro_avg = mean(Change_Reproductive_culms),
            repro_se = sd(Change_Reproductive_culms) / sqrt(n()),
            total_avg = mean(Change_Total_Live_Culms),
            total_se = sd(Change_Total_Live_Culms) / sqrt(n()),
            Perc_dev_avg = mean(Perc_dev),
            .groups = "keep")


# Separate out plot-level data
dat.plot <- dat %>% 
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm) %>% 
  distinct(.keep_all = TRUE)

plot.change <- culm.change %>% 
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm,
         -Change_Reproductive_culms, -Change_Total_Live_Culms) %>% 
  distinct(.keep_all = TRUE)
  

# Create df of average and SE to make line graph for density & cover
#   By site
plot.avg.site <- dat.plot %>% 
  group_by(Year, StudyYear, Site) %>% 
  summarise(bgden_avg = mean(BGDensity),
            bgden_se = sd(BGDensity) / sqrt(n()),
            bgcov_avg = mean(BGCover),
            bgcov_se   = sd(BGCover) / sqrt(n()),
            shrub_avg = mean(ShrubCover),
            shrub_se = sd(ShrubCover) / sqrt(n()),
            forb_avg = mean(ForbCover),
            forb_se = sd(ForbCover) / sqrt(n()),
            ng_avg = mean(NGCover),
            ng_se = sd(NGCover) / sqrt(n()),
            Perc_dev_avg = mean(Perc_dev),
            .groups = "keep")
plot.change.avg.site <- plot.change %>% 
  group_by(Year, StudyYear, Site) %>% 
  summarise(bgden_avg = mean(Change_BGDensity),
            bgden_se = sd(Change_BGDensity) / sqrt(n()),
            bgcov_avg = mean(Change_BGCover),
            bgcov_se   = sd(Change_BGCover) / sqrt(n()),
            shrub_avg = mean(Change_ShrubCover),
            shrub_se = sd(Change_ShrubCover) / sqrt(n()),
            forb_avg = mean(Change_ForbCover),
            forb_se = sd(Change_ForbCover) / sqrt(n()),
            ng_avg = mean(Change_NGCover),
            ng_se = sd(Change_NGCover) / sqrt(n()),
            Perc_dev_avg = mean(Perc_dev),
            .groups = "keep")

#   By aspect
plot.avg.aspect <- dat.plot %>% 
  group_by(Year, Aspect) %>% 
  summarise(bgden_avg = mean(BGDensity),
            bgden_se = sd(BGDensity) / sqrt(n()),
            bgcov_avg = mean(BGCover),
            bgcov_se   = sd(BGCover) / sqrt(n()),
            shrub_avg = mean(ShrubCover),
            shrub_se = sd(ShrubCover) / sqrt(n()),
            forb_avg = mean(ForbCover),
            forb_se = sd(ForbCover) / sqrt(n()),
            ng_avg = mean(NGCover),
            ng_se = sd(NGCover) / sqrt(n()),
            Perc_dev_avg = mean(Perc_dev),
            .groups = "keep")
plot.change.avg.aspect <- plot.change %>% 
  group_by(Year, Aspect) %>% 
  summarise(bgden_avg = mean(Change_BGDensity),
            bgden_se = sd(Change_BGDensity) / sqrt(n()),
            bgcov_avg = mean(Change_BGCover),
            bgcov_se   = sd(Change_BGCover) / sqrt(n()),
            shrub_avg = mean(Change_ShrubCover),
            shrub_se = sd(Change_ShrubCover) / sqrt(n()),
            forb_avg = mean(Change_ForbCover),
            forb_se = sd(Change_ForbCover) / sqrt(n()),
            ng_avg = mean(Change_NGCover),
            ng_se = sd(Change_NGCover) / sqrt(n()),
            Perc_dev_avg = mean(Perc_dev),
            .groups = "keep")

#   By site and aspect
plot.avg.site.aspect <- dat.plot %>% 
  group_by(Year, StudyYear, Site, Aspect) %>% 
  summarise(bgden_avg = mean(BGDensity),
            bgden_se = sd(BGDensity) / sqrt(n()),
            bgcov_avg = mean(BGCover),
            bgcov_se   = sd(BGCover) / sqrt(n()),
            shrub_avg = mean(ShrubCover),
            shrub_se = sd(ShrubCover) / sqrt(n()),
            forb_avg = mean(ForbCover),
            forb_se = sd(ForbCover) / sqrt(n()),
            ng_avg = mean(NGCover),
            ng_se = sd(NGCover) / sqrt(n()),
            Perc_dev_avg = mean(Perc_dev),
            .groups = "keep")
plot.change.avg.site.aspect <- plot.change %>% 
  group_by(Year, StudyYear, Site, Aspect) %>% 
  summarise(bgden_avg = mean(Change_BGDensity),
            bgden_se = sd(Change_BGDensity) / sqrt(n()),
            bgcov_avg = mean(Change_BGCover),
            bgcov_se   = sd(Change_BGCover) / sqrt(n()),
            shrub_avg = mean(Change_ShrubCover),
            shrub_se = sd(Change_ShrubCover) / sqrt(n()),
            forb_avg = mean(Change_ForbCover),
            forb_se = sd(Change_ForbCover) / sqrt(n()),
            ng_avg = mean(Change_NGCover),
            ng_se = sd(Change_NGCover) / sqrt(n()),
            Perc_dev_avg = mean(Perc_dev),
            .groups = "keep")



# Aspects at each site ----------------------------------------------------

dat %>% 
  filter(Site == "ApachePeak") %>% 
  count(Aspect) # E, S, W, flat
dat %>% 
  filter(Site == "KinneyHill") %>% 
  count(Aspect) # E, S, SW, W
dat %>% 
  filter(Site == "LomaVerde") %>% 
  count(Aspect) # S, SW, W
dat %>% 
  filter(Site == "TumamocHill") %>% 
  count(Aspect) # E, N, S, W



# Precip deviation --------------------------------------------------------

# By aspect
precip.dev.aspect <- plot.avg.aspect %>% 
  ggplot(aes(x = Year, y = Perc_dev_avg)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Aspect) +
  ggtitle("Precipitation conditions") +
  xlab(NULL) +
  ylab("Precip deviation from average") +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
precip.dev.aspect

# By site
precip.dev.site <- plot.avg.site %>% 
  ggplot(aes(x = Year, y = Perc_dev_avg)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  ggtitle("Precipitation conditions") +
  xlab(NULL) +
  ylab("Precip deviation from average") +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))
precip.dev.site

# By site and aspect
plot.avg.site.aspect %>% 
  ggplot(aes(x = Year, y = Perc_dev_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  ggtitle("Precipitation conditions") +
  xlab(NULL) +
  ylab("Precip deviation from average") +
  theme_bw() +
  scale_y_continuous(labels = percent) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  theme(axis.text.x = element_text(color = "black"))



# Reproductive culms ------------------------------------------------------

## Repro: All combined ----------------------------------------------------

# Repro: Linear regression by Perc_dev
repro.all.lm <- dat %>% 
  ggplot(aes(x = Perc_dev, y = Reproductive_culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_x_continuous(labels = percent) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  xlab("Precip deviation from average") +
  ylab("No. of reproductive culms")
repro.all.lm

# Repro: geom_smooth() by Perc_dev
dat %>% 
  ggplot(aes(x = Perc_dev, y = Reproductive_culms)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  scale_x_continuous(labels = percent) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red")  +
  xlab("Precip deviation from average") +
  ylab("No. of reproductive culms")

# Repro: Wetter/drier divided, linear regression by Perc_dev
dat %>% 
  filter(Perc_dev > 0) %>% 
  ggplot(aes(x = Perc_dev, y = Reproductive_culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_x_continuous(labels = percent) +
  ggtitle("Wetter conditions") +
  xlab("Precip deviation from average") +
  ylab("No. of reproductive culms")
dat %>% 
  filter(Perc_dev < 0) %>% 
  ggplot(aes(x = Perc_dev, y = Reproductive_culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_x_continuous(labels = percent) +
  ggtitle("Drier conditions") +
  xlab("Precip deviation from average") +
  ylab("No. of reproductive culms")


# Repro: scatterplot Plot slope and Perc_dev
dat %>% 
  ggplot(aes(x = PlotSlope, y = Reproductive_culms, color = Perc_dev)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1)


# Repro: linear regression with buffelgrass density
dat %>% 
  ggplot(aes(x = BGDensity, y = Reproductive_culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()



## Repro: By aspect -------------------------------------------------------

# Repro: Aspect, all conditions (boxplot)
dat %>% 
  ggplot(aes(x = Aspect, y = Reproductive_culms)) +
  geom_boxplot() +
  theme_bw()

# Repro: Aspect by Year (boxplot, all obs)
dat %>% 
  mutate(Year = as.character(Year)) %>% 
  ggplot(aes(x = Year, y = Reproductive_culms)) +
  geom_boxplot() +
  facet_wrap(~Aspect) +
  theme_bw()

# Repro: Aspect by Perc_dev (scatterplot, all obs)
dat %>% 
  ggplot(aes(x = Perc_dev, y = Reproductive_culms)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~Aspect) +
  theme_bw() 

# Repro: Aspect by Perc_dev (linear regression, all obs)
repro.all.aspect.lm <- dat %>% 
  ggplot(aes(x = Perc_dev, y = Reproductive_culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~Aspect) +
  theme_bw() +
  xlab("Precip deviation from average") +
  ylab("No. of reproductive culms")
repro.all.aspect.lm

# Repro: Aspect by Year (line graph, average)
culm.avg.aspect %>% 
  ggplot(aes(x = Year, y = repro_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~Aspect)

# Repro: Aspect by Perc_dev (line graph, average)
repro.aspect.avg <- culm.avg.aspect %>% 
  ggplot(aes(x = Perc_dev_avg, y = repro_avg)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = repro_avg - repro_se, ymax = repro_avg + repro_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("No. of reproductive culms") +
  facet_wrap(~Aspect) +
  theme(legend.position = "none")
repro.aspect.avg


# Repro: Aspect by Prev_year_precip (linear regression, all obs)
repro.all.aspect.prevprecip.lm <- dat %>% 
  ggplot(aes(x = Prev_year_precip, y = Reproductive_culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Aspect) +
  theme_bw() +
  xlab("Previous year precip (mm)") +
  ylab("No. of reproductive culms")
repro.all.aspect.prevprecip.lm


## Repro: By site ---------------------------------------------------------

# Repro: Site by Year (line graph, average)
culm.avg.site %>% 
  ggplot(aes(x = Year, y = repro_avg, color = Site)) +
  geom_point() +
  geom_line() +
  theme_bw()

# Repro: Site by Perc_dev (line graph, average)
repro.site.avg <- culm.avg.site %>% 
  ggplot(aes(x = Perc_dev_avg, y = repro_avg, color = Site)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = repro_avg - repro_se, ymax = repro_avg + repro_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("No. of reproductive culms")
repro.site.avg


## Repro: By site and aspect ----------------------------------------------

# Repro: Site and aspect by Year (line graph, average)
culm.avg.site.aspect %>% 
  ggplot(aes(x = Year, y = repro_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw()

# Repro: Site and aspect by Perc_dev (line graph, average)
culm.avg.site.aspect %>% 
  ggplot(aes(x = Perc_dev_avg, y = repro_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = repro_avg - repro_se, ymax = repro_avg + repro_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~Site) +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("No. of reproductive culms")



# Total culms -------------------------------------------------------------

## Total: All combined ----------------------------------------------------

# Total: Linear regression by Perc_dev
total.all.lm <- dat %>% 
  ggplot(aes(x = Perc_dev, y = Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_x_continuous(labels = percent) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  xlab("Precip deviation from average") +
  ylab("Total number of culms")
total.all.lm

# Total: geom_smooth() by Perc_dev
dat %>% 
  ggplot(aes(x = Perc_dev, y = Total_Live_Culms)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  scale_x_continuous(labels = percent) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red")  +
  xlab("Precip deviation from average") +
  ylab("Total number of culms")

# Total: Wetter/drier divided, linear regression by Perc_dev
dat %>% 
  filter(Perc_dev > 0) %>% 
  ggplot(aes(x = Perc_dev, y = Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_x_continuous(labels = percent) +
  ggtitle("Wetter conditions") +
  xlab("Precip deviation from average") +
  ylab("Total number of culms")
dat %>% 
  filter(Perc_dev < 0) %>% 
  ggplot(aes(x = Perc_dev, y = Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_x_continuous(labels = percent) +
  ggtitle("Drier conditions") +
  xlab("Precip deviation from average") +
  ylab("Total number of culms")


# Total: scatterplot by Plot slope and Perc_dev
dat %>% 
  ggplot(aes(x = PlotSlope, y = Total_Live_Culms, color = Perc_dev)) +
  geom_point() +
  scale_color_viridis(option = "viridis", direction = -1)

# Total: linear regression with buffelgrass density
dat %>% 
  ggplot(aes(x = BGDensity, y = Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()

# Total: linear regression with buffelgrass cover
dat %>% 
  ggplot(aes(x = BGCover, y = Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()



## Total: By aspect -------------------------------------------------------

# Total: Aspect by Year (boxplot, all obs)
dat %>% 
  mutate(Year = as.character(Year)) %>% 
  ggplot(aes(x = Year, y = Total_Live_Culms)) +
  geom_boxplot() +
  facet_wrap(~Aspect) +
  theme_bw()

# Total: Aspect by Perc_dev (scatterplot, all obs)
dat %>% 
  ggplot(aes(x = Perc_dev, y = Total_Live_Culms)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~Aspect) +
  theme_bw()

# Total: Aspect by Perc_dev (linear regression, all obs)
total.all.aspect.lm <- dat %>% 
  ggplot(aes(x = Perc_dev, y = Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~Aspect) +
  theme_bw() +
  xlab("Precip deviation from average") +
  ylab("Total number of culms")
total.all.aspect.lm

# Total: Aspect by Year (line graph, average)
culm.avg.aspect %>% 
  ggplot(aes(x = Year, y = repro_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~Aspect)

# Total: Aspect by Perc_dev (line graph, average)
total.aspect.avg <- culm.avg.aspect %>% 
  ggplot(aes(x = Perc_dev_avg, y = total_avg)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = total_avg - total_se, ymax = total_avg + total_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Total number of culms") +
  facet_wrap(~Aspect)
total.aspect.avg

# Total: Aspect by Prev_year_precip (linear regression, all obs)
total.all.aspect.prevprecip.lm <- dat %>% 
  ggplot(aes(x = Prev_year_precip, y = Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Aspect) +
  theme_bw() +
  xlab("Previous year precip (mm)") +
  ylab("Total number of culms")
total.all.aspect.prevprecip.lm

# Total: Aspect by buffelgrass density (linear regression, all obs)
dat %>% 
  ggplot(aes(x = BGDensity, y = Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  facet_wrap(~Aspect)

# Total: Aspect by buffelgrass cover (linear regression, all obs)
dat %>% 
  ggplot(aes(x = BGCover, y = Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  facet_wrap(~Aspect)




## Total: By site ---------------------------------------------------------

# Total: Site by Year (line graph, average)
culm.avg.site %>% 
  ggplot(aes(x = Year, y = total_avg, color = Site)) +
  geom_point() +
  geom_line() +
  theme_bw()

# Total: Site by Perc_dev (line graph, average)
total.site.avg <- culm.avg.site %>% 
  ggplot(aes(x = Perc_dev_avg, y = total_avg, color = Site)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = total_avg - total_se, ymax = total_avg + total_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Total number of culms")
total.site.avg



## Total: By site and aspect ----------------------------------------------

# Total: Site and aspect by Year (line graph)
culm.avg.site.aspect %>% 
  ggplot(aes(x = Year, y = total_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw()

# Total: Site and aspect by Perc_dev (line graph, average)
culm.avg.site.aspect %>% 
  ggplot(aes(x = Perc_dev_avg, y = total_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = total_avg - total_se, ymax = total_avg + total_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~Site) +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Total number of culms")


# Buffelgrass density -----------------------------------------------------

## BG density: All combined -----------------------------------------------

# BG density: Linear regression by Perc_dev
bgden.all.lm <- dat.plot %>% 
  ggplot(aes(x = Perc_dev, y = BGDensity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_x_continuous(labels = percent) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Precip deviation from average",
       y = expression(paste("Density (individuals /  ", m^2, ")")),
       title = "Buffelgrass density")
bgden.all.lm

# BG density: geom_smooth() by Perc_dev
dat.plot %>% 
  ggplot(aes(x = Perc_dev, y = BGDensity)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  scale_x_continuous(labels = percent) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red")  +
  labs(x = "Precip deviation from average",
       y = expression(paste("Density (individuals /  ", m^2, ")")),
       title = "Buffelgrass density")

# BG density: Wetter/drier divided, linear regression by Perc_dev
dat.plot %>% 
  filter(Perc_dev > 0) %>% 
  ggplot(aes(x = Perc_dev, y = BGDensity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_x_continuous(labels = percent) +
  labs(x = "Precip deviation from average",
       y = expression(paste("Density (individuals /  ", m^2, ")")),
       title = "Buffelgrass density in wetter conditions")
dat.plot %>% 
  filter(Perc_dev < 0) %>% 
  ggplot(aes(x = Perc_dev, y = BGDensity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_x_continuous(labels = percent) +
  ggtitle("Drier conditions") +
  labs(x = "Precip deviation from average",
       y = expression(paste("Density (individuals /  ", m^2, ")")),
       title = "Buffelgrass density in drier conditions")

# BG density: scatterplot by Plot slope and Perc_dev
dat.plot %>% 
  ggplot(aes(x = PlotSlope, y = BGDensity, color = Perc_dev)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1)



## BG density: By aspect --------------------------------------------------

# BG density: Aspect by Year (boxplot, all obs)
dat.plot %>% 
  mutate(Year = as.character(Year)) %>% 
  ggplot(aes(x = Year, y = BGDensity)) +
  geom_boxplot() +
  facet_wrap(~Aspect) +
  theme_bw()

# BG density: Aspect by Perc_dev (scatterplot, all obs)
dat.plot %>% 
  ggplot(aes(x = Perc_dev, y = BGDensity)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~Aspect) +
  theme_bw()

# BG density: Aspect by Perc_dev (linear regression)
bgden.all.aspect.lm <- dat.plot %>% 
  ggplot(aes(x = Perc_dev, y = BGDensity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~Aspect) +
  theme_bw() +
  labs(x = "Precip deviation from average",
       y = expression(paste("Density (individuals /  ", m^2, ")")),
       title = "Buffelgrass density")
bgden.all.aspect.lm

# BG density: Aspect by Year (line graph, average)
plot.avg.aspect %>% 
  ggplot(aes(x = Year, y = bgden_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~Aspect)

# BG density: Aspect by Perc_dev (line graph, average)
bgden.aspect.avg <- plot.avg.aspect %>% 
  ggplot(aes(x = Perc_dev_avg, y = bgden_avg)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = bgden_avg - bgden_se, ymax = bgden_avg + bgden_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Precip deviation from average",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  facet_wrap(~Aspect) 
bgden.aspect.avg

# BG density: Aspect by Prev_year_precip (linear regression, all obs)
bgden.all.aspect.prevprecip.lm <- dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = BGDensity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Aspect) +
  theme_bw() +
  labs(x = "Previous year precip (mm)",
       y = expression(paste("Density (individuals /  ", m^2, ")")),
       title = "Buffelgrass density") 
bgden.all.aspect.prevprecip.lm



## BG density: By site ----------------------------------------------------

# BG density: Site by Year (line graph, average)
plot.avg.site %>% 
  ggplot(aes(x = Year, y = bgden_avg, color = Site)) +
  geom_point() +
  geom_line() +
  theme_bw()

# BG density: Site by Perc_dev (line graph, average)
bgden.site.avg <- plot.avg.site %>% 
  ggplot(aes(x = Perc_dev_avg, y = bgden_avg, color = Site)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = bgden_avg - bgden_se, ymax = bgden_avg + bgden_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Precip deviation from average",
       y = expression(paste("Density (individuals /  ", m^2, ")")),
       title = "Buffelgrass density") 
bgden.site.avg

# BG density: Site by Perc_dev (linear regression)
dat.plot %>% 
  ggplot(aes(x = Perc_dev, y = BGDensity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~Site) +
  theme_bw()


## BG density: By site and aspect -----------------------------------------

# BG density: Site and aspect by Year (line graph, average)
plot.avg.site.aspect %>% 
  ggplot(aes(x = Year, y = bgden_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw()

# BG density: Site and aspect by Perc_dev (line graph, average)
plot.avg.site.aspect %>% 
  ggplot(aes(x = Perc_dev_avg, y = bgden_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = bgden_avg - bgden_se, ymax = bgden_avg + bgden_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~Site) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Precip deviation from average",
       y = expression(paste("Density (individuals /  ", m^2, ")")),
       title = "Buffelgrass density") 



# Buffelgrass cover -------------------------------------------------------

## BG cover: All combined -------------------------------------------------

# BG cover: Linear regression by Perc_dev
bgcov.all.lm <- dat.plot %>% 
  ggplot(aes(x = Perc_dev, y = BGCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_x_continuous(labels = percent) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Precip deviation from average",
       y = "Cover (%)",
       title = "Buffelgrass cover")
bgcov.all.lm

# BG cover: geom_smooth() by Perc_dev
dat.plot %>% 
  ggplot(aes(x = Perc_dev, y = BGCover)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  scale_x_continuous(labels = percent) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Precip deviation from average",
       y = "Cover (%)",
       title = "Buffelgrass cover")

# BG cover: scatterplot by Plot slope and Perc_dev
dat.plot %>% 
  ggplot(aes(x = PlotSlope, y = BGCover, color = Perc_dev)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1)



## BG cover: By aspect ----------------------------------------------------

# BG cover: Aspect by Year (boxplot, all obs)
dat.plot %>% 
  mutate(Year = as.character(Year)) %>% 
  ggplot(aes(x = Year, y = BGCover)) +
  geom_boxplot() +
  facet_wrap(~Aspect) +
  theme_bw()

# BG cover: Aspect by Perc_dev (scatterplot, all obs)
dat.plot %>% 
  ggplot(aes(x = Perc_dev, y = BGCover)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~Aspect) +
  theme_bw() 

# BG cover: Aspect by Perc_dev (linear regression)
bgcov.all.aspect.lm <- dat.plot %>% 
  ggplot(aes(x = Perc_dev, y = BGCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~Aspect) +
  theme_bw() +
  labs(x = "Precip deviation from average",
       y = "Cover (%)",
       title = "Buffelgrass cover")
bgcov.all.aspect.lm

# BG cover: Aspect by Year (line graph, average)
plot.avg.aspect %>% 
  ggplot(aes(x = Year, y = bgcov_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~Aspect) +
  theme(legend.position = "none")

# BG cover: Aspect by Perc_dev (line graph, average)
plot.avg.aspect %>% 
  ggplot(aes(x = Perc_dev_avg, y = bgcov_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = bgcov_avg - bgcov_se, ymax = bgcov_avg + bgcov_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Precip deviation from average",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) +
  facet_wrap(~Aspect) +
  theme(legend.position = "none")

# BG cover: Aspect by Perc_dev (linear regression)
dat.plot %>% 
  ggplot(aes(x = Perc_dev, y = BGCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = scales::percent) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~Aspect) +
  theme_bw()

# BG cover: Aspect by Prev_year_precip (linear regression, all obs)
bgcov.all.aspect.prevprecip.lm <- dat.plot %>% 
  ggplot(aes(x = Prev_year_precip, y = BGCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Aspect) +
  theme_bw() +
  labs(x = "Previous year precip (mm)",
       y = "Cover (%)",
       title = "Buffelgrass cover") 
bgcov.all.aspect.prevprecip.lm


## BG cover: By site ------------------------------------------------------

# BG cover: Site by Year (line graph, average)
plot.avg.site %>% 
  ggplot(aes(x = Year, y = bgcov_avg, color = Site)) +
  geom_point() +
  geom_line() +
  theme_bw()

# BG cover: Site by Perc_dev (line graph, average)
bgcov.site.avg <- plot.avg.site %>% 
  ggplot(aes(x = Perc_dev_avg, y = bgcov_avg, color = Site)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = bgcov_avg - bgcov_se, ymax = bgcov_avg + bgcov_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Buffelgrass cover (%)")
bgcov.site.avg



## BG cover: By site and aspect -------------------------------------------

# BG cover: Site and aspect by Year (line graph, average)
plot.avg.site.aspect %>% 
  ggplot(aes(x = Year, y = bgcov_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw()

# BG cover: Site and aspect by Perc_dev (line graph, average)
plot.avg.site.aspect %>% 
  ggplot(aes(x = Perc_dev_avg, y = bgcov_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = bgcov_avg - bgcov_se, ymax = bgcov_avg + bgcov_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~Site) +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Buffelgrass cover (%)")    



# BG density & cover ------------------------------------------------------

# All combined
dat.plot %>% 
  ggplot(aes(x = BGCover, y = BGDensity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(x = "Cover (%)",
       y = expression(paste("Density (individuals /  ", m^2, ")")),
       title = "Buffelgrass density & cover") 

# By aspect
dat.plot %>% 
  ggplot(aes(x = BGCover, y = BGDensity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  facet_wrap(~Aspect) +
  labs(x = "Cover (%)",
       y = expression(paste("Density (individuals /  ", m^2, ")")),
       title = "Buffelgrass density & cover") 


# Shrub cover -------------------------------------------------------------

## Shrub cover: All combined ----------------------------------------------

# Shrub cover: linear regression by Perc_dev 
dat %>% 
  ggplot(aes(x = Perc_dev, y = ShrubCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Shrub cover (%)")

# Shrub cover: linear regression by Prev_year_precip 
dat %>% 
  ggplot(aes(x = Prev_year_precip, y = ShrubCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  xlab("Prev year precip (mm)") +
  ylab("Shrub cover (%)")



## Shrub cover: By aspect -------------------------------------------------

# Shrub cover: Aspect by Perc_dev (linear regression, all obs)
dat %>% 
  ggplot(aes(x = Perc_dev, y = ShrubCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  facet_wrap(~Aspect) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Shrub cover (%)")

# Shrub cover: Aspect by Prev_year_precip (linear regression, all obs)
dat %>% 
  ggplot(aes(x = Prev_year_precip, y = ShrubCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  facet_wrap(~Aspect) +
  xlab("Prev year precip (mm)") +
  ylab("Shrub cover (%)")


## Shrub cover: By site ---------------------------------------------------

# Shrub cover: Site by Year (line graph, avg)
plot.avg.site %>% 
  ggplot(aes(x = Year, y = shrub_avg, color = Site)) +
  geom_point() +
  geom_line() +
  theme_bw()

# Shrub cover: Site by Perc_dev (line graph, avg)
plot.avg.site %>% 
  ggplot(aes(x = Perc_dev_avg, y = shrub_avg, color = Site)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = shrub_avg - shrub_se, ymax = shrub_avg + shrub_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Shrub cover (%)")


## Shrub cover: By site and aspect ----------------------------------------

# Shrub cover: Site and aspect by Year (line graph, avg)
plot.avg.site.aspect %>% 
  ggplot(aes(x = Year, y = shrub_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw()

# Shrub cover: Site and aspect by Perc_dev (line graph, avg)
plot.avg.site.aspect %>% 
  ggplot(aes(x = Perc_dev_avg, y = shrub_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = shrub_avg - shrub_se, ymax = shrub_avg + shrub_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~Site) +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Shrub cover (%)")    



# Forb cover --------------------------------------------------------------

## Forb cover: All combined -----------------------------------------------

# Forb cover: linear regression by Perc_dev 
dat %>% 
  ggplot(aes(x = Perc_dev, y = ForbCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Forb cover (%)")

# Forb cover: linear regression by Prev_year_precip 
dat %>% 
  ggplot(aes(x = Prev_year_precip, y = ForbCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  xlab("Prev year precip (mm)") +
  ylab("Forb cover (%)")


## Forb cover: By aspect --------------------------------------------------

# Forb cover: Aspect by Perc_dev (linear regression, all obs)
dat %>% 
  ggplot(aes(x = Perc_dev, y = ForbCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  facet_wrap(~Aspect) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Forb cover (%)")


# Forb cover: By site -----------------------------------------------------

# Forb cover: Site by Year (line graph, avg)
plot.avg.site %>% 
  ggplot(aes(x = Year, y = forb_avg, color = Site)) +
  geom_point() +
  geom_line() +
  theme_bw()

# Forb cover: Site by Perc_dev (line graph, avg)
plot.avg.site %>% 
  ggplot(aes(x = Perc_dev_avg, y = forb_avg, color = Site)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = forb_avg - forb_se, ymax = forb_avg + forb_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Forb cover (%)")


## Forb cover: By site and aspect -----------------------------------------

# Forb cover: Site and Aspect by Year (line graph, avg)
plot.avg.site.aspect %>% 
  ggplot(aes(x = Year, y = forb_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw()

# Forb cover: Site and Aspect by Perc_dev (line graph, avg)
plot.avg.site.aspect %>% 
  ggplot(aes(x = Perc_dev_avg, y = forb_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = forb_avg - forb_se, ymax = forb_avg + forb_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~Site) +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Forb cover (%)")    



# Native grass cover ------------------------------------------------------

## NG cover: All combined -------------------------------------------------

# NG cover: linear regression by Perc_dev 
dat %>% 
  ggplot(aes(x = Perc_dev, y = NGCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Native grass cover (%)")

# NG cover: linear regression by Prev_year_precip 
dat %>% 
  ggplot(aes(x = Prev_year_precip, y = NGCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  xlab("Prev year precip (mm)") +
  ylab("Native grass cover (%)")


## NG cover: By aspect ----------------------------------------------------

# NG cover: Aspect by Perc_dev (linear regression, all obs)
dat %>% 
  ggplot(aes(x = Perc_dev, y = NGCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  facet_wrap(~Aspect) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Native grass cover (%)")


## NG cover: By site ------------------------------------------------------

# NG cover: Site by Year (line graph, avg)
plot.avg.site %>% 
  ggplot(aes(x = Year, y = ng_avg, color = Site)) +
  geom_point() +
  geom_line() +
  theme_bw()

# NG cover: Site by Perc_dev (line graph, avg)
plot.avg.site %>% 
  ggplot(aes(x = Perc_dev_avg, y = ng_avg, color = Site)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = ng_avg - ng_se, ymax = ng_avg + ng_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Native grass cover (%)")


## NG cover: By site and aspect -------------------------------------------

# NG cover: Site and aspect by Year (line graph, avg)
plot.avg.site.aspect %>% 
  ggplot(aes(x = Year, y = ng_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw()

# NG cover: Site and aspect by Perc_dev (line graph, avg)
plot.avg.site.aspect %>% 
  ggplot(aes(x = Perc_dev_avg, y = ng_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = ng_avg - ng_se, ymax = ng_avg + ng_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~Site) +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Native grass cover (%)")  


# Herb cover (grass & forb) -----------------------------------------------

## Herb cover: All combined -----------------------------------------------

# Herb cover: linear regression by Perc_dev 
dat %>% 
  ggplot(aes(x = Perc_dev, y = HerbCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Herbaceous cover (%)")

# Herb cover: linear regression by Prev_year_precip 
dat %>% 
  ggplot(aes(x = Prev_year_precip, y = HerbCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  xlab("Prev year precip (mm)") +
  ylab("Herbaceous cover (%)")


## Herb cover: By aspect --------------------------------------------------

# Herb cover: Aspect by Perc_dev (linear regression, all obs)
dat %>% 
  ggplot(aes(x = Perc_dev, y = HerbCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  facet_wrap(~Aspect) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Herbaceous cover (%)")




# Change in reproductive culms --------------------------------------------

## Repro change: All combined ---------------------------------------------

# Repro change: Linear regression by Prev_year_precip
repro.change.all.lm <- culm.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  xlab("Prev year precip (mm)") +
  ggtitle("Change in reproductive culm count") +
  labs(y = expression(Delta ~ "Reproductive culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")  
repro.change.all.lm

# Repro change: linear regression by buffelgrass density (change)
repro.change.all.bgden.lm <- culm.change %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ paste("Density (individuals /  ", m^2, ")")),
       title = "Change in reproductive culm count vs. plot density")
repro.change.all.bgden.lm

# Repro change: linear regression by shrub cover (change)
repro.change.all.shrub.lm <- culm.change %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in reproductive culm count vs. plot shrub cover")
repro.change.all.shrub.lm

# Repro change: linear regression by shrub cover (change) and previous year precipitation
repro.change.shrub.prevprecip.lm <- culm.change %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Reproductive_culms)) +
  geom_point(aes(color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in reproductive culm count vs. plot shrub cover")
repro.change.shrub.prevprecip.lm

# Repro change: linear regression by herb cover (change)
repro.change.all.herb.lm <- culm.change %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ "Native grass and forb cover (%)"),
       title = "Change in reproductive culm count vs. plot herb cover")
repro.change.all.herb.lm

# Repro change: linear regression by herb cover (change) and previous year precipitation
repro.change.herb.prevprecip.lm <- culm.change %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Reproductive_culms)) +
  geom_point(aes(color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ "Native grass and forb cover (%)"),
       title = "Change in reproductive culm count vs. plot herb cover")
repro.change.herb.prevprecip.lm

# Repro change: scatterplot buffelgrass density (change) and Prev_year_precip
repro.change.bgden.prevprecip <- culm.change %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Reproductive_culms, color = Prev_year_precip)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ paste("Density (individuals /  ", m^2, ")")),
       title = "Change in reproductive culm count vs. plot density") 
repro.change.bgden.prevprecip

# Repro change: linear regression buffelgrass density (change) and Prev_year_precip
repro.change.bgden.prevprecip.lm <- culm.change %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Reproductive_culms)) +
  geom_point(aes( color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = expression(Delta ~ paste("Density (individuals /  ", m^2, ")")),
       title = "Change in reproductive culm count vs. plot density") 
repro.change.bgden.prevprecip.lm


# Repro change: scatterplot PlotSlope and Prev_year_precip
repro.change.plotslope.prevprecip <- culm.change %>% 
  ggplot(aes(x = PlotSlope, y = Change_Reproductive_culms, color = Prev_year_precip)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Reproductive culm count"),
       x = "Plot slope (degrees)",
       title = "Change in reproductive culm count vs. plot slope") 
repro.change.plotslope.prevprecip

# Repro change: linear regression with buffelgrass cover (change)
culm.change %>% 
  ggplot(aes(x = Change_BGCover, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") 



## Repro change: By aspect ------------------------------------------------

# Repro change: Aspect, all conditions (boxplot)
repro.change.all.aspect <- culm.change %>% 
  ggplot(aes(x = Aspect, y = Change_Reproductive_culms)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  theme_bw() +
  labs(title = "Change in reproductive culm count by aspect",
       y = expression(Delta ~ "Reproductive culm count"),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
  
repro.change.all.aspect

# Repro change: Aspect by Prev_year_precip (linear regression, all obs)
repro.change.all.aspect.lm <- culm.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_Reproductive_culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~Aspect) +
  theme_bw() +
  xlab("Prev year precip (mm)") +
  ylab("Change in reproductive culm count")
repro.change.all.aspect.lm


# Change in total culms ---------------------------------------------------

## Total change: All combined ---------------------------------------------

# Total change: Linear regression by Prev_year_precip
total.change.all.lm <- culm.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  xlab("Prev year precip (mm)") +
  ggtitle("Change in total culm count") +
  labs(y = expression(Delta ~ "Total culm count")) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")  
total.change.all.lm

# Total change: linear regression by buffelgrass density (change)
total.change.all.bgden.lm <- culm.change %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ paste("Density (individuals /  ", m^2, ")")),
       title = "Change in total culm count vs. plot density")
total.change.all.bgden.lm

# Total change: linear regression by shrub cover (change)
total.change.all.shrub.lm <- culm.change %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in total culm count vs. plot shrub cover")
total.change.all.shrub.lm

# Total change: linear regression by shrub cover (change) and previous year precipitation
total.change.shrub.prevprecip.lm <- culm.change %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_Total_Live_Culms)) +
  geom_point(aes(color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in total culm count vs. plot shrub cover")
total.change.shrub.prevprecip.lm

# Total change: linear regression by herb cover (change)
total.change.all.herb.lm <- culm.change %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in total culm count vs. plot herb cover")
total.change.all.herb.lm

# Total change: linear regression by herb cover (change) and previous year precipitation
total.change.herb.prevprecip.lm <- culm.change %>% 
  ggplot(aes(x = Change_HerbCover, y = Change_Total_Live_Culms)) +
  geom_point(aes(color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ "Native grass & forb cover (%)"),
       title = "Change in total culm count vs. plot herb cover")
total.change.herb.prevprecip.lm

# Total change: scatterplot buffelgrass density (change) and Prev_year_precip
total.change.bgden.prevprecip <- culm.change %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Total_Live_Culms, color = Prev_year_precip)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ paste("Density (individuals /  ", m^2, ")")),
       title = "Change in total culm count vs. plot density") 
total.change.bgden.prevprecip

# Total change: linear regression buffelgrass density (change) and Prev_year_precip
total.change.bgden.prevprecip.lm <- culm.change %>% 
  ggplot(aes(x = Change_BGDensity, y = Change_Total_Live_Culms)) +
  geom_point(aes(color = Prev_year_precip)) +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = expression(Delta ~ paste("Density (individuals /  ", m^2, ")")),
       title = "Change in total culm count vs. plot density") 
total.change.bgden.prevprecip.lm

# Total change: scatterplot PlotSlope and Prev_year_precip
total.change.plotslope.prevprecip <- culm.change %>% 
  ggplot(aes(x = PlotSlope, y = Change_Total_Live_Culms, color = Prev_year_precip)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1,
                      name = "Previous year \nprecip (mm)") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Total culm count"),
       x = "Plot slope (degrees)",
       title = "Change in total culm count vs. plot slope") 
total.change.plotslope.prevprecip




## Total change: By aspect ------------------------------------------------

# Total change: Aspect, all conditions (boxplot)
total.change.all.aspect <- culm.change %>% 
  ggplot(aes(x = Aspect, y = Change_Total_Live_Culms)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  theme_bw() +
  labs(title = "Change in total culm count by aspect",
       y = expression(Delta ~ "Total culm count"),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")

total.change.all.aspect

# Total change: Aspect by Prev_year_precip (linear regression, all obs)
total.change.all.aspect.lm <- culm.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_Total_Live_Culms)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~Aspect) +
  theme_bw() +
  xlab("Prev year precip (mm)") +
  ylab("Change in total culm count")
total.change.all.aspect.lm



# Change in buffelgrass density -------------------------------------------

## BG density change: All combined ----------------------------------------

# BG density change: Linear regression by Prev_year_precip
bgden.change.all.lm <- plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGDensity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Prev year precip (mm)",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       title = "Change in buffelgrass density vs. precip")
bgden.change.all.lm

# BG density change: linear regression by shrub cover (change)
bgden.change.all.shrub.lm <- plot.change %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGDensity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(paste(Delta ~ "Buffelgrass density (individuals /  ", m^2, ")")),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffegrass density vs. shrub cover change")
bgden.change.all.shrub.lm

# BG density change: linear regression by elevation
bgden.change.all.elev.lm <- plot.change %>% 
  ggplot(aes(x = Elevation_ft, y = Change_BGDensity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = "Elevation (ft)",
       title = "Change in buffelgrass density vs. elevation")
bgden.change.all.elev.lm


# BG density change: scatterplot by Plot slope and Prev_year_precip
plot.change %>% 
  ggplot(aes(x = PlotSlope, y = Change_BGDensity, color = Prev_year_precip)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1) +
  geom_hline(yintercept = 0,
             linetype = "dashed")


## BG density change: By aspect -------------------------------------------

# BG density change: Aspect, all conditions (boxplot)
bgden.change.all.aspect <- plot.change %>% 
  ggplot(aes(x = Aspect, y = Change_BGDensity)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  theme_bw() +
  labs(title = "Change in buffelgrass density by aspect",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
bgden.change.all.aspect

# BG density change: Aspect by Prev_year_precip (linear regression, all obs)
bgden.change.all.aspect.lm <- plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGDensity)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Prev year precip (mm)",
       y = expression(paste(Delta ~ "Density (individuals /  ", m^2, ")")),
       title = "Change in buffelgrass density") +
  facet_wrap(~Aspect)
bgden.change.all.aspect.lm



# Change in buffelgrass cover ---------------------------------------------

## BG cover change: All combined ------------------------------------------

# BG cover change: Linear regression by Prev_year_precip
bgcov.change.all.lm <- plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Prev year precip (mm)",
       y = expression(Delta ~ "Cover (%)"),
       title = "Change in buffelgrass cover vs. precip")
bgcov.change.all.lm

# BG cover change: Linear regression by Elevation
bgcov.change.all.elev.lm <- plot.change %>% 
  ggplot(aes(x = Elevation_ft, y = Change_BGCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Elevation (ft)",
       y = expression(Delta ~ "Cover (%)"),
       title = "Change in buffelgrass cover vs. elevation")
bgcov.change.all.elev.lm

# BG density change: linear regression by shrub cover (change)
bgcov.change.all.shrub.lm <- plot.change %>% 
  ggplot(aes(x = Change_ShrubCover, y = Change_BGCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = expression(Delta ~ "Buffelgrass cover (%)"),
       x = expression(Delta ~ "Native shrub cover (%)"),
       title = "Change in buffegrass cover vs. shrub cover change")
bgcov.change.all.shrub.lm

# BG cover change: scatterplot by Plot slope and Prev_year_precip
plot.change %>% 
  ggplot(aes(x = PlotSlope, y = Change_BGDensity, color = Prev_year_precip)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1) +
  geom_hline(yintercept = 0,
             linetype = "dashed")


# BG cover change: By aspect ----------------------------------------------

# BG cover change: Aspect, all conditions (boxplot)
bgcov.change.all.aspect <- plot.change %>% 
  ggplot(aes(x = Aspect, y = Change_BGCover)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  theme_bw() +
  labs(title = "Change in buffelgrass cover by aspect",
       y = expression(Delta ~ "Cover (%)"),
       x = NULL) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")
bgcov.change.all.aspect


# BG cover change: Aspect by Prev_year_precip (linear regression, all obs)
bgcov.change.all.aspect.lm <- plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_BGCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Prev year precip (mm)",
       y = expression(Delta ~ "Cover (%)"),
       title = "Change in buffelgrass cover") +
  facet_wrap(~Aspect)
bgcov.change.all.aspect.lm


# Change in shrub cover ---------------------------------------------------

## Shrub cover change: All combined ---------------------------------------

# Shrub cover change: Linear regression by Prev_year_precip
plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_ShrubCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Prev year precip (mm)",
       y = expression(Delta ~ "Cover (%)"),
       title = "Change in shrub cover")


## Shrub cover change: By aspect ------------------------------------------

# Shrub cover change: Aspect by Prev_year_precip (linear regression, all obs)
plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_ShrubCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Prev year precip (mm)",
       y = expression(Delta ~ "Cover (%)"),
       title = "Change in shrub cover") +
  facet_wrap(~Aspect)




# Change in herb cover ----------------------------------------------------


## Herb cover change: All combined ----------------------------------------

# Herb cover change: Linear regression by Prev_year_precip
plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_HerbCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Prev year precip (mm)",
       y = expression(Delta ~ "Cover (%)"),
       title = "Change in herbaceous cover")


## Herb cover change: By aspect -------------------------------------------

# Herb cover change: Aspect by Prev_year_precip (linear regression, all obs)
plot.change %>% 
  ggplot(aes(x = Prev_year_precip, y = Change_HerbCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(x = "Prev year precip (mm)",
       y = expression(Delta ~ "Cover (%)"),
       title = "Change in herbaceous cover") +
  facet_wrap(~Aspect)




# Write out draft figures -------------------------------------------------

## Precipitation deviation ------------------------------------------------

tiff("figures/2025-02_draft-figures/Precip-deviation_site.tiff", units = "in", height = 5, width = 7, res = 150)
precip.dev.site
dev.off()

tiff("figures/2025-02_draft-figures/Precip-deviation_aspect.tiff", units = "in", height = 5, width = 7, res = 150)
precip.dev.aspect
dev.off()


## Raw values -------------------------------------------------------------

### Reproductive culms ----------------------------------------------------

# All observations
tiff("figures/2025-02_draft-figures/Reproductive-culms_precip-dev_all_regression.tiff", 
     units = "in", height = 4, width = 6, res = 150)
repro.all.lm
dev.off()

tiff("figures/2025-02_draft-figures/Reproductive-culms_precip-dev-by-aspect_all_regression.tiff", 
     units = "in", height = 4, width = 6, res = 150)
repro.all.aspect.lm
dev.off()

# Averages
tiff("figures/2025-02_draft-figures/Reproductive-culms_precip-dev-by-aspect_avg.tiff", 
     units = "in", height = 4, width = 6, res = 150)
repro.aspect.avg
dev.off()

tiff("figures/2025-02_draft-figures/Reproductive-culms_precip-dev-by-site_avg.tiff", 
     units = "in", height = 5, width = 6, res = 150)
repro.site.avg
dev.off()


### Total culms -----------------------------------------------------------

# All observations
tiff("figures/2025-02_draft-figures/Total-culms_precip-dev_all_regression.tiff", 
     units = "in", height = 4, width = 6, res = 150)
total.all.lm
dev.off()

tiff("figures/2025-02_draft-figures/Total-culms_precip-dev-by-aspect_all_regression.tiff", 
     units = "in", height = 4, width = 6, res = 150)
total.all.aspect.lm
dev.off()

# Averages
tiff("figures/2025-02_draft-figures/Total-culms_precip-dev-by-aspect_avg.tiff", 
     units = "in", height = 4, width = 6, res = 150)
total.aspect.avg
dev.off()

tiff("figures/2025-02_draft-figures/Total-culms_precip-dev-by-site_avg.tiff", units = "in", height = 5, width = 6, res = 150)
total.site.avg
dev.off()



### Buffelgrass density ---------------------------------------------------

# All observations
tiff("figures/2025-02_draft-figures/BG-density_precip-dev-by-aspect_all_regression.tiff", 
     units = "in", height = 4, width = 6, res = 150)
bgden.all.aspect.lm
dev.off()

# Averages
tiff("figures/2025-02_draft-figures/BG-density_precip-dev-by-aspect_avg.tiff", 
     units = "in", height = 4, width = 6, res = 150)
bgden.aspect.avg
dev.off()

tiff("figures/2025-02_draft-figures/BG-density_precip-dev-by-site_avg.tiff", 
     units = "in", height = 5, width = 6, res = 150)
bgden.site.avg
dev.off()


### Buffelgrass cover -----------------------------------------------------

# All observations
tiff("figures/2025-02_draft-figures/BG-cover_precip-dev-by-aspect_all_regression.tiff", 
     units = "in", height = 4, width = 6, res = 150)
bgcov.all.aspect.lm
dev.off()

# Averages
tiff("figures/2025-02_draft-figures/BG-cover_precip-dev-by-site_avg.tiff", 
     units = "in", height = 5, width = 6, res = 150)
bgcov.site.avg
dev.off()


## Change in value --------------------------------------------------------

### Repro change ----------------------------------------------------------

# Repro change vs. BG density change
tiff("figures/2025-03_draft-figures/Repro-change_by-BG-density-change_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.change.all.bgden.lm
dev.off()

# Repro change vs. BG density change by Prev_year_precip
tiff("figures/2025-03_draft-figures/Repro-change_by-BG-density-change-and-prev-year-precip.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.change.bgden.prevprecip
dev.off()

# Repro change vs. BG density change by Prev_year_precip (linear regression)
tiff("figures/2025-03_draft-figures/Repro-change_by-BG-density-change-and-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.change.bgden.prevprecip.lm
dev.off()

# Repro change vs. PlotSlope by Prev_year_precip
tiff("figures/2025-03_draft-figures/Repro-change_by-plot-slope-and-prev-year-precip.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.change.plotslope.prevprecip
dev.off()

# Repro change vs. shrub cover
tiff("figures/2025-03_draft-figures/Repro-change_by-shrub-cover_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.change.all.shrub.lm
dev.off()

# Repro change vs. shrub cover by Prev_year_precip (linear regression)
tiff("figures/2025-03_draft-figures/Repro-change_by-shrub-cover-and-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.change.shrub.prevprecip.lm
dev.off()

# Repro change vs. herb cover
tiff("figures/2025-03_draft-figures/Repro-change_by-herb-cover_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.change.all.herb.lm
dev.off()

# Repro change vs. herb cover by Prev_year_precip (linear regression)
tiff("figures/2025-03_draft-figures/Repro-change_by-herb-cover-and-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.change.herb.prevprecip.lm
dev.off()

# Repro change by aspect, all conditions
tiff("figures/2025-03_draft-figures/Repro-change_by-aspect_boxplot.tiff",
     units = "in", height = 4, width = 6, res = 150)
repro.change.all.aspect
dev.off()

# Repro change by Prev_year_precip
tiff("figures/2025-03_draft-figures/Repro-change_by-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
repro.change.all.lm
dev.off()


### Total change ----------------------------------------------------------

# Total change vs. BG density change
tiff("figures/2025-03_draft-figures/Total-change_by-BG-density-change_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.change.all.bgden.lm
dev.off()

# Total change vs. BG density change by Prev_year_precip
tiff("figures/2025-03_draft-figures/Total-change_by-BG-density-change-and-prev-year-precip.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.change.bgden.prevprecip
dev.off()

# Total change vs. BG density change by Prev_year_precip (linear regression)
tiff("figures/2025-03_draft-figures/Total-change_by-BG-density-change-and-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.change.bgden.prevprecip.lm
dev.off()

# Total change vs. PlotSlope by Prev_year_precip
tiff("figures/2025-03_draft-figures/Total-change_by-plot-slope-and-prev-year-precip.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.change.plotslope.prevprecip
dev.off()

# Total change vs. shrub cover
tiff("figures/2025-03_draft-figures/Total-change_by-shrub-cover_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.change.all.shrub.lm
dev.off()

# Total change vs. shrub cover by Prev_year_precip (linear regression)
tiff("figures/2025-03_draft-figures/Total-change_by-shrub-cover-and-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.change.shrub.prevprecip.lm
dev.off()

# Total change vs. herb cover
tiff("figures/2025-03_draft-figures/Total-change_by-herb-cover_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.change.all.herb.lm
dev.off()

# Total change vs. herb cover by Prev_year_precip (linear regression)
tiff("figures/2025-03_draft-figures/Total-change_by-herb-cover-and-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.change.herb.prevprecip.lm
dev.off()

# Total change by aspect, all conditions
tiff("figures/2025-03_draft-figures/Total-change_by-aspect_boxplot.tiff",
     units = "in", height = 4, width = 6, res = 150)
total.change.all.aspect
dev.off()

# Total change by Prev_year_precip
tiff("figures/2025-03_draft-figures/Total-change_by-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
total.change.all.lm
dev.off()



### Buffelgrass density change --------------------------------------------

# BG density change vs. Prev_year_precip
tiff("figures/2025-03_draft-figures/BG-density-change_by-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.change.all.lm
dev.off()

# BG density change vs. elevation
tiff("figures/2025-03_draft-figures/BG-density-change_by-elevation_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.change.all.elev.lm
dev.off()

# BG density change vs. shrub cover 
tiff("figures/2025-03_draft-figures/BG-density-change_by-shrub-cover_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.change.all.shrub.lm
dev.off()

# BG density change by aspect, all conditions
tiff("figures/2025-03_draft-figures/BG-density-change_by-aspect_boxplot.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgden.change.all.aspect
dev.off()



### Buffelgrass cover change ----------------------------------------------

# BG cover change vs. Prev_year_precip
tiff("figures/2025-03_draft-figures/BG-cover-change_by-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.change.all.lm
dev.off()

# BG cover change vs. elevation
tiff("figures/2025-03_draft-figures/BG-cover-change_by-elevation_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgcov.change.all.elev.lm
dev.off()

# BG cover change by aspect, all conditions
tiff("figures/2025-03_draft-figures/BG-cover-change_by-aspect_boxplot.tiff",
     units = "in", height = 4, width = 6, res = 150)
bgcov.change.all.aspect
dev.off()

=======
>>>>>>> fcad81967c3c77f7b1a8337309da38f0446e4630



### Buffelgrass density change --------------------------------------------

# BG density change by Prev_year_precip
tiff("figures/2025-03_draft-figures/BG-density-change_by-prev-year-precip_regression.tiff",
     units = "in", height = 4, width = 5, res = 150)
bgden.change.all.lm
dev.off()

save.image("RData/05_draft-figs.RData")
