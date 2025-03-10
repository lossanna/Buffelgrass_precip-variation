# Created: 2024-09-23
# Updated: 2025-03-10

# Purpose: Graph culm, cover and density response to precip variation.

# Have to use average precip dev to represent each year, because the actual precip dev
#   is slightly different by sampling date.


library(tidyverse)
library(scales)
library(viridis)

# Load data ---------------------------------------------------------------

dat <- read_csv("data/cleaned/04_demography-data_clean.csv")


# Data wrangling ----------------------------------------------------------

# Create df of average and SE to make line graph for culm count
culm.avg.site <- dat %>% 
  group_by(Year, StudyYear, Site) %>% 
  summarise(repro_avg = mean(Reproductive_culms),
            repro_se = sd(Reproductive_culms) / sqrt(n()),
            total_avg = mean(Total_Live_Culms),
            total_se = sd(Total_Live_Culms) / sqrt(n()),
            Perc_dev_avg = mean(Perc_dev),
            .groups = "keep")

culm.avg.site.aspect <- dat %>% 
  group_by(Year, StudyYear, Site, Aspect) %>% 
  summarise(repro_avg = mean(Reproductive_culms),
            repro_se = sd(Reproductive_culms) / sqrt(n()),
            total_avg = mean(Total_Live_Culms),
            total_se = sd(Total_Live_Culms) / sqrt(n()),
            Perc_dev_avg = mean(Perc_dev),
            .groups = "keep")

culm.avg.aspect <- dat %>% 
  group_by(Year, Aspect) %>% 
  summarise(repro_avg = mean(Reproductive_culms),
            repro_se = sd(Reproductive_culms) / sqrt(n()),
            total_avg = mean(Total_Live_Culms),
            total_se = sd(Total_Live_Culms) / sqrt(n()),
            Perc_dev_avg = mean(Perc_dev),
            .groups = "keep")


# Separate out plot-level data
dat.plot <- dat %>% 
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm) %>% 
  distinct(.keep_all = TRUE)

# Create df of average and SE to make line graph for density & cover
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


# Repro: Plot slope and Perc_dev
dat %>% 
  ggplot(aes(x = PlotSlope, y = Reproductive_culms, color = Perc_dev)) +
  geom_point() +
  theme_bw() +
  scale_color_viridis(option = "viridis", direction = -1)


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


# Total: Plot slope and Perc_dev
dat %>% 
  ggplot(aes(x = PlotSlope, y = Total_Live_Culms, color = Perc_dev)) +
  geom_point() +
  scale_color_viridis(option = "viridis", direction = -1)


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

# Repro: Aspect by Perc_dev (linear regression, all obs)
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

# BG density: Plot slope and Perc_dev
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

plot.avg.site %>% 
  ggplot(aes(x = Year, y = shrub_avg, color = Site)) +
  geom_point() +
  geom_line() +
  theme_bw()
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

# By site and aspect (line graph)
plot.avg.site.aspect %>% 
  ggplot(aes(x = Year, y = shrub_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw()
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

plot.avg.site %>% 
  ggplot(aes(x = Year, y = forb_avg, color = Site)) +
  geom_point() +
  geom_line() +
  theme_bw()
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

# By site and aspect (line graph)
plot.avg.site.aspect %>% 
  ggplot(aes(x = Year, y = forb_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw()
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

dat %>% 
  ggplot(aes(x = Perc_dev, y = NGCover)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() +
  facet_wrap(~Aspect) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Shrub cover (%)")

plot.avg.site %>% 
  ggplot(aes(x = Year, y = ng_avg, color = Site)) +
  geom_point() +
  geom_line() +
  theme_bw()
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

# By site and aspect (line graph)
plot.avg.site.aspect %>% 
  ggplot(aes(x = Year, y = ng_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw()
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




# Write out draft figures -------------------------------------------------

## Precipitation deviation ------------------------------------------------

tiff("figures/2025-02_draft-figures/Precip-deviation_site.tiff", units = "in", height = 5, width = 7, res = 150)
precip.dev.site
dev.off()

tiff("figures/2025-02_draft-figures/Precip-deviation_aspect.tiff", units = "in", height = 5, width = 7, res = 150)
precip.dev.aspect
dev.off()


## Reproductive culms -----------------------------------------------------

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


## Total culms ------------------------------------------------------------

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



## Buffelgrass density ----------------------------------------------------

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


## Buffelgrass cover ------------------------------------------------------

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


save.image("RData/05_draft-figs.RData")
