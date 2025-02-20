# Created: 2024-09-23
# Updated: 2025-02-20

# Purpose: Graph culm, cover and density response to precip variation.

# Have to use average precip dev to represent each year, because the actual precip dev
#   is slightly different by sampling date.


library(readxl)
library(tidyverse)
library(scales)

# Load data ---------------------------------------------------------------

dat <- read_csv("data/cleaned/04_demography-data_clean.csv")


# Data wrangling ----------------------------------------------------------

# Create df of average and SE to make line graph for culm count
culm.avg.site <- dat |> 
  group_by(Year, StudyYear, Site) |> 
  summarise(repro_avg = mean(Reproductive_culms),
            repro_se = sd(Reproductive_culms) / sqrt(n()),
            total_avg = mean(Total_Live_Culms),
            total_se = sd(Total_Live_Culms) / sqrt(n()),
            Perc_dev_avg = mean(Perc_dev),
            .groups = "keep")

culm.avg.site.aspect <- dat |> 
  group_by(Year, StudyYear, Site, Aspect) |> 
  summarise(repro_avg = mean(Reproductive_culms),
            repro_se = sd(Reproductive_culms) / sqrt(n()),
            total_avg = mean(Total_Live_Culms),
            total_se = sd(Total_Live_Culms) / sqrt(n()),
            Perc_dev_avg = mean(Perc_dev),
            .groups = "keep")


# Separate out plot-level data
dat.plot <- dat |> 
  select(-Plant_ID, -Vegetative_culms, -Reproductive_culms, -Total_Live_Culms, -Longestleaflength_cm) |> 
  distinct(.keep_all = TRUE)

# Create df of average and SE to make line graph for density & cover
plot.avg.site <- dat.plot |> 
  group_by(Year, StudyYear, Site) |> 
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

plot.avg.site.aspect <- dat.plot |> 
  group_by(Year, StudyYear, Site, Aspect) |> 
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


# Precip deviation --------------------------------------------------------

precip.dev <- plot.avg.site |> 
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
precip.dev

plot.avg.site.aspect |> 
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

# By aspect (scatterplot, all obs)
dat |> 
  ggplot(aes(x = Perc_dev, y = Reproductive_culms)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent) +
  facet_wrap(~Aspect) +
  theme_bw()

# By site (line graph)
culm.avg.site |> 
  ggplot(aes(x = Year, y = repro_avg, color = Site)) +
  geom_point() +
  geom_line() +
  theme_bw()
repro.site <- culm.avg.site |> 
  ggplot(aes(x = Perc_dev_avg, y = repro_avg, color = Site)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = repro_avg - repro_se, ymax = repro_avg + repro_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("No. of reproductive culms")
repro.site

# By site and aspect (line graph)
culm.avg.site.aspect |> 
  ggplot(aes(x = Year, y = repro_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw()
culm.avg.site.aspect |> 
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

# By aspect (scatterplot, all obs)
dat |> 
  ggplot(aes(x = Perc_dev, y = Total_Live_Culms)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent) +
  facet_wrap(~Aspect) +
  theme_bw()

# By site (line graph)
culm.avg.site |> 
  ggplot(aes(x = Year, y = total_avg, color = Site)) +
  geom_point() +
  geom_line() +
  theme_bw()
total.site <- culm.avg.site |> 
  ggplot(aes(x = Perc_dev_avg, y = total_avg, color = Site)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = total_avg - total_se, ymax = total_avg + total_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Total number of culms")
total.site

# By site and aspect (line graph)
culm.avg.site.aspect |> 
  ggplot(aes(x = Year, y = total_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw()
culm.avg.site.aspect |> 
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

# By site (line graph)
plot.avg.site |> 
  ggplot(aes(x = Year, y = bgden_avg, color = Site)) +
  geom_point() +
  geom_line() +
  theme_bw()
bgden.site <- plot.avg.site |> 
  ggplot(aes(x = Perc_dev_avg, y = bgden_avg, color = Site)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = bgden_avg - bgden_se, ymax = bgden_avg + bgden_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Precip deviation from average",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) 
bgden.site

# By site and aspect (line graph)
plot.avg.site.aspect |> 
  ggplot(aes(x = Year, y = bgden_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw()
plot.avg.site.aspect |> 
  ggplot(aes(x = Perc_dev_avg, y = bgden_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = bgden_avg - bgden_se, ymax = bgden_avg + bgden_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~Site) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Precip deviation from average",
       y = expression(paste("Density (individuals /  ", m^2, ")"))) 


# Buffelgrass cover -------------------------------------------------------

plot.avg.site |> 
  ggplot(aes(x = Year, y = bgcov_avg, color = Site)) +
  geom_point() +
  geom_line() +
  theme_bw()
bgcov.site <- plot.avg.site |> 
  ggplot(aes(x = Perc_dev_avg, y = bgcov_avg, color = Site)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = bgcov_avg - bgcov_se, ymax = bgcov_avg + bgcov_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Buffelgrass cover (%)")
bgcov.site

# By site and aspect (line graph)
plot.avg.site.aspect |> 
  ggplot(aes(x = Year, y = bgcov_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw()
plot.avg.site.aspect |> 
  ggplot(aes(x = Perc_dev_avg, y = bgcov_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = bgcov_avg - bgcov_se, ymax = bgcov_avg + bgcov_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  facet_wrap(~Site) +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from average") +
  ylab("Buffelgrass cover")    


# Shrub cover -------------------------------------------------------------

plot.avg.site |> 
  ggplot(aes(x = Year, y = shrub_avg, color = Site)) +
  geom_point() +
  geom_line() +
  theme_bw()
plot.avg.site |> 
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
plot.avg.site.aspect |> 
  ggplot(aes(x = Year, y = shrub_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw()
plot.avg.site.aspect |> 
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

plot.avg.site |> 
  ggplot(aes(x = Year, y = forb_avg, color = Site)) +
  geom_point() +
  geom_line() +
  theme_bw()
plot.avg.site |> 
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
plot.avg.site.aspect |> 
  ggplot(aes(x = Year, y = forb_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw()
plot.avg.site.aspect |> 
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

plot.avg.site |> 
  ggplot(aes(x = Year, y = ng_avg, color = Site)) +
  geom_point() +
  geom_line() +
  theme_bw()
plot.avg.site |> 
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
plot.avg.site.aspect |> 
  ggplot(aes(x = Year, y = ng_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site) +
  theme_bw()
plot.avg.site.aspect |> 
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




# Write out draft figures -------------------------------------------------

tiff("figures/2025-02_draft-figures/Precip-deviation.tiff", units = "in", height = 5, width = 7, res = 150)
precip.dev
dev.off()

tiff("figures/2025-02_draft-figures/Reproductive-culms_precip-dev-by-site.tiff", units = "in", height = 5, width = 6, res = 150)
repro.site
dev.off()

tiff("figures/2025-02_draft-figures/Total-culms_precip-dev-by-site.tiff", units = "in", height = 5, width = 6, res = 150)
total.site
dev.off()

tiff("figures/2025-02_draft-figures/BG-density_precip-dev-by-site.tiff", units = "in", height = 5, width = 6, res = 150)
bgden.site
dev.off()

tiff("figures/2025-02_draft-figures/BG-cover_precip-dev-by-site.tiff", units = "in", height = 5, width = 6, res = 150)
bgcov.site
dev.off()


save.image("RData/05_draft-figs.RData")
