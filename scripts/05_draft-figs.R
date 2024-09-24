# Created: 2024-09-23
# Updated: 2024-09-23

# Purpose: Graph culm, cover and density response to precip variation.


library(readxl)
library(tidyverse)
library(scales)

# Load data ---------------------------------------------------------------

dat <- read_csv("data/cleaned/04_demography-data_clean.csv")


# Data wrangling ----------------------------------------------------------

# Create df of average and SE to make line graph
culm.avg.site.aspect <- dat |> 
  group_by(Year, StudyYear, Site, Aspect) |> 
  summarise(repro_avg = mean(Reproductive_culms),
            repro_se = sd(Reproductive_culms) / sqrt(40),
            .groups = "keep")

culm.avg.percdev.aspect <- dat |> 
  group_by(Year, Site, Aspect) |> 
  summarise(repro_avg = mean(Reproductive_culms),
            repro_se = sd(Reproductive_culms) / sqrt(78),
            total_avg = mean(Total_Live_Culms),
            total_se = sd(Total_Live_Culms) / sqrt(78),
            Perc_dev_avg = mean(Perc_dev),
            .groups = "keep")
  
  

# Draft figs --------------------------------------------------------------

# By aspect
dat |> 
  ggplot(aes(x = Perc_dev, y = Reproductive_culms)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent) +
  facet_wrap(~Aspect)


# Line graph
culm.avg.site.aspect |> 
  ggplot(aes(x = Year, y = repro_avg)) +
  geom_point(aes(color = Aspect)) +
  geom_line(aes(color = Aspect)) +
  facet_wrap(~Site)

culm.avg.percdev.aspect |> 
  ggplot(aes(x = Perc_dev_avg, y = repro_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = repro_avg - repro_se, ymax = repro_avg + repro_se)) +
  facet_wrap(~Site) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from normals") +
  ylab("No. of reproductive culms")

culm.avg.percdev.aspect |> 
  ggplot(aes(x = Perc_dev_avg, y = total_avg, color = Aspect)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = total_avg - total_se, ymax = total_avg + total_se)) +
  facet_wrap(~Site) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  xlab("Precip deviation from normals") +
  ylab("Total number of culms")




    
