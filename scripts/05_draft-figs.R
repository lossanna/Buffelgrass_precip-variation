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
            repro_se = sd(Reproductive_culms) / 40,
            .groups = "keep")

culm.avg.percdev.aspect <- dat |> 
  group_by(Site, Aspect, Perc_dev) |> 
  summarise(repro_avg = mean(Reproductive_culms),
            repro_se = sd(Reproductive_culms) / 78,
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
  ggplot(aes(x = Perc_dev, y = repro_avg)) +
  geom_point(aes(color = Aspect)) +
  geom_line(aes(color = Aspect)) +
  facet_wrap(~Site)

culm.avg.percdev.aspect |> 
  filter(Aspect == "E") |> 
  ggplot(aes(x = Perc_dev, y = repro_avg)) +
  geom_point(aes(color = Aspect)) +
  geom_line(aes(color = Aspect)) +
  facet_wrap(~Site)

culm.avg.percdev.aspect |> 
  filter(Aspect == "E") |> 
  ggplot(aes(x = Perc_dev, y = repro_avg)) +
  geom_bar(aes(color = Aspect), stat = "identity") +
  facet_wrap(~Site)
    
