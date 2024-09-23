# Created: 2024-09-23
# Updated: 2024-09-23

# Purpose: Graph culm, cover and density response to precip variation.


library(readxl)
library(tidyverse)
library(scales)

# Load data ---------------------------------------------------------------

dat <- read_csv("data/cleaned/04_demography-data_clean.csv")


# Draft figs --------------------------------------------------------------

dat |> 
  ggplot(aes(x = Perc_dev, y = Reproductive_culms)) +
  geom_point() +
  scale_x_continuous(labels = scales::percent) +
  facet_wrap(~Aspect)
