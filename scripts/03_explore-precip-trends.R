# Created: 2024-07-29
# Last updated: 2024-07-29

# Purpose: Explore precip trends. Compare actual precip values (from PRISM daily values, see 02.1.R)
#   with 30-year normals. Actual precip (Prev_year_precip) is recorded as the total precip from 
#  the year preceding the monitoring event. Normals (MAP, MAT) also represent annual values,
#   so the time interval is the same, and they are therefore comparable.


library(tidyverse)
library(readxl)
library(scales)

# Load data ---------------------------------------------------------------

prism.dat.raw <- read_csv("data/cleaned/02_monitoring-info-with-PRISM-data_clean.csv")


# Add Site_Transect col ---------------------------------------------------

prism.dat <- prism.dat.raw |> 
  mutate(Site_Transect = paste0(Site, "_", Transect))
prism.dat <- prism.dat[, c(1:9, 13, 10:12)]


# Add Perc_deviation col --------------------------------------------------

# Percent deviation of actual values from normals
prism.dat <- prism.dat |> 
  mutate(Perc_deviation = (Prev_year_precip - MAP) / MAP,
         Deviation_mm = Prev_year_precip - MAP)


# Graph -------------------------------------------------------------------

# Percent deviation
prism.dat |> 
  ggplot(aes(x = Date, y = Perc_deviation)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site_Transect) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

# Deviation in mm ppt
prism.dat |> 
  ggplot(aes(x = Date, y = Deviation_mm)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Site_Transect) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

