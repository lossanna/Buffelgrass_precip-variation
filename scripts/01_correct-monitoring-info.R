# Created: 2024-07-25
# Last updated: 2024-07-25

# Purpose: Make list of monitoring events with site information (coordinates, elevation).

# GPS coordinates are specific only to transect, not plot.

# Plots were sampled once a year in fall. Transects were not always sampled in their entirety
#   within a single day (2-3 plots per transect), but plots were sampled in their entirety
#   within a single day.  Hence, the number of SiteDateTransectIDs is less than the number of 
#   SiteDatePlotIDs.

# Tumamoc Hill: 11 transects, sampled 3 years.
# Kinney Hill: 7 transects, sampled 2 years.
# Apache Peak: 6 transects, sampled 3 years.
# Loma Verde: 3 transects, sampled 3 years.


library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

coord.raw <- read_xlsx("data/raw/Buffelgrass Demography GPS coordinates.xlsx")
dat.raw <- read_xlsx("data/raw/2024-09_LO_Buffelgrass-culm-demography_master.xlsx", sheet = "Demography")


# Convert coordinates to decimal degrees ----------------------------------

coord <- coord.raw |> 
  separate(N, c("N_degree", "N_min"), sep = "\u00B0") |> # separate cols on degree sign
  separate(W, c("W_degree", "W_min"), sep = "\u00B0") |> 
  mutate(N_min = str_sub(N_min, 2)) |>  # remove leading space
  mutate(W_min = str_sub(W_min, 2)) |> 
  mutate(N_min = str_sub(N_min, 1, -2)) |>  # remove minute symbol
  mutate(W_min = str_sub(W_min, 1, -2)) |> 
  mutate(across(c(N_degree, N_min, W_degree, W_min), as.numeric)) |> 
  mutate(N_min = N_min / 60) |>  # convert to decimal degrees
  mutate(W_min = W_min / 60) |> 
  mutate(Latitude = N_degree + N_min) |> 
  mutate(Longitude = W_degree + W_min) |> 
  mutate(Longitude = -Longitude) |> 
  select(Site, Transect, Latitude, Longitude, Elevation_ft) |> 
  mutate(Site = str_replace_all(Site, " ", "")) # remove spaces from Site so names will match


# Create list of monitoring events ----------------------------------------

# By Date & Transect
monitor <- dat.raw |> 
  select(Date, Year, StudyYear, Site, Transect) |> 
  distinct(.keep_all = TRUE)

# Assign ID
monitor <- monitor |> 
  mutate(SiteDateTransectID = 1:nrow(monitor))


# By Date & Plot
monitor.plot <- dat.raw |> 
  select(Date, Year, StudyYear, Site, Transect, Plot) |> 
  distinct(.keep_all = TRUE)

# Assign IDs
monitor.plot <- monitor.plot |> 
  mutate(SiteDatePlotID = 1:nrow(monitor.plot))
monitor.plot <- left_join(monitor, monitor.plot)


# Add coordinate info to Date-Transect
monitor <- left_join(coord, monitor)
monitor <- monitor |> 
  select(Site, Date, Year, StudyYear, Latitude, Longitude, Elevation_ft, SiteDateTransectID, Transect)





# Write to CSV ------------------------------------------------------------

write_csv(monitor,
          file = "data/cleaned/01_site-and-monitoring-info_clean.csv")

write_csv(monitor.plot,
          file = "data/cleaned/01_SiteDatePlotID_clean.csv")
