# Created: 2024-07-25
# Last updated: 2025-04-16

# Purpose: Wrangle PRISM data to add Prev_year_precip, MAT, and MAP cols to monitoring info.

# Sometimes plots within the same transect were sampled on different days (a transect was not sampled
#   in its totality on a single day). In this case, there are multiple CSVs of daily values for
#   a single transect of the same year. However, they are all assigned the same normals. Normals
#   are unique to site and transect only.

# As of March 2025, PRISM released the 800m resolution daily data for free, so I downloaded that to
#   compare and it is basically the same. Will just keep using 4km data going forward.

library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

prism.daily.4km.raw <- read_xlsx("data/data-wrangling-intermediate/02_monitoring-events-with-PRISM-csv-file-name.xlsx",
                             sheet = "daily_4km")
prism.daily.800m.raw <- read_xlsx("data/data-wrangling-intermediate/02_monitoring-events-with-PRISM-csv-file-name.xlsx",
                                 sheet = "daily_800m")

prism.normals.raw <- read_xlsx("data/data-wrangling-intermediate/02_monitoring-events-with-PRISM-csv-file-name.xlsx",
                               sheet = "normals")

monitor <- read_csv("data/cleaned/01_site-and-monitoring-info_clean.csv")


# Add complete path to prism.daily columns --------------------------------

# Add complete path to file names for daily values
prism.daily.4km <- prism.daily.4km.raw %>% 
  mutate(precip_file = paste0("data/prism-dat/", path_beginning, "/", file_name))
prism.daily.800m <- prism.daily.800m.raw %>% 
  mutate(precip_file = paste0("data/prism-dat/", path_beginning, "/", file_name))

# Add complete path for normals
prism.normals <- prism.normals.raw %>% 
  mutate(precip_file = paste0("data/prism-dat/", path_beginning, "/", file_name))


# Add Prev_year_precip col based on PRISM ---------------------------------

# Add column for total precipitation during the year leading up to the monitoring events based
#   on PRISM data. I downloaded a file from PRISM based on the site-transect GPS coordinates
#   and monitoring date, which can be used to calculate the precip values for each SiteDateTransectID.

# Write a function to process the CSVs, and then iterative over for each row.


# Write function to process CSVs
process_csv <- function(file.names) {
  
  # Read the CSV file
  data <- read.csv(file.names, skip = 10)
  
  # Rename columns
  colnames(data) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
  
  # Calculate the sum of the desired column
  sum_value <- sum(data$ppt_mm)
  
  return(sum_value)
}

# Use purrr::map to apply the function to each row of df
prism.daily.4km <- prism.daily.4km %>%
  mutate(Prev_year_precip = map_dbl(precip_file, process_csv)) %>% 
  select(-path_beginning, -file_name, -precip_file) %>% 
  arrange(SiteDateTransectID)

prism.daily.800m <- prism.daily.800m %>%
  mutate(Prev_year_precip = map_dbl(precip_file, process_csv)) %>% 
  select(-path_beginning, -file_name, -precip_file) %>% 
  arrange(SiteDateTransectID)


# Compare 4km and 800m
prism.compare <- prism.daily.4km %>% 
  rename(Prev_year_precip_4km = Prev_year_precip) %>% 
  left_join(prism.daily.800m) 
prism.compare <- prism.compare %>% 
  mutate(difference = Prev_year_precip_4km - Prev_year_precip)
summary(prism.compare$difference) # lol it is no different
summary(prism.compare$Prev_year_precip_4km)
summary(prism.compare$Prev_year_precip)


# Extract 30-year normals for MAT & MAP -----------------------------------

# Write function to process CSVs
# MAP
ppt_normals <- function(file.names) {
  
  # Read the CSV file
  data <- read.csv(file.names, skip = 10)
  
  # Rename columns
  colnames(data) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
  
  # Extract annual value
  annual <- data$ppt[13]
  
  return(annual)
}

# MAT
temp_normals <- function(file.names) {
  
  # Read the CSV file
  data <- read.csv(file.names, skip = 10)
  
  # Rename columns
  colnames(data) <- c("Date", "ppt_mm", "tmin", "tmean", "tmax")
  
  # Extract annual value
  annual <- data$tmean[13]
  
  return(annual)
}

# Use purrr::map to apply the function to each row of df
prism.normals <- prism.normals %>%
  mutate(MAP = map_dbl(precip_file, ppt_normals)) %>%
  mutate(MAT = map_dbl(precip_file, temp_normals)) %>% 
  select(-path_beginning, -file_name, -precip_file)



# Check that monitoring info matches --------------------------------------

monitor.check <- prism.daily.4km %>% 
  left_join(monitor)


# Combine normals with daily values ---------------------------------------

monitor.prism.4km <- prism.daily.4km %>% 
  left_join(prism.normals)

monitor.prism.800m <- prism.daily.800m %>% 
  left_join(prism.normals)



# Write to csv ------------------------------------------------------------

write_csv(monitor.prism.4km,
          file = "data/cleaned/02_monitoring-info-with-PRISM-data_clean.csv")

write_csv(monitor.prism.800m,
          file = "data/cleaned/02_monitoring-info-with-PRISM-data-800m_clean.csv")

