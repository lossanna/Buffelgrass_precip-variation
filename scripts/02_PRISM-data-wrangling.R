# Created: 2024-07-25
# Last updated: 2024-07-29

# Purpose: Wrangle PRISM data to add Prev_year_precip, MAT, and MAP cols to monitoring info.

# Sometimes plots within the same transect were sampled on different days (a transect was not sampled
#   in its totality on a single day). In this case, there are multiple CSVs of daily values for
#   a single transect of the same year. However, they are all assigned the same normals. Normals
#   are unique to site and transect only.


library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

prism.daily.raw <- read_xlsx("data/data-wrangling-intermediate/02_monitoring-events-with-PRISM-csv-file-name.xlsx",
                             sheet = "daily")

prism.normals.raw <- read_xlsx("data/data-wrangling-intermediate/02_monitoring-events-with-PRISM-csv-file-name.xlsx",
                               sheet = "normals")

monitor <- read_csv("data/cleaned/01_site-and-monitoring-info_clean.csv")


# Add complete path to prism.daily columns --------------------------------

# Add complete path to file names for daily values
prism.daily <- prism.daily.raw %>% 
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
prism.daily <- prism.daily %>%
  mutate(Prev_year_precip = map_dbl(precip_file, process_csv)) %>% 
  select(-path_beginning, -file_name, -precip_file)



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


# Combine monitoring info with PRISM cols ---------------------------------

# Combine
monitor.prism <- left_join(monitor, prism.daily)
monitor.prism$Prev_year_precip # creates NAs - something doesn't match

# See that Longitude and Latitude are slightly different, so left_join() won't work
#   (I'm not sure why this is, but I think it's how the values were rounded/stored in a csv vs xlsx?)
monitor$Latitude == prism.daily$Latitude
monitor$Longitude == prism.daily$Longitude

# Drop Lat & Long cols
prism.daily <- prism.daily %>% 
  select(-Latitude, -Longitude)
prism.normals <- prism.normals %>% 
  select(-Latitude, -Longitude)

# Attempt left_join() again
monitor.prism <- monitor.prism %>% 
  select(-Prev_year_precip) %>% 
  left_join(prism.daily) %>% 
  left_join(prism.normals)


# Write to csv ------------------------------------------------------------

write_csv(monitor.prism,
          file = "data/cleaned/02_monitoring-info-with-PRISM-data_clean.csv")

