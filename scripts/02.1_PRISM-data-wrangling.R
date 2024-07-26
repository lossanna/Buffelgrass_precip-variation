# Created: 2024-07-25
# Last updated: 2024-07-25

# Purpose:

# Sometimes plots within the same transect were sampled on different days (a transect was not sampled
#   in its totality on a single day). In this case, there are multiple CSVs of daily values for
#   a single transect of the same year. However, they are all assigned the same normals. Normals
#   are unique to site and transect only.


library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

prism.daily.raw <- read_xlsx("data/data-wrangling-intermediate/02.1_monitoring-events-with-PRISM-csv-file-name.xlsx",
                             sheet = "daily")

prism.normals.raw <- read_xlsx("data/data-wrangling-intermediate/02.1_monitoring-events-with-PRISM-csv-file-name.xlsx",
                               sheet = "normals")