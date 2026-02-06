# Created: 2026-02-04
# Updated: 2026-02-05

# Purpose: Do some extra data cleaning to see that one Plant_ID was entered wrong (now corrected).
#   Ultimately create table of culm change data with initial conditions for BG and native veg.

library(tidyverse)

# Load data ---------------------------------------------------------------

dat.raw <- read_csv("data/cleaned/04_demography-data_clean.csv")


# Begin creating df of initial conditions ---------------------------------

# Create version of initial conditions for join
initial <- dat.raw %>% 
  filter(StudyYear == 1) %>% 
  select(Site, Transect, Plot, Plant_ID, BGCover, BGDensity, ShrubCover, HerbCover)

# Check for duplicate rows
count(initial, Plant_ID) %>% 
  arrange(desc(n)) # there are 2 rows for 579 for some reason

# Investigate duplicate row
plant579 <- dat.raw %>% 
  filter(Plant_ID == 579 & StudyYear == 1) # the culm counts are different


# Check number of rows for each Plant_ID ----------------------------------

# Check for other duplicates in demography data
plantid.count <- dat.raw %>% 
  group_by(Site, Plant_ID) %>% 
  summarise(n = n(),
            .groups = "keep")

# Apache Peak, Loma Verde, Tumamoc Hill should have 3; Kinney Hill should have 2
#   Apache Peak
apachepeak.inspect.id <- plantid.count %>% 
  filter(Site == "ApachePeak", n != 3)
apachepeak.inspect <- dat.raw %>% 
  filter(Plant_ID %in% apachepeak.inspect.id$Plant_ID) 
#       notes in original data say they could not locate tag in Year 1 (2021)

#   Loma Verde
lomaverde.inspect.id <- plantid.count %>% 
  filter(Site == "LomaVerde", n != 3)
lomaverde.inspect <- dat.raw %>% 
  filter(Plant_ID %in% lomaverde.inspect.id$Plant_ID)
#       notes in original data say nail was missing for Year 1 for 693
#       original data has only one row for 753; it was for Year 3 and it was dead

#   Tumamoc Hill
tumamoc.inspect.id <- plantid.count %>% 
  filter(Site == "TumamocHill", n != 3)
#     43-72 are all of the same transect that must have been discontinued after Year 2
#     188-217 are all of the same transect that must have been discontinued after Year 2
#     all the ones with only 1 will have to be removed

#   Kinney Hill
kinneyhill.inspect.id <- plantid.count %>% 
  filter(Site == "KinneyHill" & n != 2)
kinneyhill.inspect <- dat.raw %>% 
  filter(Plant_ID %in% kinneyhill.inspect.id$Plant_ID)
#     idk 577 is just missing from Year 1; only has one row in original data
#     579 is the one that has two rows
#     one of the instances of 579 is probably 577
# I checked the original scanned datasheet for KH 2021 and the one with 2 repro & 
#   17 total is actually plant 577

# Create fixed row for 577
plant577.fix <- dat.raw %>% 
  filter(StudyYear == 1, Plant_ID == 579,
         Total_Live_Culms == 17, Reproductive_culms == 2) %>% 
  mutate(Plant_ID = 577)

# Remove incorrect row and add in fixed one
dat <- dat.raw %>% 
  mutate(raw.row = 1:nrow(dat.raw))
row.rm <- dat %>% 
  filter(StudyYear == 1, Plant_ID == 579,
         Total_Live_Culms == 17, Reproductive_culms == 2)
dat <- dat %>% 
  filter(raw.row != row.rm$raw.row) %>% 
  select(-raw.row) %>% 
  bind_rows(plant577.fix)


# Remove survival_perc column (see 11.2.R for correct survival data)
dat <- dat %>% 
  select(-survival_perc)

# Save intermediate
dat1 <- dat


# Recalculate culm change -------------------------------------------------

culm.change <- dat %>%
  arrange(Plant_ID, Year) %>%
  group_by(Plant_ID) %>%
  mutate(Change_Reproductive_culms = Reproductive_culms - lag(Reproductive_culms),
         Change_Total_Live_Culms = Total_Live_Culms - lag(Total_Live_Culms)) %>% 
  mutate(Change_BGDensity = BGDensity - lag(BGDensity),
         Change_BGCover = BGCover - lag(BGCover)) %>% 
  mutate(Change_HerbCover = HerbCover - lag(HerbCover),
         Change_ShrubCover = ShrubCover - lag(ShrubCover)) %>% 
  filter(!is.na(Change_Total_Live_Culms))



# Join initial conditions with culm.change --------------------------------

# Remove duplicate row from initial caused by 577/579 mistake (mistake does not change
#   initial data, as that is plot-level)
initial.fixed <- initial %>% 
  distinct(.keep_all = TRUE)

# Create version of culm.change for join
culm.change.join <- culm.change %>% 
  select(Year, Site, Transect, Plot, Plant_ID, Prev_year_precip, PlotSlope, Aspect,
         Change_Total_Live_Culms, Change_Reproductive_culms,
         Change_BGDensity, Change_BGCover, Change_ShrubCover, Change_HerbCover)

# Join
culm.change <- culm.change.join %>% 
  left_join(initial.fixed)

# Change col names
culm.change <- culm.change %>% 
  rename(Init_BGDensity = BGDensity,
         Init_BGCover = BGCover,
         Init_ShrubCover = ShrubCover,
         Init_HerbCover = HerbCover)



# Fix NAs -----------------------------------------------------------------

# Look for NAs
apply(culm.change, 2, anyNA)

# Inspect NAs for initial cols
init.na <- culm.change %>% 
  filter(is.na(Init_BGCover))

dat.init.na <- dat %>% 
  filter(Plant_ID %in% init.na$Plant_ID)

init.init.na <- initial %>% 
  filter(Plant_ID %in% init.na$Plant_ID)


# Fix plant 165 & 169: the plot number for Year 2 is wrong/switched
#   Create fixed rows for 165 & 169
plant165.169.fix <- dat.raw %>% 
  filter(StudyYear == 2, Plant_ID %in% c(165, 169)) %>% 
  mutate(Plot = c(221, 222))

#   Remove incorrect row and add in fixed one
dat <- dat1 %>% 
  mutate(raw.row = 1:nrow(dat1))
row.rm2 <- dat %>% 
  filter(StudyYear == 2, Plant_ID %in% c(165, 169))
dat <- dat %>% 
  filter(!raw.row %in% row.rm2$raw.row) %>% 
  select(-raw.row) %>% 
  bind_rows(plant165.169.fix)

# Save intermediate
dat2 <- dat


# Add initial for plants 561, 577, 693, 774, which occurred in Year 2 (not Year 1)
init.add <- dat %>% 
  filter(StudyYear == 2,
         Plant_ID %in% c(561, 577, 693, 774)) %>% 
  select(Site, Transect, Plot, Plant_ID, BGCover, BGDensity, ShrubCover, HerbCover)

#   Add to initial
initial.fixed2 <- initial.fixed %>% 
  bind_rows(init.add)


# Recalculate culm change (again) -----------------------------------------

culm.change <- dat2 %>%
  arrange(Plant_ID, Year) %>%
  group_by(Plant_ID) %>%
  mutate(Change_Reproductive_culms = Reproductive_culms - lag(Reproductive_culms),
         Change_Total_Live_Culms = Total_Live_Culms - lag(Total_Live_Culms)) %>% 
  mutate(Change_BGDensity = BGDensity - lag(BGDensity),
         Change_BGCover = BGCover - lag(BGCover)) %>% 
  mutate(Change_HerbCover = HerbCover - lag(HerbCover),
         Change_ShrubCover = ShrubCover - lag(ShrubCover)) %>% 
  filter(!is.na(Change_Total_Live_Culms))



# Join initial conditions with culm.change (again) ------------------------

# Create version of culm.change for join
culm.change.join <- culm.change %>% 
  select(Year, Site, Transect, Plot, Plant_ID, Prev_year_precip, PlotSlope, Aspect,
         Change_Total_Live_Culms, Change_Reproductive_culms,
         Change_BGDensity, Change_BGCover, Change_ShrubCover, Change_HerbCover)

# Join
culm.change <- culm.change.join %>% 
  left_join(initial.fixed2)

# Change col names
culm.change <- culm.change %>% 
  rename(Init_BGDensity = BGDensity,
         Init_BGCover = BGCover,
         Init_ShrubCover = ShrubCover,
         Init_HerbCover = HerbCover)

# Look for NAs
apply(culm.change, 2, anyNA)


# Write to CSV ------------------------------------------------------------

write_csv(dat,
          file = "data/cleaned/11.1_demography-data_clean.csv")

write_csv(culm.change,
          file = "data/cleaned/11.1_change-in-culm-density-cover_clean.csv")
