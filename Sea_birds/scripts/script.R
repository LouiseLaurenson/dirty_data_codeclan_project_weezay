library(tidyverse)
library(readxl)
library(janitor)

#reading in both sheets
bird_data <- read_excel("raw_data/seabirds.xls", sheet = "Bird data by record ID")
ship_data <- read_excel("raw_data/seabirds.xls", sheet = "Ship data by record ID")


#clean bird data
bird_data <- bird_data %>%
  select(RECORD,
    `RECORD ID`,
    `Species  scientific name (taxon [AGE /SEX /  PLUMAGE PHASE])`,
    `Species common name (taxon [AGE / SEX / PLUMAGE PHASE])`,
    `Species abbreviation`,
    COUNT
  )

bird_data <- bird_data %>% 
  clean_names()

bird_data <- bird_data %>%
  rename(
    bird_record = record,
    species = species_common_name_taxon_age_sex_plumage_phase,
    species_scientific = species_scientific_name_taxon_age_sex_plumage_phase
  )

#clean species_scie

bird_data <- bird_data %>%
  mutate(species  = str_replace(
    species, pattern = "AD|JUV|IMM|SUBAD|PL[0-9]+",
    ""))

bird_data <- bird_data %>%
  mutate(species_abbreviation  = str_replace(
    species_abbreviation, pattern = "AD|JUV|IMM|SUBAD|PL[0-9]+",
    ""))

bird_data <- bird_data %>%
  mutate(species_scientific  = str_replace(
    species_scientific, pattern = "AD|JUV|IMM|SUBAD|PL[0-9]+",
    ""))




bird_data <- bird_data %>%
  mutate(species = str_replace(
    species, pattern = "Black|White|Grey", ""))

bird_data <- bird_data %>%
  mutate(species = str_replace(
    species, pattern = "-", " "))




bird_data <- bird_data %>%
  mutate(species_abbreviation = str_replace(
    species_abbreviation, pattern = "PL[0-9]+", ""))

bird_data <- bird_data %>%
  mutate(species_scientific = str_replace(
    species_scientific, pattern = "PL[0-9]+", ""))

#clean ship data --------------------------------------------------------
ship_data <- ship_data %>%
  select(RECORD, `RECORD ID`, LAT, LONG)

ship_data <- ship_data %>%
  clean_names()

#join on record ID
ship_and_bird <- full_join(bird_data, ship_data, by = "record_id")

ship_and_bird %>% 
  summarise(across(.fns = ~ sum(is.na(.x))))

#remove anything with na in count 
ship_and_bird <- ship_and_bird %>% 
  filter(!is.na(count)) 



write_csv(ship_and_bird, "raw_data/ship_and_bird_clean.csv")
