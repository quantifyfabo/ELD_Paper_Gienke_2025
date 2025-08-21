# Packages
library(quanteda)
library(haven)
library(dplyr)
library(sandwich)
library(lmtest)
library(clubSandwich)
library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(purrr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(stringr)
library(ggalluvial)




# Load final version of merged LEA and LIE Data
leaderlist <- read.csv('/Users/fabiangi/Documents/Goethe Uni/Semester 2/VP Regierungschefs/Term Paper/Datasets/LIE_LAE_Merging/leaderlist_v3.csv', sep=";")

# Remove unwanted white spaces
leaderlist <- leaderlist %>%
  mutate(Destination1 = str_trim(Destination1, side = "both"))
leaderlist <- leaderlist %>%
  mutate(Destination2 = str_trim(Destination2, side = "both"))
leaderlist <- leaderlist %>%
  mutate(Destination3 = str_trim(Destination3, side = "both"))

# Load WRD Data
WRD_Raw <- read.csv2('/Users/fabiangi/Documents/Goethe Uni/Semester 2/VP Regierungschefs/Term Paper/Datasets/Religious Composition 2010-2020 dataset/WRD_Raw.csv', fileEncoding = "UTF-8")

# WRD - Religion (only year 2010)
WRD_Raw <- WRD_Raw %>% 
  filter(Year == 2010)

religions <- c("Christians", "Muslims", "Buddhists", "Hindus", "Jews", "Other_religions")

# WRD - Religion (as numeric)
WRD_Raw <- WRD_Raw %>%
  mutate(across(all_of(religions), as.numeric))

# WRD - Religion (create "dominant religion" variable per country)
WRD_Base <- WRD_Raw %>%
  rowwise() %>%
  mutate(
    dominant_religion = religions[which.max(c_across(all_of(religions)))]
  ) %>%
  ungroup()

# WRD - Religion (empty cells = NA)
leaderlist <- leaderlist %>%
  mutate(
    across(starts_with("Destination"), ~na_if(., ""))
  )

# WRD - Merge Religion with ELD Data (based on Origin Countries)
leaderlist <- leaderlist %>%
  left_join(
    WRD_Base %>%
      select(Country, dominant_religion),
    by = c("country" = "Country")
  ) %>%
  rename(origin_religion = dominant_religion)

# WRD - Merge Religion with ELD Data (based on Destination1 Countries)
leaderlist <- leaderlist %>%
  left_join(
    WRD_Base %>%
      select(Country, dominant_religion),
    by = c("Destination1" = "Country")
  ) %>%
  rename(d1_religion = dominant_religion)

# WRD - Merge Religion with ELD Data (based on Destination1 Countries)
leaderlist <- leaderlist %>%
  mutate(
    last_destination = coalesce(Destination3, Destination2, Destination1) #last destination variable
  ) %>%
  left_join(
    WRD_Base %>%
      select(Country, dominant_religion),
    by = c("last_destination" = "Country")
  ) %>%
  rename(d_final_religion = dominant_religion)

# WRD - Merge Region with ELD Data (based on Origin Country)
leaderlist <- leaderlist %>%
  left_join(
    WRD_Base %>%
      select(Country, Region),
    by = c("country" = "Country")
  ) %>%
  rename(origin_region = Region)

# WRD - Merge Region with ELD Data (based on Destination1 Country)
leaderlist <- leaderlist %>%
  left_join(
    WRD_Base %>%
      select(Country, Region),
    by = c("Destination1" = "Country")
  ) %>%
  rename(d1_region = Region)

# WRD - Merge Region with ELD Data (based on Final Destination Country)
leaderlist <- leaderlist %>%
  mutate(
    last_destination = coalesce(Destination3, Destination2, Destination1)
  ) %>%
  left_join(
    WRD_Base %>%
      select(Country, Region),
    by = c("last_destination" = "Country")
  ) %>%
  rename(d_final_region = Region)



# Load Language Data
global_languages <- read_excel('/Users/fabiangi/Documents/Goethe Uni/Semester 2/VP Regierungschefs/Term Paper/Datasets/Languages/countries-languages.xlsx', skip = 1)

# Language - Merge Langauge with ELD Data (based on Origin Country)
leaderlist <- leaderlist %>%
  left_join(
    global_languages %>% 
      select(Country, language),
    by = c("country" = "Country"),
  ) %>% 
  rename(origin_language = language)

# Language - Merge Langauge with ELD Data (based on Final Destination Country)
leaderlist <- leaderlist %>%
  left_join(
    global_languages %>% 
      select(Country, language),
    by = c("last_destination" = "Country"),
  ) %>% 
  rename(d_final_language = language)



# Load Polity V Data
PolityV <- read_excel(
  '/Users/fabiangi/Documents/Goethe Uni/Semester 2/VP Regierungschefs/Term Paper/Compare/Data_Overview.xlsx',
  sheet = "PolV"
) %>%
  mutate(
    Entity    = str_squish(as.character(Entity)),
    Year      = as.integer(Year),
    Democracy = as.numeric(Democracy)
  )

# 2) PolV - Clean ELD for PolV Merge
leaderlist <- leaderlist %>%
  mutate(
    exile.starts     = as.integer(exile.starts),
    country          = str_trim(as.character(country)),
    Destination1     = str_trim(as.character(Destination1)),
    last_destination = str_trim(as.character(last_destination))
  )

# 3) PolV - Polity-Subset 
polity_subset <- PolityV %>%
  select(country = Entity, year = Year, Democracy)

# 4) Merges
leaderlist_merged <- leaderlist %>%
  left_join(polity_subset, by = c("country" = "country", "exile.starts" = "year")) %>%
  rename(origin_PolV = Democracy) %>%
  left_join(polity_subset, by = c("Destination1" = "country", "exile.starts" = "year")) %>%
  rename(d1_PolV = Democracy) %>%
  left_join(polity_subset, by = c("last_destination" = "country", "exile.starts" = "year")) %>%
  rename(d_final_PolV = Democracy)



# - Final ELD Dataset
write.csv(leaderlist_merged,'/Users/fabiangi/Documents/Goethe Uni/Semester 2/VP Regierungschefs/Term Paper/Datasets/Exiled_Leader_List_Final_v3.csv', na ="")

