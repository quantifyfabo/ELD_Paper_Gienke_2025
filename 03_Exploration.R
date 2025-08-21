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

# Load Final EDL dataset
ELD <- read.csv('/Users/fabiangi/Documents/Goethe Uni/Semester 2/VP Regierungschefs/Term Paper/Datasets/Exiled_Leader_List_Final_v3.csv')
ELD <- ELD[, -c(1,2)] # remove first two columns


#0 Leaders Origin Country Descriptive Statistics
sort(table(ELD$country), decreasing = T)
sort(table(ELD$origin_region), decreasing = T)
sort(prop.table(table(ELD$origin_region)), decreasing = T)
sort(prop.table(table(ELD$origin_religion)), decreasing = T)
sort(prop.table(table(ELD$origin_language)), decreasing = T)

#0 Leaders Final Destination Country Descriptive Statistics
sort(table(ELD$last_destination), decreasing = T)
sort(table(ELD$d_final_region), decreasing = T)
sort(table(ELD$d_final_religion), decreasing = T)
sort(table(ELD$d_final_language), decreasing = T)



#2: Region Switches based on Origin Region to final Region
region_changes <- ELD %>%
  filter(origin_region != d_final_region)

change_counts <- region_changes %>% # Count the occurrences of each specific region change
  count(origin_region, d_final_region, sort = TRUE)

total_changes <- nrow(region_changes) # Calculate the total number of cases that changed regions

change_percentages <- change_counts %>% # Compute the percentage for each type of region change
  mutate(percentage = (n / total_changes) * 100)

print(change_percentages)



#4: Religion Switches based on Religion Origin to Final Destination
religion_changes <- ELD %>%
  filter(origin_religion != d_final_religion)

change_counts <- religion_changes %>% # Count the occurrences of each specific religion change
  count(origin_religion, d_final_religion, sort = TRUE)

total_changes <- nrow(religion_changes) # Calculate the total number of cases that changed religions

change_percentages <- change_counts %>% # Compute the percentage for each type of religion change
  mutate(percentage = (n / total_changes) * 100)

print(change_percentages)



#6: Language Switches
language_changes <- ELD %>%
  filter(origin_language != d_final_language)

change_counts <- language_changes %>% # Count the occurrences of each specific language change
  count(origin_language, d_final_language, sort = TRUE)

total_changes <- nrow(language_changes) # Calculate the total number of cases that changed languages

change_percentages <- change_counts %>% # Compute the percentage for each type of language change
  mutate(percentage = (n / total_changes) * 100)

print(change_percentages) # Display the final result



# ---------- Utility: Flow-Tables ----------
flow_tables <- function(df, origin_col, dest_col, digits = 1, drop_na = TRUE, only_changes = FALSE) {
  o <- rlang::ensym(origin_col)
  d <- rlang::ensym(dest_col)
  
  data <- df
  if (drop_na) data <- data %>% filter(!is.na(!!o), !is.na(!!d))
  if (only_changes) data <- data %>% filter(!!o != !!d)
  
  # long table with counts
  counts <- data %>%
    mutate(across(c(!!o, !!d), ~str_trim(as.character(.)))) %>%
    count(!!o, !!d, name = "n")
  
  total_n <- sum(counts$n)
  
  # row totals for row%
  row_tot <- counts %>% group_by(!!o) %>% summarise(row_n = sum(n), .groups = "drop")
  # col totals for col%
  col_tot <- counts %>% group_by(!!d) %>% summarise(col_n = sum(n), .groups = "drop")
  
  long <- counts %>%
    left_join(row_tot, by = rlang::as_name(o)) %>%
    left_join(col_tot, by = rlang::as_name(d)) %>%
    mutate(
      row_pct   = 100 * n / row_n,
      col_pct   = 100 * n / col_n,
      share_all = 100 * n / total_n
    ) %>%
    arrange(desc(n)) %>%
    mutate(
      row_pct   = round(row_pct, digits),
      col_pct   = round(col_pct, digits),
      share_all = round(share_all, digits)
    )
  
  # wide table (row %) like an alluvial-as-numbers
  wide_rowpct <- counts %>%
    group_by(!!o) %>%
    mutate(row_pct = 100 * n / sum(n)) %>%
    ungroup() %>%
    mutate(row_pct = round(row_pct, digits)) %>%
    select(!!o, !!d, row_pct) %>%
    pivot_wider(names_from = !!d, values_from = row_pct, values_fill = 0) %>%
    arrange(!!o)
  
  list(long = long, wide_rowpct = wide_rowpct, total_n = total_n)
}

# all Flows (with same region)
reg_all <- flow_tables(ELD, origin_region, d_final_region, digits = 1, only_changes = FALSE)
reg_all$long         # Counts, row%, col%, share_all (sortiert nach Count)
reg_all$wide_rowpct  # row share matrix (noch im paper definieren!!!)
reg_all$total_n      # total cases

# only change without same religion
reg_changes <- flow_tables(ELD, origin_region, d_final_region, digits = 1, only_changes = TRUE)
reg_changes$long
reg_changes$wide_rowpct
reg_changes$total_n

rel_all <- flow_tables(ELD, origin_religion, d_final_religion, digits = 1)
rel_all$long
rel_all$wide_rowpct

rel_all <- flow_tables(ELD, origin_religion, d_final_religion, digits = 1)
rel_all$long
rel_all$wide_rowpct

# Top 10 Region-Flows by cases with row share
reg_all$long %>%
  transmute(
    origin = !!rlang::sym("origin_region"),
    destination = !!rlang::sym("d_final_region"),
    n, row_pct, col_pct, share_all
  ) %>%
  slice_max(n, n = 10)

# Global perspective of Region changes across ELD Data
region_change_share <- ELD %>%
  mutate(change_region = ifelse(origin_region == d_final_region, "same region", "changed region")) %>%
  count(change_region) %>%
  mutate(share = round(100 * n / sum(n), 1))
region_change_share


# Language Flows
language_flows <- ELD %>%
  filter(!is.na(origin_language), !is.na(d_final_language)) %>%
  count(origin_language, d_final_language, name = "n") %>%
  group_by(origin_language) %>%
  mutate(
    row_pct = round(100 * n / sum(n), 1)
  ) %>%
  ungroup() %>%
  arrange(desc(n))

language_flows


# check for language on whole population
language_summary <- ELD %>%
  mutate(language_change = case_when(
    origin_language == d_final_language ~ "Same language",
    d_final_language == "English" ~ "Switched to English",
    TRUE ~ "Other switch"
  )) %>%
  count(language_change) %>%
  mutate(share = round(100 * n / sum(n), 1))

print(language_summary)

# ckeck for region changes across n
same_region_pct <- ELD %>%
  summarise(
    total = n(),
    same_region = sum(origin_region == d_final_region, na.rm = TRUE),
    pct_same_region = (same_region / total) * 100
  )
print(same_region_pct)

# check for religion changes across n
same_religion_pct <- ELD %>%
  summarise(
    total = n(),
    same_religion = sum(origin_religion == d_final_religion, na.rm = TRUE),
    pct_same_religion = (same_religion / total) * 100
  )
print(same_religion_pct)



# Chi-squared test for Region (origin_region vs. d_final_region)
tab_region <- table(ELD$origin_region, ELD$d_final_region)
chisq_region <- chisq.test(tab_region)
chisq_region

# Chi-squared test for Religion (origin_religion vs. d_final_religion)
tab_religion <- table(ELD$origin_religion, ELD$d_final_religion)
chisq_religion <- chisq.test(tab_religion)
chisq_religion

# Chi-squared test for Language (origin_language vs. d_final_language)
tab_language <- table(ELD$origin_language, ELD$d_final_language)
chisq_language <- chisq.test(tab_language)
chisq_language


