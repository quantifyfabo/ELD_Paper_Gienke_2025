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

# load datasets based on the I'll be back paper's CodeBook

civ_confl_rep_01 <- read_dta('/Civil Conflict Replication Data.dta')
irr_trans_rep_02 <- read_dta('/Irregular Transitions Replication Data.dta')
coups_rep_03 <- read_dta('/Coups Replication Data.dta')
protest_rep_04 <- read_dta('/Protests Replication Data.dta')

# created LIE Leader list from PDF
lie_leaderlist_v2 <- read_excel('/lie_leader_list.xlsx')
lae_raw <- read.csv('/LIE_LAE_Merging/LAE_DATA.csv')

# combine datasets
lie_leaderlist_v2 <- read_excel('/LIE_LAE_Merging/lie_leader_list.xlsx')
lae_raw <- read.csv('/LIE_LAE_Merging/LAE_DATA.csv')

# Merge LIE and LAE
lae_raw_subset <- lae_raw[, c("Leader", "PoliticalUpheaval.", "Destination1", "Destination2", "Destination3")]

Merge Data based on identical variable (LAE_Name and Leader)
merged_LAE <- merge(
  lie_leaderlist_v2,
  lae_raw_subset,
  by.x = "LAE_Name",
  by.y = "Leader",
  all.x = TRUE
)

# Remove the redundant variable LEA_Name
merged_LAE$LAE_Name <- NULL

# download merged data as excel
write.csv(merged_LAE,'/MergedLAE.csv', na="")

