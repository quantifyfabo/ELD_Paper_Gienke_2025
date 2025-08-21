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

# Map 1

# Set up right country names (as in rnaturalearth package)
leaderlist_maptool <- ELD %>%
  mutate(country = recode(country,
                          "Central African Republic" = "Central African Rep.",
                          "Dominican Republic"       = "Dominican Rep.",
                          "Ivory Coast"              = "Côte d'Ivoire",
  ))


# 1. count country number in leaderlist dataset
country_counts <- leaderlist_maptool %>%
  count(country, name = "n")

# 2. load map
world <- ne_countries(scale = "medium", returnclass = "sf")

# 3. merge map with country data
world_data <- world %>%
  left_join(country_counts, by = c("name" = "country"))

# Basis: Weltkarte (world_data aus deinem Merge)
ggplot(data = world_data) +
  geom_sf(aes(fill = n), color = "white", size = 0.1) +  # Ländergrenzen dünn und weiß
  scale_fill_gradientn(
    colours = c("darkgreen", "yellow", "red"),
    na.value = "grey90",
    name = "Cases"
  ) +
  # 🌊 Wasserfarbe & Kartendesign
  theme_minimal(base_family = "Helvetica") +
  theme(
    panel.background = element_rect(fill = "#d6f1ff", color = NA),  # Hellblaues Wasser
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(size = 9, color = "grey40")
  ) +
  labs(
    title = "Origin Countries of Exiled Leader",
    caption = "Created by Fabian Gienke, n=214"
  )



# Map 2
# 1. last_destination ggf. wie country harmonisieren (Recode)
leaderlist_maptool <- leaderlist_maptool %>%
  mutate(last_destination = recode(last_destination,
                                   "United States"        = "United States of America",
                                   "Ivory Coast"          = "Côte d'Ivoire",
                                   "Dominican Republic"   = "Dominican Rep."
  ))

# 2. Zähle Häufigkeit der letzten Destinationen
destination_counts <- leaderlist_maptool %>%
  count(last_destination, name = "n")

# 3. Merge mit Weltkarte
world_data_dest <- world %>%
  left_join(destination_counts, by = c("name" = "last_destination"))

# 4. Zeichne die zweite Karte
ggplot(data = world_data_dest) +
  geom_sf(aes(fill = n), color = "white", size = 0.1) +
  scale_fill_gradientn(
    colours = c("darkgreen", "yellow", "red"),
    na.value = "grey90",
    name = "Cases"
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    panel.background = element_rect(fill = "#d6f1ff", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(size = 9, color = "grey40")
  ) +
  labs(
    title = "Destination Countries of Exiled Leader",
    caption = "Created by Fabian Gienke, n=214"
  )

