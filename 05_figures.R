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

# Figures for the ELD Termpaper

#0.1 Time Range
as.numeric(ELD$exile.starts)

yearly_counts <- ELD %>%
  mutate(year = as.integer(exile.starts)) %>%
  count(year) %>%
  complete(year = 1900:2024, fill = list(n = 0))

ggplot(yearly_counts, aes(x = year, y = n)) +
  geom_line(color = "steelblue") +
  geom_smooth(se = FALSE, method = "loess", span = 0.2, color = "darkred", size = 1.2) +
  labs(
    title = "Number of New Exiles per Year",
    x = "Year",
    y = "Number of beginning Exiles"
  ) +
  theme_minimal()

# Plot absolute numbers of leader per time
ELD <- ELD %>%
  mutate(
    start = as.integer(exile.starts),
    end = as.integer(exile.ends)
  )

# 2. One Column per Exiled Leader/Year
active_years <- ELD %>%
  filter(!is.na(start), !is.na(end)) %>%
  mutate(year_range = map2(start, end, ~ .x:.y)) %>%
  unnest(year_range) %>%
  filter(year_range >= 1900, year_range <= 2024)  # Begrenze auf relevante Jahre

# 3. COunt how many exiled Leader per year 
yearly_exile_counts <- active_years %>%
  count(year = year_range) %>%
  complete(year = 1900:2024, fill = list(n = 0))

mean_year <- with(yearly_exile_counts, weighted.mean(year, n))

# 4. Plot
ggplot(yearly_exile_counts, aes(x = year, y = n)) +
  geom_line(color = "steelblue") +
  geom_vline(xintercept = mean_year, color = "red", linewidth = 0.5, linetype = "dashed") +
  labs(
    title = "Number of Exiled Leader per Year (1900–2024)",
    x = "Year",
    y = "Number of Exiled Leader"
  ) +
  theme_minimal()


# ALLUVIAL PLOT (ggplot2 + ggalluvial)
# Clean Data from NA
flows <- ELD %>%
  filter(!is.na(origin_region), !is.na(d_final_region)) %>%
  count(origin_region, d_final_region, name = "n") %>%
  mutate(
    origin_region = str_trim(as.character(origin_region)),
    d_final_region = str_trim(as.character(d_final_region))
  )

# 2) Sort Regions by Size
origin_order <- flows %>% group_by(origin_region) %>% summarise(total = sum(n)) %>%
  arrange(desc(total)) %>% pull(origin_region)
dest_order   <- flows %>% group_by(d_final_region) %>% summarise(total = sum(n)) %>%
  arrange(desc(total)) %>% pull(d_final_region)

flows <- flows %>%
  mutate(
    origin_region = factor(origin_region, levels = origin_order),
    d_final_region = factor(d_final_region, levels = dest_order)
  )

# 3) Plot
p_alluvial <- ggplot(
  flows,
  aes(axis1 = origin_region, axis2 = d_final_region, y = n)
) +
  geom_alluvium(aes(fill = origin_region), alpha = 0.8, knot.pos = 0.4) +
  geom_stratum(width = 0.25, color = "grey30") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3.4, vjust = -0.5) +
  scale_x_discrete(limits = c("Origin region", "Final destination region"), expand = c(.1, .05)) +
  labs(
    title = "Flows from Origin Regions to Final Destination Regions based on ELD Dataset",
    x = NULL, y = "Number of exiled leaders",
    fill = "Origin region"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

p_alluvial


# Alluvial Chart 2 for Religion
# 1) Clean Data
flows_rel <- ELD %>%
  filter(!is.na(origin_religion), !is.na(d_final_religion)) %>%
  count(origin_religion, d_final_religion, name = "n") %>%
  mutate(
    origin_religion  = str_trim(as.character(origin_religion)),
    d_final_religion = str_trim(as.character(d_final_religion)),
    # disambiguierte Achsen-Labels
    origin_religion_axis  = paste0("Origin: ", origin_religion),
    d_final_religion_axis = paste0("Final: ", d_final_religion)
  )

# 2) Sort Religion by Size
origin_order_rel <- flows_rel %>% group_by(origin_religion_axis) %>% summarise(total = sum(n)) %>%
  arrange(desc(total)) %>% pull(origin_religion_axis)
dest_order_rel   <- flows_rel %>% group_by(d_final_religion_axis) %>% summarise(total = sum(n)) %>%
  arrange(desc(total)) %>% pull(d_final_religion_axis)

flows_rel <- flows_rel %>%
  mutate(
    origin_religion_axis  = factor(origin_religion_axis,  levels = origin_order_rel),
    d_final_religion_axis = factor(d_final_religion_axis, levels = dest_order_rel)
  )

# 3) Plot
p_alluvial_rel <- ggplot(
  flows_rel,
  aes(axis1 = origin_religion_axis, axis2 = d_final_religion_axis, y = n)
) +
  geom_alluvium(aes(fill = origin_religion), alpha = 0.8, knot.pos = 0.4) +
  geom_stratum(width = 0.25, color = "grey30") +
  geom_text(
    stat = "stratum",
    aes(label = gsub("^(Origin: |Final: )", "", after_stat(stratum))),
    size = 3.4, vjust = -0.5
  ) +
  scale_x_discrete(limits = c("Origin religion", "Final destination religion"), expand = c(.1, .05)) +
  labs(
    title = "Flows from Origin Religions to Final Destination Religions based on ELD Dataset",
    x = NULL, y = "Number of exiled leaders",
    fill = "Origin religion"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

p_alluvial_rel



# Plots for Pol V Distrbution
# 1) Difference calculation
ELD_diff <- ELD %>%
  filter(!is.na(origin_PolV), !is.na(d_final_PolV)) %>%
  mutate(delta_PolV = d_final_PolV - origin_PolV)

# 2) Histogramm for differences
p_hist <- ggplot(ELD_diff, aes(x = delta_PolV)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Change in Polity V Index (Final – Origin)",
    x = "Delta Polity V (Destination - Origin)",
    y = "Number of exiled leaders"
  ) +
  theme_minimal(base_size = 13)

print(p_hist)

# 3) Density Plot 
p_density <- ggplot(ELD_diff, aes(x = delta_PolV)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Distribution of Polity V Changes",
    x = "Delta Polity V (Destination - Origin)",
    y = "Density"
  ) +
  theme_minimal(base_size = 13)

print(p_density)


# PolV Boxplots
# 1) Data in Long-Format (Only working pairs)
ELD_long <- ELD %>%
  filter(!is.na(origin_PolV), !is.na(d_final_PolV)) %>%
  transmute(
    case_id = row_number(),
    Origin = origin_PolV,
    `Final destination` = d_final_PolV
  ) %>%
  pivot_longer(cols = c(Origin, `Final destination`),
               names_to = "stage", values_to = "PolV")

# 2) Boxplot with points showing PolV Final destination vs Origin
p_box <- ggplot(ELD_long, aes(x = stage, y = PolV, fill = stage)) +
  geom_boxplot(alpha = 0.7, width = 0.55, outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.35, size = 1) +
  labs(
    title = "Polity V: Origin vs. Final Destination",
    x = NULL,
    y = "Polity V Index (-10 … +10)",
    fill = NULL
  ) +
  coord_cartesian(ylim = c(-10, 10)) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(face = "bold", size = 12)
  )

print(p_box)

# 3) Optional: Violin-Plot (shows desnity) + median
p_violin <- ggplot(ELD_long, aes(x = stage, y = PolV, fill = stage)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  stat_summary(fun = median, geom = "point", size = 2.5, color = "black") +
  stat_summary(fun = mean,   geom = "point", size = 2.5, color = "red") +
  labs(
    title = "Distribution of Polity V: Origin vs. Final Destination",
    x = NULL,
    y = "Polity V Index (-10 … +10)",
    fill = NULL
  ) +
  coord_cartesian(ylim = c(-10, 10)) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(face = "bold", size = 12)
  )

#show
print(p_violin)


# 5) statistics
ELD_diff <- ELD %>%
  filter(!is.na(origin_PolV), !is.na(d_final_PolV)) %>%
  mutate(delta = d_final_PolV - origin_PolV)

summary_stats <- ELD_diff %>%
  summarise(
    n = n(),
    mean_origin = mean(origin_PolV),
    mean_final  = mean(d_final_PolV),
    mean_delta  = mean(delta),
    median_delta = median(delta)
  )
print(summary_stats)


# 1) Difference
ELD_diff <- ELD %>%
  filter(!is.na(origin_PolV), !is.na(d_final_PolV)) %>%
  mutate(delta = d_final_PolV - origin_PolV) %>%
  arrange(delta) %>%
  mutate(case_id = row_number())

# 2) Balkendiagramm sortet by Delta
p_delta <- ggplot(ELD_diff, aes(x = factor(case_id), y = delta)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Change in Polity V Index per Exiled Leader",
    x = "Exiled leaders (sorted by change)",
    y = "Delta Polity V (Destination - Origin)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_blank(),  
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

print(p_delta)

# Figure Change in Polity V Index per Exiled Leader
# Alternativ Bar chart
ELD_diff <- ELD %>%
  filter(!is.na(origin_PolV), !is.na(d_final_PolV)) %>%
  mutate(delta = d_final_PolV - origin_PolV) %>%
  arrange(delta) %>%
  mutate(case_id = row_number())


ELD_diff <- ELD_diff %>%
  mutate(group_delta = round(delta))   

label_df <- ELD_diff %>%
  group_by(group_delta) %>%
  summarise(
    xmin = min(case_id),
    xmax = max(case_id),
    xmid = (xmin + xmax) / 2,
    y    = unique(group_delta),
    .groups = "drop"
  ) %>%
  mutate(
    ypos = ifelse(y > 0, y + 0.6, ifelse(y < 0, y - 0.6, y + 0.6))
  )

# Plot
p_delta <- ggplot(ELD_diff, aes(x = case_id, y = delta)) +
  geom_col(fill = "steelblue", width = 0.9) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.9) +
  geom_text(
    data = label_df,
    aes(x = xmid, y = ypos, label = y),
    inherit.aes = FALSE,
    size = 5, fontface = "bold", color = "grey25"
  ) +
  labs(
    title = "Change in Polity V Index per Exiled Leader",
    x = "Exiled leaders (sorted by change)",
    y = "Delta Polity V (Destination - Origin)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off")  

print(p_delta)

# Appendix Figure A2
# PolV clear duplicates
PolityV <- PolityV %>%
  distinct(Entity, Year, .keep_all = TRUE)

# 1) Polity only year 1974
polity_1974 <- PolityV %>%
  filter(Year == 1974) %>%
  transmute(
    Country         = as.character(Entity),
    Democracy_1974  = as.numeric(Democracy)
  ) %>%
  distinct(Country, .keep_all = TRUE) 

# Merge left on country
WRD_Base_merged <- WRD_Base %>%
  mutate(Country = as.character(Country)) %>%
  left_join(polity_1974, by = "Country")

# Make Figure showing Religion and Democracy Index
WRD_Base_merged %>%
  group_by(dominant_religion) %>%
  summarise(avg_democracy = mean(Democracy_1974, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(dominant_religion, avg_democracy),
             y = avg_democracy)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 0, color = "black") +
  ylim(-10, 10) +
  labs(x = "Dominant Religion within Country",
       y = "Average Democracy Score (1974)",
       title = "Democracy Score 1974 by Religion (Average) based on WRD, PolV and ELD Data") +
  coord_flip()  
