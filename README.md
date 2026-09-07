# Exiled Leader Destination Dataset (ELD) & Replication Code

Replication repository and data pipeline for the quantitative analysis of political leaders' destination choices in exile (1918–2020).

## Overview

* **`ELD_Dataset.csv`** – Core dataset covering $n = 214$ cases of exiled political leaders across linguistic, religious, geographical, and regime characteristics.
* **`codebook.xlsx`** – Variable descriptions, operationalization details, and source attributions.
* **`All_Datasets.xlsx`** – Supplementary raw and merged background data.

## Replication Pipeline

The scripts should be executed in sequential order:

* **Data Preparation (Optional):**
  * `01_Load_Data.R` & `02_DataWrangling.R` – Raw data ingestion, variable merging, and processing pipeline used to construct the final `ELD_Dataset.csv`.
* **Empirical Analysis & Visualizations:**
  * `03_Exploration.R` – Descriptive statistics and distribution tests ($\chi^2$-tests).
  * `04_maps.R` – Spatial analysis and global exile flow mapping.
  * `05_figures.R` – Generation of main figures, regime-shift transitions, and publication-ready plots.

## Software Requirements

* **R** (>= 4.2)
* Core packages: `tidyverse`, `sf`, `rnaturalearth`, `countrycode`
