# script for cleaning

#___________________________----
# SET UP ----

# PACKAGES ----
library(tidyverse) # tidy data packages
library(janitor) # cleans variable names
library(patchwork) # To make final figure
library(colorBlindness) #check plot accessibility

# IMPORT DATA ----
covid_data_raw <- read_csv("data/covid_example_data.csv")
