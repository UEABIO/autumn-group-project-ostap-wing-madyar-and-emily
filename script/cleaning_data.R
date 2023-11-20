# script for cleaning

#___________________________----
# SET UP ----

# PACKAGES ----
library(tidyverse) # tidy data packages
library(janitor) # cleans variable names
library(lubridate) # make sure dates are processed properly
library(patchwork) # To make final figure
library(colorBlindness) #check plot accessibility

# IMPORT DATA ----
covid_data_raw <- read_csv("data/owid-covid-data.csv")


# working with dates changing then to day/month/year format----



