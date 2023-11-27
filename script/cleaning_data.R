# Script for cleaning

#___________________________----
# SET UP ----

# PACKAGES ----
library(tidyverse) # tidy data packages
library(janitor) # clean varibale names

# IMPORT DATA ----
covid_data_raw <- read_csv("data/covid_example_data.csv")
#___________________________----


## Exploring data to clean it -----

# Checking for NA and varibale names
summary(covid_data_raw)

# Cleaning all variable names

covid_data <- janitor::clean_names(covid_data_raw)

#Checking variable names
glimpse (covid_data)


# Filter the data variable for individuals that died of covid
filtered_covid_dead <- filter(.data = covid_data, died_covid == "Yes") %>% 
  select (died_covid)
