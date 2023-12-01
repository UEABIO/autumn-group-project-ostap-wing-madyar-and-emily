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

# Checking if variable types and names are appropriate, also how many categories
glimpse (covid_data_raw)


# Checking for NA and where they are
summary(covid_data_raw)

# Previous function doesn't show location of NAs in some variables. Therefore need to be investigated separately

# Cleaning all variable names
covid_data <- janitor::clean_names(covid_data_raw)

#Checking variable names
glimpse (covid_data)

# Checking for duplicate rows in the data
covid_data %>% 
  duplicated() %>% # produces a list of TRUE/FALSE statements for duplicated or not
  sum() # sums all duplicates if present


# Removing duplicates
covid_data <- unique(covid_data) # Only includes unique individuals, no duplicates

# Checking the result after removal of the duplication
covid_data %>% 
  duplicated() %>% # produces a list of TRUE/FALSE statements for duplicated or not
  sum() # sums all duplicates if present
