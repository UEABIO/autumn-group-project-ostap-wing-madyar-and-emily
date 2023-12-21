# BIO-5023Y Autumn Group Project
## Script for cleaning
### Data first published in  Batra, Neale et al. (2021), The Epidemiologist R Handbook 

# Loading packages ----
library(tidyverse) # tidy data packages
library(janitor) # clean variable names

# Importing the data ----
covid_data_raw <- read_csv("data/covid_example_data.csv")

#___________________----

# Exploring the data-----

# Checking if variable types and names are appropriate and the number of variables
glimpse (covid_data_raw)

# Checking for NA and where they are
summary(covid_data_raw)
#The location of NAs are not shown in some variables. Separate investigation is needed.

# Cleaning all variable names
covid_data <- janitor::clean_names(covid_data_raw)

#Checking variable names
glimpse (covid_data)

# Checking for duplicate rows
covid_data %>% 
  duplicated() %>% # produces a list of TRUE/FALSE statements for duplicates
  sum() # sums all duplicates if present

# Removing duplicates
covid_data <- unique(covid_data) # Only includes unique individuals, no duplicates

# Checking the result after removal of the duplication
covid_data %>% 
  duplicated() %>% # produces a list of TRUE/FALSE statements for duplicated or not
  sum() # sums all duplicates if present
