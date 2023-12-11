# Script for cleaning----

#___________________----

# Loading packages ----
library(tidyverse) # tidy data packages
library(janitor) # clean variable names

# Importing data ----
covid_data_raw <- read_csv("data/covid_example_data.csv")
#___________________----


# Exploring data to clean it -----

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

# Renaming the variable in the new data frame for future work
covid_data <- rename(covid_data,
                     "case_dates"="reprt_creationdt_false") # use rename from the dplyr package

# Checking the study period of cases to be as extra information
# Converting the character date values into data format and calculating the study period
covid_data <- covid_data %>%
  mutate(case_dates = lubridate::dmy(case_dates))  #Converting into date format

# Calculating the study period 
covid_data %>% 
  summarise(first_case=min(case_dates), # Pulling out the first date of case
            last_case=max(case_dates),  # Pulling out the last date of case
            study_duration_months = (last_case-first_case)/lubridate::dmonths(1)) # Calculating the period in months

#___________________-----
