# Title: BIO-5023Y 23-24 autumn group project ----
## Ostap
### Exploring death rates on age categories

#__________________----

# Setting up personal loading packages ----
library(colorBlindness) #check plot accessibility #### REMOVE NOT NEEDED

#__________________-----

# Loading objects and functions from main cleaning script ----
# Import tidied covid data

source("scripts/cleaning_data.R")

#__________________-----

# Exploring the variables of interest for my plot ----

# Population sampled
covid_data%>%
  select(died_covid)%>% # Selecting death from covid variable from dataset
  summarise(n=n()) #Calculating all individuals involved

# Checking total deaths
covid_data %>% 
  select(died_covid) %>% #Selects death from covid variable
  filter(died_covid == "Yes") %>% # Filter only deaths
  summarise (n=n()) # Summarizes total deaths

# Checking death age range to inform of future graph plot
covid_data %>% 
  select (case_age, died_covid) %>% # Selects the 2 variables from data set
  filter(died_covid == "Yes") %>% # Filter only deaths
  summarise(youngest=min(case_age),
            oldest=max(case_age)) # Retrieves min and max age groups

# Checking for any unexpected values such as NA and unknown in the age and death from covid variables
# Looking if are any typos in the values
covid_data %>%
  group_by(case_age, died_covid) %>%  # Grouping data by the 2 variables
  summarise(n = n()) # Outputting all the values per variable

