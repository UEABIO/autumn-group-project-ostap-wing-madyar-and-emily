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



