# Title: BIO-5023Y 23-24 autumn group project ----
## Wing Lun Lim
### An analysis and plot of covid data - gender vs covid deaths
#### Data first published in  Batra, Neale et al. (2021), The Epidemiologist R Handbook

#___________________----

# Setting up loading packages ----
library(colorBlindness) #check accessibility of plot to colour blind users

#___________________-----

# Loading objects and functions from main cleaning script ----
# Import tidied covid data

source("scripts/cleaning_data.R")

#___________________-----

# Determining and exploring the variables needed for my plot ----

# Brief overview of data
glimpse(covid_data)
summary(covid_data)

# Checking population sampled 
covid_data%>%
  select(died_covid)%>% # Selecting died covid variable from dataset
  summarise(n=n()) #Calculating all individuals involved (82100)

# Checking total deaths from people sampled
covid_data %>% 
  select(died_covid) %>% #Selects death from covid variable
  filter(died_covid == "Yes") %>% # Filter only deaths
  summarise (n=n()) # Summarizes total deaths (1338)

# Checking for any unexpected values such as NA and unknown in the age and death from covid variables
# Looking if are any typos in the values
covid_data %>%
  group_by(case_gender, died_covid) %>%  # Grouping data by the 2 variables
  summarise(n = n()) # Outputting all the values per variable

