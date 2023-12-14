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

# Checking for distinct values in death from covid variable
covid_data %>% 
  select(died_covid)%>% # Selecting the variable (died covid)
  group_by(died_covid)%>% # Grouping by deaths
  summarise(n=n()) # Outputting all distinct values and number per category

# Checking for any other values than male or female, in gender variable
covid_data %>%
  select(case_gender)%>% # Selecting the variable (gender)
  group_by(case_gender)%>% # Grouping by gender
  summarise(n=n()) # Outputting all distinct values and number per category

#___________________----

# Narrowing down data frame for the interested variables ----
# New variable for the data frame to contain only individuals that died from covid from different gender
gender_vs_covid_death <- covid_data %>%
  select(pid, case_gender, died_covid) %>% # selecting variables of interest
  filter(died_covid == "Yes", case_gender == "Male" | case_gender == "Female") # including only "Yes" values in the death from covid variable, Filter for only "Male" and "Female in the gender variable, removes any NA/ unknown  

gender_vs_covid_death %>%
  select(case_gender)%>% # Selecting the variable (gender)
  group_by(case_gender)%>% # Grouping by gender
  summarise(n=n()) # Outputting all distinct values and number per category

#___________________----

# Briefly exploring data distribution of deaths across all ages to inform into age categories----
## Briefly plotting data to visualize. Allows analyzing and interpreting data
ggplot(data = gender_vs_covid_death, # Selecting data
       aes(x = case_gender)) + # Setting x axis
  geom_bar() # Building bar plot with death frequency/count
# Insight - Male with more deaths than Females. Interesting as more males in sample than females.
# Some data is missing due to NAs and Under Review
