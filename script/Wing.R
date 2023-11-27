#___________________________----
# SET UP ----
## An analysis and plot of covid data - gender vs covid deaths  ----

### Data first published in  Batra, Neale et al. (2021), The Epidemiologist R Handbook. ------
#__________________________----
# PACKAGES ----
library(colorBlindness) #check plot accessibility
#__________________________----
# Import tided data and load r objects and functions 
source("script/cleaning_data.R")
#__________________________----
# DPLYR VERBS ----
# this example is human readable without intermediate objects
gender_vs_covid_death <- covid_data %>% 
  select(pid, case_gender, died_covid) %>% 
  filter(died_covid == "Yes") 
  
