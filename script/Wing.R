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
# Checking distinct ouputs
covid_data%>%
  distinct(case_gender, died_covid)
           
covid_data%>%
  distinct(case_gender) # 4 distinct outputs Male, Female, NA, Unknown

covid_data%>%
  distinct(died_covid) # 4 distinct outputs No, NA, Yes, Under Review 

# Filtered for only people who died from covid
gender_vs_covid_death <- covid_data %>% 
  select(pid, case_gender, died_covid) %>% 
  filter(died_covid == "Yes") 

# check for duplicate rows in the data
gender_vs_covid_death %>% 
  duplicated() %>% # produces a list of TRUE/FALSE statements for duplicated or not
  sum() # sums all the TRUE statements

# checking for typos in answers
gender_vs_covid_death %>%
  distinct(case_gender, died_covid)

