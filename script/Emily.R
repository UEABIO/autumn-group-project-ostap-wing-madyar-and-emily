# BIO-5023Y Autumn Summative Group Project Script---- 
## Tsz-Ching Chiu (Emily)

### Data first published in Batra, Neale et al. (2021), The Epidemiologist R Handbook. ------
### Figure topic: evaluates if the symptom of headache indicates COVID-19 diagnosis

#_____________________----

# SET UP----
## Packages----
library(patchwork) # compile figure, might remove
library(colorBlindness) #check plot accessibility

## Load R Objects and Functions----
source("script/cleaning_data.R") #from separate script for cleaning

#_____________________----

# DATA EXPLORATION----

## Select variables----
sym_case <- select(.data = covid_data, c(pid, confirmed_case, sym_headache))

## filter cases to include only positive or negative covid results
sym_case <- filter(.data = sym_case, confirmed_case %in% c("Yes", "No"))

## group other observations as 'uncertain' (e.g. pending, unknown, N/A)
sym_case_summary <- sym_case %>% 
  group_by(confirmed_case, sym_headache) %>% 
  summarise(n=n(),
            n_distinct=n_distinct(pid)) %>% #number of unique cases
  ungroup() %>% #remove group calculations
  mutate(freq=n/sum(n)) # calculate percentage of each group across the dataset

sym_case_summary

#_____________________----
# VISUALISATION: Create Bar Plot ----

#_____________________----

## Check Accessibility----
colorBlindness::cvdPlot()

# Export final plot as png file into separate figures folder (to be created)