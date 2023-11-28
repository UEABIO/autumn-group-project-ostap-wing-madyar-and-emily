# BIO-5023Y Autumn Summative Group Project Script---- 
## Tsz-Ching Chiu (Emily)

### Data first published in Batra, Neale et al. (2021), The Epidemiologist R Handbook. ------
### Figure topic: Explore how symptoms indicate COVID-19 diagnosis

#_____________________----

# SET UP----
## Packages----
library(patchwork) # compile figure, might remove
library(colorBlindness) #check plot accessibility

## Load R Objects and Functions----
source("script/cleaning_data.R") #from separate script for cleaning

#_____________________----

# EXPLORATORY ANALYSIS----

## Select variables----
sym_case <- select(.data = covid_data_raw, #data from cleaning script
                          confirmed_case, sym_headache, PID)

### Overview----
glimpse(sym_case) 
summary(sym_case) #Both variables are categorical nominal variables.

## Overview of both variables 
case_n <- sym_case %>% 
  group_by(confirmed_case) %>% 
  summarise(n = n()) %>% #frequency of distinct values
  mutate(prob_obs = n/sum(n)) #relative frequency
case_n #Action: Remove pending and NA

headache_n <- sym_case %>%
  group_by(sym_headache) %>%
  summarise(n=n()) %>% #frequency of distinct values
  mutate(prob_obs = n/sum(n)) #relative frequency
headache_n  #Action: remove Unk/NA

## Filter to only include "Yes" and "No" in both variables----
sym_case_cleaned <- filter(.data = sym_case, confirmed_case %in% c("Yes", "No"), sym_headache %in% c("Yes", "No"))

summary(sym_case_cleaned)

# original count = 82101, with Unk, NA, Pending; after filter = 48864

sym_case_summary <- sym_case_cleaned %>% 
  group_by(confirmed_case, sym_headache) %>% 
  summarise(n=n(),
            n_distinct=n_distinct(PID)) %>% 
  ungroup() %>% # needed to remove group calculations
  mutate(freq=n/sum(n)) # then calculates percentage of each group across WHOLE dataset

sym_case_summary
#_____________________----
# Create Plot ----

#_____________________----

# Check Accessibility----
## colorBlindness::cvdPlot()

# Export final plot as png file into separate figures folder (to be created)