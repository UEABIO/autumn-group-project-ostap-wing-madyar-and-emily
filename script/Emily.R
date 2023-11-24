# BIO-5023Y Autumn Summative Group Project Script---- 
## Tsz-Ching Chiu (Emily)

### Data first published in Batra, Neale et al. (2021), The Epidemiologist R Handbook. ------
### Figure topic: Explore how symptoms indicate COVID-19 diagnosis

#_____________________----

# SET UP----
## Packages----
library(tidyverse) # core packages for tidying, exploring, wrangling and visualising data
library(janitor) # clean variable names
library(patchwork) # compile figure, might remove
library(colorBlindness) #check plot accessibility

## Load R Objects and Functions----
source("script/cleaning_data.R") #from separate script for cleaning

#_____________________----

# EXPLORATORY ANALYSIS----

## Select relevant variables----
symp_case <- select(.data = covid_data_raw, #data from cleaning script
                          confirmed_case, sym_fever, sym_subjfever, sym_myalgia, sym_losstastesmell, 
                          sym_sorethroat, sym_cough, sym_headache)
#query source of sym_subjfever and sym_cough

## Overview of variables----
glimpse(symp_case) 
summary(symp_case)
### All variables are categorical nominal variables.

### Original frequency for each variable saved as separate object----
case_n <- symp_case %>% 
  group_by(confirmed_case) %>% 
  summarise(n = n()) #output for confirmed cases

fever_n <- symp_case %>%
  group_by(sym_fever) %>%
  summarise(n=n()) #output for fever symptoms

myalagia_n <- symp_case %>%
  group_by(sym_myalgia) %>%
  summarise(n=n()) #output for myalagia symptoms

loststastesmell_n <- symp_case %>%
  group_by(sym_losstastesmell) %>%
  summarise(n=n()) #output for loss of taste and smell

sorethroat_n <- symp_case %>%
  group_by(sym_sorethroat) %>%
  summarise(n=n()) #output for sore throat

headache_n <- symp_case %>%
  group_by(sym_sorethroat) %>%
  summarise(n=n()) #output for headache

### Frequency for cough and subjfever (N/A in README)
cough_n <- symp_case %>%
  group_by(sym_cough) %>%
  summarise(n=n()) #output for cough

subjfever_n <- symp_case %>%
  group_by(sym_subjfever) %>%
  summarise(n=n()) #output for subjfever
  
## each variable has 'yes', 'no', 'Na'; confirmed case also with 'pending', symptoms also with 'unk' for 'unknown'

#_____________________----
# Create Plot ----

#_____________________----

# Check Accessibility----
## colorBlindness::cvdPlot()

# Export final plot as png file into separate figures folder (to be created)