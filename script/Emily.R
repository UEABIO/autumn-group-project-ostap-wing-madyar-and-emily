# BIO-5023Y Autumn Summative Group Project Script---- 
## Tsz-ching Chiu (Emily)

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
covid_symp_case <- select(.data = covid_data_raw, #data from cleaning script
                          confirmed_case, sym_fever, sym_subjfever, sym_myalgia, sym_losstastesmell, 
                          sym_sorethroat, sym_cough, sym_headache)

# Consider filter or adjust into counts
# column = positive / negative cases
# rows = various symptoms
# cells = indicate count

#_____________________----
# Create Plot ----

#_____________________----

# Check Accessibility----
## colorBlindness::cvdPlot()

# Export final plot as png file into separate figures folder (to be created)