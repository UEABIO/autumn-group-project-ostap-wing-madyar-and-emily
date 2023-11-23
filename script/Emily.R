# BIO-5023Y Autumn Summative Group Project Script---- 
## Tsz-ching Chiu (Emily)
### Topic: Exploring confirmed cases and symptoms

# SET UP----
library(tidyverse) # core packages for data tidying, wrangling and visualisation
library(janitor) # clean variable names
library(patchwork) # compile figure, might remove
library(colorBlindness) #check plot accessibility

#Load R Objectve and Functions----
source("script/cleaning_data.R") #imported tidied/clean data and functions

#_____________________----

# FILTER----
#covid_symp_case_df <- covid_data_raw %>%
#  select()
# Create Plot ----

#_____________________----

# Check Accessibility----
## colorBlindness::cvdPlot()