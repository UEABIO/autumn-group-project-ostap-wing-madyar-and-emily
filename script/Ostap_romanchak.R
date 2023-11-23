# Title: BIO-5023Y 23-24 autumn group project ----
## Ostap
### Exploring confirmed cases vs death from covid

#__________________----
# SET UP: Loading packages ----

library(tidyverse) # tidy data packages
library(janitor) # cleans variable names
library(patchwork) # To make final figure
library(colorBlindness) #check plot accessibility

#__________________-----

# Importing the data ----
covid_data_raw <- read_csv("data/covid_example_data.csv")
#__________________------


