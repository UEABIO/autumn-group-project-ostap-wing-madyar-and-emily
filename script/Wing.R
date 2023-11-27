#___________________________----
# SET UP ----
## An analysis and plot of covid data - gender vs covid deaths  ----

### Data first published in  Batra, Neale et al. (2021), The Epidemiologist R Handbook. ------
#__________________________----
# PACKAGES ----
library(colorBlindness) #check plot accessibility
#__________________________----
# Import tided data and load r objects and functions 
source("scripts/cleaning_data.R")
#__________________________----
# Filter data to find only people who died from covid 
filter(.data = covid_data_raw, died_covid == "Yes")