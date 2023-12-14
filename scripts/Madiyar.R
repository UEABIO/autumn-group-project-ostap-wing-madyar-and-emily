# Script for 5023Y Group Project----
## Script author: Madiyar Seidaly
### Topic: investigating the different symptoms and rate of hospitalizations 

# ____________________----

# IMPORTING CLEAN DATA----

source("scripts/cleaning_data.R")

# ____________________----

# FURTHER CLEANING----

# Selecting only symptom and hospitalization data

symptom_data <- select(.data = covid_data,
                       sym_fever, sym_myalgia, sym_losstastesmell, sym_sorethroat, sym_cough, sym_headache, hospitalized)

# Renaming variables to be easier to work with

symptom_data <- rename(symptom_data,
                       "sym_fev" = "sym_fever",
                       "sym_mya" = "sym_myalgia",
                       "sym_lts" = "sym_losstastesmell",
                       "sym_sot" = "sym_sorethroat",
                       "sym_cog" = "sym_cough",
                       "sym_hdc" = "sym_headache")

# Renaming "Unk" values to "NA"

is.na(symptom_data) <- symptom_data == "Unk"

# Renaming "YES" values to "Yes" in the "sym_mya" column

symptom_data <- symptom_data %>%
  mutate(sym_mya = case_when(sym_mya == "YES" ~ "Yes",
                             sym_mya == "Yes" ~ "Yes",
                             sym_mya == "No" ~ "No"))

# Removing NA data

symptom_data <- drop_na(symptom_data)

# ____________________----

# COUNTING VALUES

# Plot will be a Grouped+Stacked bar plot, showcasing the number of 
# [hospitalized yes/no] occurrences for [individual symptom + multiple symptom]

# Values to collect - individual symptom = Yes + multiple symptom = Yes, when hospitalized = Yes
#                   - individual symptom = Yes + multiple symptom = Yes, when hospitalized = No

# individual symptom is defined as the count of occurrences with ONLY the key 
# symptom being positive.
# multiple symptom is defined as the count of all occurrences of the individual 
# symptom being positive ALONGSIDE other symptoms.

# In the plot, multiple symptom would have individual symptom subtracted from 
# the count, so that individual may be stacked under to highlight independent symptom occurrences.

# To accomplish this, I will need to separate symptoms (eg sym_fev) into integer
# objects "sym_fev_solo" and "sym_fev_mult"

symptom_data %>%
  select(sym_fev, hospitalized) %>%
  filter(sym_fev == "Yes",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 733

symptom_data %>%
  select(sym_fev, hospitalized) %>%
  filter(sym_fev == "Yes",
         hospitalized == "No") %>%
  summarise(n=n()) # 7217
