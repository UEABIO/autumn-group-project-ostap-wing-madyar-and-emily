# Title: BIO-5023Y 23-24 autumn group project ----
## Ostap
### Exploring death rates on age categories

#___________________----

# Setting up packages ----
library(colorBlindness) #check plot accessibility

#___________________-----

# Loading objects and functions from main cleaning script ----
source("scripts/cleaning_data.R") # Import tidied covid data

#___________________-----

# Understanding the data----

# Renaming the variable in the new data frame with dplyr::rename from tidyverse
covid_data <- rename(covid_data,
                     "case_dates"="reprt_creationdt_false")

# Converting the character date values to calculate the study period
covid_data <- covid_data %>%
  mutate(case_dates = lubridate::dmy(case_dates))  #Converting into date format

# Calculating the study period 
covid_data %>% 
  summarise(first_case=min(case_dates), # Pulling out the first date of case
            last_case=max(case_dates),  # Pulling out the last date of case
            study_duration_months = (last_case-first_case)/lubridate::dmonths(1)) # Calculating the period in months

#___________________-----

# Exploring the variables of interest----

# Brief overview of data
glimpse(covid_data)

# Population sampled
covid_data%>%
  select(died_covid)%>% # Selecting variable for covid deaths
  summarise(n=n()) #Calculate total deaths

# Checking total deaths
covid_data %>% 
  select(died_covid) %>% #Select variable
  filter(died_covid == "Yes") %>% # Include death only
  summarise (n=n()) # Summarizes total deaths

# Checking death age range for plotting
covid_data %>% 
  select (case_age, died_covid) %>% # Selects the 2 variables
  filter(died_covid == "Yes") %>% # Include death only
  summarise(youngest=min(case_age),
            oldest=max(case_age)) # Retrieves minimum and maximum age groups

# Checking for any unexpected values such as NA and unknown in both variables
# Checking for typos
covid_data %>%
  group_by(case_age, died_covid) %>%  # Group the data by the 2 variables
  summarise(n = n()) # Output all the values per variable

# Checking for distinct values in death from covid variable
covid_data %>% 
  select(died_covid)%>% # Selecting the variable
  group_by(died_covid)%>% # Grouping by deaths
  summarise(n=n()) # Outputting all distinct values and number per category

# Checking for any other values than sensible positive numerical (age), in age variable
covid_data %>%
  select(case_age)%>% # Selecting the variable
  arrange(case_age)%>% # Arrange in ascending order
  distinct() # Output: all distinct values

# Calculating for number of NAs in age variable before filtering for "Yes" deaths
covid_data %>%
  dplyr::summarise(count_na_age = sum (is.na(case_age))) # NAs in age variable

#___________________----

# Narrowing down the data frame with the interested variables only----
age_vs_covid_death <- covid_data %>%
  select(pid, case_age, died_covid) %>% # select variables of interest
  filter(died_covid == "Yes") # include only "Yes" values in the covid death variable

#___________________----

# Exploring data distribution of deaths across all ages----
ggplot(data = age_vs_covid_death, # Feeding data
       aes(x = case_age)) + # Setting x axis
  geom_bar() # Building bar plot with death frequency/count
# Insight: more deaths with increasing age.
# Missing data due to NAs and other uninformative values will be excluded

#___________________----

# Creating and saving age categories for deaths into a new variable----
# The previous bar plot suggests grouping cases of above 80 years old as a single age category for a clearer message
age_vs_covid_death <- age_vs_covid_death %>% # Saving the data in original object
  mutate(age_range = case_when(case_age <= 29 ~ "19-29", # Adding a variable to group deaths by age groups
                               case_age >= 30 & case_age <= 39 ~ "30-39",
                               case_age >= 40 & case_age <= 49 ~ "40-49",
                               case_age >= 50 & case_age <= 59 ~ "50-59",
                               case_age >= 60 & case_age <= 69 ~ "60-69",
                               case_age >= 70 & case_age <= 79 ~ "70-79",
                               case_age >= 80 ~ "80+")) %>%
  group_by(age_range) # Group deaths into age categories

#___________________----

# Making some statistics for the plot----
age_death_stats <- age_vs_covid_death %>% 
  group_by(age_range) %>%  # Group by age
  summarise(n = n()) %>% # Shows deaths by age category
  mutate(age_cat_prob = n/sum(n)) # Add new variable for the frequency of deaths per age category / total deaths

#___________________----

# Gathering information (death rate) for y-axis label----
covid_data %>%
  select (died_covid) %>% #Select variable
  filter (died_covid %in% c("No","Yes")) %>% # Filter to include "Yes" and "No"
  summarise(n = n()) # Summarize the total value

#___________________----

# Building final bar plot to represent deaths per age category with proportion out of total population sampled---
age_death_bar_plot <- age_vs_covid_death %>%  # Saving the plot
  ggplot(aes(x=age_range), colour= "black") +  #Setting x-axis variable
  geom_bar (aes(fill=age_range),   # Filling in the bars according to age categories
            width = 0.7, #Setting bar width
            colour= "black") +      
  labs(x="Age groups \n",
       y = "\n Number of deaths per 40 000",  # Label both axes with indent for clarity
       title= "The number of COVID19 deaths in the U.S. by the age",
       subtitle= "Sample Size: 39676 cases (excluded 'NA' and 'Under Review')") + # Insert title and sub-title
  geom_text(data=age_death_stats, # Providing data for bar text
            aes(y=(n+35), # Shifting bar text to make it visually accessible
                x=age_range, # Specify location of bar text
                label=scales::percent(age_cat_prob))) + #Insert probability as a percentage
  scale_fill_manual(values=c("deepskyblue",
                             "deepskyblue",
                             "deepskyblue",
                             "deepskyblue",
                             "deepskyblue",
                             "deepskyblue",
                             "darkblue"))+
  coord_flip()+  # Flipping coordinate axis
  theme_classic() + # Specify theme
  theme(legend.position = "none") + # Removing legend position
  scale_y_continuous(breaks = seq(0, 600, 50)) # Specify maximum value and interval snip for the flipped y-axis

#___________________----

# Checking the colour scheme accessibility----
colorBlindness::cvdPlot()

#___________________----

# Saving the final plot as png----
ggsave("figures/ostap_07_12_2023_5023Y_minireport_barplot.png", # Assigning folder and name to the file
       plot= age_death_bar_plot, # Assigning my plot to be saved
       dpi=300, # Setting resolution
       width = 6, #Setting width
       height= 5) #Setting width
