# Title: BIO-5023Y 23-24 autumn group project ----
## Ostap
### Exploring death rates on age categories

#__________________----

# Setting up personal loading packages ----
library(colorBlindness) #check plot accessibility

#__________________-----

# Loading objects and functions from main cleaning script ----
# Import tidied covid data

source("scripts/cleaning_data.R")

#__________________-----

# Exploring the variables of interest for my plot ----

# Brief overview of data
glimpse(covid_data)

# Population sampled
covid_data%>%
  select(died_covid)%>% # Selecting death from covid variable from dataset
  summarise(n=n()) #Calculating all individuals involved

# Checking total deaths
covid_data %>% 
  select(died_covid) %>% #Selects death from covid variable
  filter(died_covid == "Yes") %>% # Filter only deaths
  summarise (n=n()) # Summarizes total deaths

# Checking death age range to inform of future graph plot
covid_data %>% 
  select (case_age, died_covid) %>% # Selects the 2 variables from data set
  filter(died_covid == "Yes") %>% # Filter only deaths
  summarise(youngest=min(case_age),
            oldest=max(case_age)) # Retrieves min and max age groups

# Checking for any unexpected values such as NA and unknown in the age and death from covid variables
# Looking if are any typos in the values
covid_data %>%
  group_by(case_age, died_covid) %>%  # Grouping data by the 2 variables
  summarise(n = n()) # Outputting all the values per variable

# Checking for distinct values in death from covid variable
covid_data %>% 
  select(died_covid)%>% # Selecting the variable
  group_by(died_covid)%>% # Grouping by deaths
  summarise(n=n()) # Outputting all distinct values and number per category

# Checking for any other values than sensible positive numerical (age), in age variable
covid_data %>%
  select(case_age)%>% # Selecting the variable
  arrange(case_age)%>% # Arranging from lowest to highest
  distinct() # Outputting all distinct values

# Calculating how many NA in age variable before filtering for "Yes" deaths

covid_data %>%
  dplyr::summarise(count_na_age = sum (is.na(case_age))) # NAs in age variable

#_______________----

# Narrowing down data frame for the interested variables ----
# New variable for the data frame to contain only individuals that died from covid from different ages
age_vs_covid_death <- covid_data %>%
  select(pid, case_age, died_covid) %>% # selecting variables of interest
  filter(died_covid == "Yes") # including only "Yes" values in the death from covid variable, removes any NA/ unknown

# _____________----

# Exploring data distribution of deaths across all ages to inform into age categories----
ggplot(data = age_vs_covid_death, # Feeding data
       aes(x = case_age)) + # Setting x axis
  geom_bar() # Building bar plot with death frequency/count
# Insight - with increasing age more deaths.
# Can be understood that there are not a lot of old people above 80 roughly
# Some data is missing due to NAs and other uninformative values that cant be used

#______________----

# Making age categories for deaths and saving into new variable----
# Following the last bar plot it makes sense to group the individuals above 80 year into one age category ->
# -> for the purposes of clear message to audience
age_vs_covid_death <- age_vs_covid_death %>% # Saving the data in original object
  mutate(age_range = case_when(case_age <= 29 ~ "19-29", # Adding a variable to group deaths by age groups
                               case_age >= 30 & case_age <= 39 ~ "30-39",
                               case_age >= 40 & case_age <= 49 ~ "40-49",
                               case_age >= 50 & case_age <= 59 ~ "50-59",
                               case_age >= 60 & case_age <= 69 ~ "60-69",
                               case_age >= 70 & case_age <= 79 ~ "70-79",
                               case_age >= 80 ~ "80+")) %>%
  group_by(age_range) # Group deaths into age categories

# ______________----

# Making some statistics for the plot----
age_death_stats <- age_vs_covid_death %>% 
  group_by(age_range) %>%  # Grouping by age
  summarise(n = n()) %>% # Shows deaths by age category
  mutate(age_cat_prob = n/sum(n)) # Adding new variable involving frequency of deaths per age category / total number died

#_______________----

# Gathering information for y-axis label----
# This data will be put on y axis and inform of death rate
covid_data %>%
  select (died_covid) %>% #Selecting died from covid variable from data set
  filter (died_covid %in% c("No","Yes")) %>% # Filtering by "Yes" and "No"
  summarise(n = n()) # Summarizing the total value and will round for sake of clear message to audience

#_______________----


# Building final plot ----
# Bar plot representing deaths per age category with proportion out of total population
age_death_bar_plot <- age_vs_covid_death %>%      # Saving my plot
  ggplot(aes(x=age_range), colour= "black") +       #Setting x-axis variable
  geom_bar (aes(fill=age_range),   # Filling in the bars according to age categories
            width = 0.7,
            colour= "black") +      #Setting width of bars
  labs(x="Age groups \n",       # Labeling x-axis and making an indent for clarity
       y = "\n Number of deaths per 40 000",      # Labeling y-axis and making an indent for clarity
       title= "Number of COVID19 deaths by age categories", # Inserting a tittle
       subtitle= "Proportions of individual groups") +       # Inserting a sub-tittle
  geom_text(data=age_death_stats,   # Providing data for bar text
            aes(y=(n+35),       # Shifting bar text to make is visually accessible
                x=age_range,        # Stating where I like my bar text to be
                label=scales::percent(age_cat_prob))) +    #Inserting probability text and converting into a percentage
  scale_fill_manual(values=c("deepskyblue",
                             "deepskyblue",
                             "deepskyblue",
                             "deepskyblue",
                             "deepskyblue",
                             "deepskyblue",
                             "darkblue"))+
  coord_flip()+     # Flipping coordinate axis
  theme_classic() +    # Setting theme
  theme(legend.position = "none") + # Removing legend position
  scale_y_continuous(breaks = seq(0, 600, 50)) # Making max range 600 on flipped y-axis, in 50 intervals snips.
# For better clarity of where the smallest bar stop

#______________----


# Checking accessibility for colorblind people----
colorBlindness::cvdPlot()


#______________----

# Saving the final plot as png----
ggsave("figures/ostap_07_12_2023_5023Y_minireport_barplot.png", # Assigning folder and name to the file
       plot= age_death_bar_plot, # Assigning my plot to be saved
       dpi=300, # Setting resolution
       width = 9, #Setting width
       height= 7) #Setting width

# _________________----






