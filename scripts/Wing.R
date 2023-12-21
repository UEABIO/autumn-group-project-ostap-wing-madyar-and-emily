# Title: BIO-5023Y 23-24 autumn group project ----
## Wing Lun Lim
### An analysis and plot of covid data - gender vs covid deaths

#___________________----

# Setting up loading packages ----
library(colorBlindness) #check accessibility of plot to colour blind users

#___________________-----

# Loading objects and functions from main cleaning script ----
source("scripts/cleaning_data.R") # Import tidied covid data

#___________________-----

# Exploring the variables for my plot ----

# Brief overview of data
glimpse(covid_data)
summary(covid_data)

# Checking population sampled 
covid_data%>%
  select(died_covid)%>% # Selecting covid death variable
  summarise(n=n()) #Calculating all individuals involved (82100)

# Checking total deaths from people sampled
covid_data %>% 
  select(died_covid) %>% #Selects covid death variable
  filter(died_covid == "Yes") %>% # Include only deaths
  summarise (n=n()) # Summarizes total deaths (1338)

# Checking for any unexpected values such as NA and unknown in age and death variables
# Checking for typos
covid_data %>%
  group_by(case_gender, died_covid) %>%  # Grouping data by the 2 variables
  summarise(n = n()) # Outputting all the values per variable

# Checking for distinct values in covid death variable
covid_data %>% 
  select(died_covid)%>% # Selecting the variable
  group_by(died_covid)%>% # Grouping by deaths
  summarise(n=n()) # Outputting all distinct values and number per category

# Checking for any other values than male or female, in gender variable
covid_data %>%
  select(case_gender)%>% # Selecting the variable
  group_by(case_gender)%>% # Grouping by gender
  summarise(n=n()) # Outputting all distinct values and number per category

# Calculating how many NA in died_covid variable 
covid_data %>%
  dplyr::summarise(count_na_died_covid = sum (is.na(died_covid)))

# Calculating how many NA in gender variable before filtering for "Yes" deaths
covid_data %>%
  dplyr::summarise(count_na_gender = sum (is.na(case_gender)))

#___________________----

# Narrowing down the data frame for the interested variables ----
gender_vs_covid_death <- covid_data %>%
  select(pid, case_gender, died_covid) %>% # selecting variables of interest
  filter(died_covid == "Yes", # including only "Yes" values
         case_gender == "Male" | case_gender == "Female") #Filter for only "Male" and "Female in the gender variable

gender_vs_covid_death %>%
  select(case_gender)%>% # Selecting the variable
  group_by(case_gender)%>% # Grouping by gender
  summarise(n=n()) # Outputting all distinct values and number per category

#___________________----

# Briefly exploring data distribution of deaths across genders to inform gender categories----
## Briefly plotting data to visualize for data analysis and interpretation
ggplot(data = gender_vs_covid_death, # Selecting data
       aes(x = case_gender)) + # Setting x axis
  geom_bar() # Building bar plot with death frequency/count
# Insight: Male with more deaths than Females, missing data due to NAs and "Under Review"

#___________________----

# Making statistics for the plot----
gender_death_stats <- gender_vs_covid_death %>% 
  group_by(case_gender) %>% # Grouping by gender
  summarise(n = n()) %>% # Shows deaths by gender category
  mutate(gender_death_prob = n/sum(n)) # Adding new variable for percentage of deaths from each gender

#___________________----

# Building final bar plot to represent deaths by gender category ----
gender_death_bar_plot <- gender_vs_covid_death %>%  # Saving my plot into object
  ggplot(aes(x=case_gender), colour= "black") + #Setting x-axis variable and colour
  geom_bar (aes(fill=case_gender), # Filling in the bars according to gender categories
            width = 0.7, # Setting bar widths
            colour= "black") + 
  labs(x="Gender \n",  
       y = "\n Number of deaths", # Labeling yboth axes
       title= "COVID-19 Deaths By Gender")+ # Insert plot title
  geom_text(data=gender_death_stats,   # Providing data for bar text
            aes(y=(n+50), # Shifting bar text to make it more visually accessible
                x=case_gender,   # Stating location of bar text
                label=scales::percent(gender_death_prob))) +  #Inserting probability text as a percentage
  scale_fill_manual(values=c("deepskyblue",
                             "darkblue"))+
  coord_flip()+ # Flipping coordinate axis
  theme_classic() + # Setting preferred theme
  theme(plot.title = element_text(hjust = 0.5))+ # Making title central
  theme(legend.position = "none") + # Removing legend
  scale_y_continuous(breaks = seq(0, 750, 50)) # Specify maximum range on flipped y-axis, in intervals of 50

#___________________----

# Checking and ensuring accessibility for colorblindness ----
colorBlindness::cvdPlot()

#___________________----

# Saving the final plot as png----
ggsave("figures/wing_c19gender_barplot.png", # Assigning folder and name to the file
       plot= gender_death_bar_plot, # Assigning my plot to be saved
       dpi=300, # Setting resolution
       width = 7, #Setting width
       height= 5) #Setting width

#___________________----

# Informative statistics for the figure legend
covid_data %>%
  select(case_gender, confirmed_case)%>% # Selecting the variable (gender and confirmed case)
  group_by(case_gender, confirmed_case)%>% # Grouping by gender and cofirmed case
  summarise(n=n())
