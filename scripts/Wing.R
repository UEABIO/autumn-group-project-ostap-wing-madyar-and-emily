# Title: BIO-5023Y 23-24 autumn group project ----
## Wing Lun Lim
### An analysis and plot of covid data - gender vs covid deaths
#### Data first published in  Batra, Neale et al. (2021), The Epidemiologist R Handbook

#___________________----

# Setting up loading packages ----
library(colorBlindness) #check accessibility of plot to colour blind users

#___________________-----

# Loading objects and functions from main cleaning script ----
# Import tidied covid data

source("scripts/cleaning_data.R")

#___________________-----

# Determining and exploring the variables needed for my plot ----

# Brief overview of data
glimpse(covid_data)
summary(covid_data)

# Checking population sampled 
covid_data%>%
  select(died_covid)%>% # Selecting died covid variable from dataset
  summarise(n=n()) #Calculating all individuals involved (82100)

# Checking total deaths from people sampled
covid_data %>% 
  select(died_covid) %>% #Selects death from covid variable
  filter(died_covid == "Yes") %>% # Filter only deaths
  summarise (n=n()) # Summarizes total deaths (1338)

# Checking for any unexpected values such as NA and unknown in the age and death from covid variables
# Looking if are any typos in the values
covid_data %>%
  group_by(case_gender, died_covid) %>%  # Grouping data by the 2 variables
  summarise(n = n()) # Outputting all the values per variable

# Checking for distinct values in death from covid variable
covid_data %>% 
  select(died_covid)%>% # Selecting the variable (died covid)
  group_by(died_covid)%>% # Grouping by deaths
  summarise(n=n()) # Outputting all distinct values and number per category

# Checking for any other values than male or female, in gender variable
covid_data %>%
  select(case_gender)%>% # Selecting the variable (gender)
  group_by(case_gender)%>% # Grouping by gender
  summarise(n=n()) # Outputting all distinct values and number per category

# Calculating how many NA in died_covid variable 

covid_data %>%
  dplyr::summarise(count_na_died_covid = sum (is.na(died_covid))) # NAs in died_covid variable

# Calculating how many NA in gender variable before filtering for "Yes" deaths

covid_data %>%
  dplyr::summarise(count_na_gender = sum (is.na(case_gender))) # NAs in gender variable

#___________________----

# Narrowing down data frame for the interested variables ----
# New variable for the data frame to contain only individuals that died from covid from different gender
gender_vs_covid_death <- covid_data %>%
  select(pid, case_gender, died_covid) %>% # selecting variables of interest
  filter(died_covid == "Yes", case_gender == "Male" | case_gender == "Female") # including only "Yes" values in the death from covid variable, Filter for only "Male" and "Female in the gender variable, removes any NA/ unknown  

gender_vs_covid_death %>%
  select(case_gender)%>% # Selecting the variable (gender)
  group_by(case_gender)%>% # Grouping by gender
  summarise(n=n()) # Outputting all distinct values and number per category

#___________________----

# Briefly exploring data distribution of deaths across genders to inform gender categories----
## Briefly plotting data to visualize. Allows analyzing and interpreting data
ggplot(data = gender_vs_covid_death, # Selecting data
       aes(x = case_gender)) + # Setting x axis
  geom_bar() # Building bar plot with death frequency/count
# Insight - Male with more deaths than Females. Interesting as more males in sample than females.
# Some data is missing due to NAs and Under Review

#___________________----

# Making statistics for the plot----
gender_death_stats <- gender_vs_covid_death %>% 
  group_by(case_gender) %>% # Grouping by gender
  summarise(n = n()) %>% # Shows deaths by gender category
  mutate(gender_death_prob = n/sum(n)) # Adding new variable, percentage of deaths from each gender

#___________________----

# Building final plot ----
# Bar plot representing deaths per gender category 
gender_death_bar_plot <- gender_vs_covid_death %>%      # Saving my plot into object
  ggplot(aes(x=case_gender), colour= "black") +       #Setting x-axis variable and colour
  geom_bar (aes(fill=case_gender),   # Filling in the bars according to gender categories
            width = 0.7,
            colour= "black") +      #Setting width of bars
  labs(x="Gender \n",       # Labeling x-axis
       y = "\n Number of deaths",      # Labeling y-axis
       title= "COVID-19 Deaths By Gender")+ # Insert title of plot
  geom_text(data=gender_death_stats,   # Providing data for bar text
            aes(y=(n+50),       # Shifting bar text to make it more visually accessible with more clarity 
                x=case_gender,        # Stating where bar text should be
                label=scales::percent(gender_death_prob))) +    #Inserting probability text and converting it into a percentage
  scale_fill_manual(values=c("deepskyblue",
                             "darkblue"))+
  coord_flip()+     # Flipping coordinate axis, more professional look
  theme_classic() +    # Setting prefered theme
  theme(plot.title = element_text(hjust = 0.5))+ # Making title central
  theme(legend.position = "none") + # Removing legend
  scale_y_continuous(breaks = seq(0, 750, 50)) # Making max range 750 on flipped y-axis, in intervals of 50. For better clarity of where the smallest bar stop

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
