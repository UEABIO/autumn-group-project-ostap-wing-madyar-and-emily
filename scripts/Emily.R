# BIO-5023Y Autumn Summative Group Project Script---- 
## Tsz-Ching Chiu (Emily)
### Topic: In confirmed cases, how indicative is the symptom of headache?

#_____________________----

# SET UP----
## Packages----
library(tidyverse)
library(colorBlindness) #check accessibility of plots by other teammates

## Load R Objects and Functions----
source("scripts/cleaning_data.R") #from separate script for cleaning

#_____________________----

# REVIEW----
## select variables from original data----
covid_data %>%
  group_by(confirmed_case, sym_headache) %>%  # Grouping data by the 2 variables
  summarise(n = n()) # Outputting all different values per variable, include NAs

##_____________________----

## Obtain count for NAs----
covid_data %>%
  dplyr::summarise(case_n = sum(is.na(confirmed_case))) # NAs in confirmed cases

covid_data %>%
  dplyr::summarise(headache_n = sum(is.na(sym_headache))) #NAs in headache symptom

## Check for total number of positive cases----
covid_data %>%
  select(confirmed_case)%>% # Selecting cases variable from original dataframe
  filter(confirmed_case == "Yes") %>% #Filter only positive cases
  summarise(n=n()) #calculate the count: 82061

##_____________________----

## Check for frequency of both variables combined----
covid_data %>% 
  group_by(confirmed_case, sym_headache) %>% 
  summarise(n=n(),
            n_distinct=n_distinct(pid)) %>% #number of unique cases
  ungroup() %>% #remove group calculations
  mutate(freq=n_distinct/sum(n_distinct)) %>% # calculate percentage of each group across the dataset
  mutate()
# By comparing n and n_distinct, some repeated values are observed

#_____________________----

#SUBSET OF DATA----
sym_case <- select(.data = covid_data, 
                   c(pid, confirmed_case, sym_headache)) #select variables of interest

sym_case <- sym_case %>%
  filter(confirmed_case %in% c("Yes")) %>% #keep only positive covid results
  rename(case = confirmed_case,
         headache = sym_headache) #rename variable names to be shorter

head(sym_case) #review

## Check the frequency of positive and negative cases----
sym_case %>% 
  group_by(headache) %>% 
  summarise(n=n(),
            n_distinct=n_distinct(pid)) %>% #number of unique cases
  ungroup() %>% #remove group calculations
  mutate(freq=n/sum(n)) %>% # calculate percentage of each group across the dataset
  mutate()
#Difference in values for n and n_distinct indicates repeated observations

##Group "Unk" (unknown) and "NA" observations of headache----
sym_case_grp <- sym_case %>% 
  mutate(headache = replace_na(headache, "Unk")) %>%
  mutate(headache = case_when(headache == "Unk" ~ "Unknown",
                              headache == "Yes" ~ "Yes",
                              headache == "No" ~ "No"))

##Recheck the frequency----
sym_case_grp_n <- sym_case_grp %>% 
  group_by(headache) %>% 
  summarise(n=n(),
            n_distinct=n_distinct(pid)) %>% #number of unique cases
  ungroup() %>% #remove group calculations
  mutate(freq=n/sum(n)) %>% # obtain relative frequency
  mutate()

sym_case_grp_n <-sym_case_grp_n %>%
  mutate(freq = round(freq, digits = 2)) %>% #round off the relative frequency to 3 digits
  mutate(percent = freq * 100) #obtain percentage

sym_case_grp_n <- sym_case_grp_n %>%
  mutate(headache = fct_relevel(headache, "Unknown", "No", "Yes")) #reorder level

#_____________________----

# VISUALISATION----

## Preparation----
total <-sym_case_grp_n %>%
  summarise(total = sum(n))%>% #obtain value of the total number of positive cases
  pull() #extract the numerical value from the tibble

## Create Bar Plot----
headache_bar <- sym_case_grp_n %>% #save the plot
  ggplot(aes(x=n_distinct, y=headache, fill = headache))+ #select variables
  geom_col(width = 0.6)+ #adjust width of the bars
  xlim(0, 35000)+
  labs(x = "Number of cases",
       y = "Symptom of headache experienced", #provide labels for both axes
       title = "The symptom of headache is not indicative of a positive COVID-19 diagnosis",
       subtitle = paste0("Sample Size: ", total," positive cases"))+ #combine the variable for total number of positive cases into the string
  geom_text(aes(label = scales::percent(freq)), hjust = -0.15)+ #display percentage
  scale_fill_manual(values=c("darkblue", #Apply colours to the corresponding categories to highlight the unconfirmed cases
                             "deepskyblue4",
                             "deepskyblue"))+
  theme_classic()+
  theme(legend.position = "none")

## Checking accessibility for colorblind people----
colorBlindness::cvdPlot()

## Output----
ggsave("figures/emily_headache_barplot.png", # Assign folder and name to the file
       plot= headache_bar, # Assign my plot to be saved
       dpi=300, # Setting resolution
       width = 8, #Setting width
       height= 6) #Setting width
