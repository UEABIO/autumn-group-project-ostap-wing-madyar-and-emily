# BIO-5023Y Autumn Summative Group Project Script---- 
## Tsz-Ching Chiu (Emily)
### Topic: How indicative is the symptom of headache for a positive COVID-10 diagnosis?

#_____________________----

# SET UP----
## Packages----
library(tidyverse) #for data analysis and plotting
library(colorBlindness) #check plot accessibility

## Load R Objects and Functions----
source("scripts/cleaning_data.R") #load cleaned data from separate script

#_____________________----

# REVIEW----
## select variables from original data----
covid_data %>%
  group_by(confirmed_case, sym_headache) %>%  # Group data by the 2 variables
  summarise(n = n()) # Output all different values per variable, including NAs

##_____________________----

## Obtain count for NAs----
covid_data %>%
  dplyr::summarise(case_n = sum(is.na(confirmed_case))) # NAs in confirmed cases

covid_data %>%
  dplyr::summarise(headache_n = sum(is.na(sym_headache))) #NAs in headache symptom

## Check for total number of positive cases----
covid_data %>%
  select(confirmed_case)%>% # Selecting cases variable from cleaned data
  filter(confirmed_case == "Yes") %>% #Filter only positive cases
  summarise(n=n()) #count: 82061

##_____________________----

## Check for frequency of both variables combined----
covid_data %>% 
  group_by(confirmed_case, sym_headache) %>% 
  summarise(n=n(),
            n_distinct=n_distinct(pid)) %>% #number of unique cases
  ungroup() %>% #remove group calculations
  mutate(freq=n_distinct/sum(n_distinct)) %>% # calculate percentage of each group
  mutate()
#Difference in values for n and n_distinct indicates repeated observations

#_____________________----

#SUBSET OF DATA----
sym_case <- select(.data = covid_data, 
                   c(pid, confirmed_case, sym_headache)) #select variables

sym_case <- sym_case %>%
  filter(confirmed_case %in% c("Yes")) %>% #keep only positive covid results
  rename(case = confirmed_case,
         headache = sym_headache) #rename variable names to be shorter

head(sym_case) #review

##_____________________----

## Check the frequency for different status of headache reported----
sym_case %>% 
  group_by(headache) %>% 
  summarise(n=n(),
            n_distinct=n_distinct(pid)) %>% #number of unique cases
  ungroup() %>% #remove group calculations
  mutate(freq=n/sum(n)) %>% # calculate percentage of each group
  mutate()
#Difference in values for n and n_distinct indicates repeated observations

##_____________________----

##Group "Unk" (unknown) and "NA" observations of headache----
sym_case_grp <- sym_case %>% 
  mutate(headache = replace_na(headache, "Unk")) %>%
  mutate(headache = case_when(headache == "Unk" ~ "Unknown",
                              headache == "Yes" ~ "Yes",
                              headache == "No" ~ "No"))

##_____________________----

##Recheck the frequency----
sym_case_grp_n <- sym_case_grp %>% 
  group_by(headache) %>% 
  summarise(n=n(),
            n_distinct=n_distinct(pid)) %>% #number of unique cases
  ungroup() %>% #remove group calculations
  mutate(freq=n/sum(n)) %>% # obtain relative frequency
  mutate()

sym_case_grp_n <-sym_case_grp_n %>%
  mutate(freq = round(freq, digits = 2)) %>% #round off the relative frequency
  mutate(percent = freq * 100) #obtain percentage

sym_case_grp_n <- sym_case_grp_n %>%
  mutate(headache = fct_relevel(headache, "Unknown", "No", "Yes")) #reorder level

#_____________________----

# VISUALISATION----

## Preparation----
total <-sym_case_grp_n %>%
  summarise(total = sum(n))%>% #obtain the numerical value of the total number of positive cases
  pull() #extract the number from the tibble

##_____________________----

## Create Bar Plot----
headache_bar <- sym_case_grp_n %>% #save the plot
  ggplot(aes(x=n_distinct, y=headache, fill = headache))+ #select variables
  geom_col(width = 0.6, colour = "black")+ #adjust bar width and apply borders to increase visibility
  xlim(0, 35000)+
  labs(x = "Number of cases",
       y = "Symptom of headache experienced", #provide labels for both axes
       title = "The symptom of headache does not indicate a positive COVID-19 diagnosis",
       subtitle = paste0("Sample Size: ", total," positive cases"))+ #combine the numerical value with the string
  geom_text(aes(label = scales::percent(freq)), hjust = -0.15)+ #display percentage with the location adjusted
  scale_fill_manual(values=c("darkblue", #Apply colours to the corresponding categories
                             "deepskyblue4",
                             "deepskyblue"))+
  theme_classic()+ #specify theme
  theme(legend.position = "none") #remove any legend

##_____________________----

## Check colour scheme accessibility----
colorBlindness::cvdPlot()

##_____________________----

## Output----
ggsave("figures/emily_headache_barplot.png", # Assign the folder and file name
       plot= headache_bar, # Assign the plot to be saved
       dpi=300, # Setting resolution
       width = 8, #Setting width
       height= 6) #Setting width
