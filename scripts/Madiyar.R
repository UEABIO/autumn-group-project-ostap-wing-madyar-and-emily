# Script for 5023Y Group Project----
## Script author: Madiyar Seidaly
### Topic: investigating the different symptoms and rate of hospitalizations 

# ____________________----

# IMPORTING CLEAN DATA----

source("scripts/cleaning_data.R") # Importing cleaned up data

library(patchwork) # Loading the patchwork package

library(ggplot2) # Loading the ggplot2 package

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

# COUNTING VALUES----

# For each symptom, the counted values will include rows with positive values in 
# either only the specific symptom/the symptom along with others + Yes/No values
# for hospitalization

# When counting the rows with the desired symptom alongside others, the number
# of rows with only the specific symptom will be subtracted as they are already
# included within that selection, and I plan to stack both counts together in
# the final plot

symptom_data %>%      # Counting Only Fever + Hospitalized
  filter(sym_fev == "Yes",
         sym_mya == "No",
         sym_lts == "No",
         sym_cog == "No",
         sym_hdc == "No",
         sym_sot == "No",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 53

symptom_data %>%      # Counting All instances of Fever + Hospitalized
  select(sym_fev, hospitalized) %>%
  filter(sym_fev == "Yes",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 733 - 53 = 680

symptom_data %>%      # Counting Fever only + Not Hospitalized
  filter(sym_fev == "Yes",
         sym_mya == "No",
         sym_lts == "No",
         sym_cog == "No",
         sym_hdc == "No",
         sym_sot == "No",
         hospitalized == "No") %>%
  summarise(n=n()) # 502

symptom_data %>%      # Counting All instances of Fever + Not Hospitalized
  select(sym_fev, hospitalized) %>%
  filter(sym_fev == "Yes",
         hospitalized == "No") %>%
  summarise(n=n()) # 7217 - 502 = 6715

symptom_data %>%      # Counting Only Myalgia + Hospitalized
  filter(sym_fev == "No",
         sym_mya == "Yes",
         sym_lts == "No",
         sym_cog == "No",
         sym_hdc == "No",
         sym_sot == "No",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 29

symptom_data %>%      # Counting All instances of Myalgia + Hospitalized
  select(sym_mya, hospitalized) %>%
  filter(sym_mya == "Yes",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 757 - 29 = 728

symptom_data %>%      # Counting Only Myalgia + Not Hospitalized
  filter(sym_fev == "No",
         sym_mya == "Yes",
         sym_lts == "No",
         sym_cog == "No",
         sym_hdc == "No",
         sym_sot == "No",
         hospitalized == "No") %>%
  summarise(n=n()) # 452

symptom_data %>%      # Counting All instances of Myalgia + Not Hospitalized
  select(sym_mya, hospitalized) %>%
  filter(sym_mya == "Yes",
         hospitalized == "No") %>%
  summarise(n=n()) # 10559 - 452 = 10107

symptom_data %>%      # Counting Only Loss of Taste and Smell + Hospitalized
  filter(sym_fev == "No",
         sym_mya == "No",
         sym_lts == "Yes",
         sym_cog == "No",
         sym_hdc == "No",
         sym_sot == "No",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 38

symptom_data %>%      # Counting All instances of Loss of Taste and Smell + Hospitalized
  select(sym_lts, hospitalized) %>%
  filter(sym_lts == "Yes",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 585 - 38 = 547

symptom_data %>%      # Counting Only Loss of Taste and Smell + Not Hospitalized
  filter(sym_fev == "No",
         sym_mya == "No",
         sym_lts == "Yes",
         sym_cog == "No",
         sym_hdc == "No",
         sym_sot == "No",
         hospitalized == "No") %>%
  summarise(n=n()) # 1724

symptom_data %>%      # Counting All instances of Loss of Taste and Smell + Not Hospitalized
  select(sym_lts, hospitalized) %>%
  filter(sym_lts == "Yes",
         hospitalized == "No") %>%
  summarise(n=n()) # 11208 - 1724 = 9484

symptom_data %>%      # Counting Only Coughing + Hospitalized
  filter(sym_fev == "No",
         sym_mya == "No",
         sym_lts == "No",
         sym_cog == "Yes",
         sym_hdc == "No",
         sym_sot == "No",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 132

symptom_data %>%      # Counting All instances of Coughing + Hospitalized
  select(sym_cog, hospitalized) %>%
  filter(sym_cog == "Yes",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 970 - 132 = 838

symptom_data %>%      # Counting Only Coughing + Not Hospitalized
  filter(sym_fev == "No",
         sym_mya == "No",
         sym_lts == "No",
         sym_cog == "Yes",
         sym_hdc == "No",
         sym_sot == "No",
         hospitalized == "No") %>%
  summarise(n=n()) # 956

symptom_data %>%      # Counting All instances of Coughing + Not Hospitalized
  select(sym_cog, hospitalized) %>%
  filter(sym_cog == "Yes",
         hospitalized == "No") %>%
  summarise(n=n()) # 10988 - 956 = 10032

symptom_data %>%      # Counting Only Headaches + Hospitalized
  filter(sym_fev == "No",
         sym_mya == "No",
         sym_lts == "No",
         sym_cog == "No",
         sym_hdc == "Yes",
         sym_sot == "No",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 21

symptom_data %>%      # Counting All instances of Headaches + Hospitalized
  select(sym_hdc, hospitalized) %>%
  filter(sym_hdc == "Yes",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 642 - 21 = 621

symptom_data %>%      # Counting Only Headaches + Not Hospitalized
  filter(sym_fev == "No",
         sym_mya == "No",
         sym_lts == "No",
         sym_cog == "No",
         sym_hdc == "Yes",
         sym_sot == "No",
         hospitalized == "No") %>%
  summarise(n=n()) # 813

symptom_data %>%      # Counting All instances of Headaches + Not Hospitalized
  select(sym_hdc, hospitalized) %>%
  filter(sym_hdc == "Yes",
         hospitalized == "No") %>%
  summarise(n=n()) # 11616 - 813 = 10803

symptom_data %>%      # Counting Only Sore Throats + Hospitalized
  filter(sym_fev == "No",
         sym_mya == "No",
         sym_lts == "No",
         sym_cog == "No",
         sym_hdc == "No",
         sym_sot == "Yes",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 11

symptom_data %>%      # Counting All instances of Sore Throats + Hospitalized
  select(sym_sot, hospitalized) %>%
  filter(sym_sot == "Yes",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 365 - 11 = 354

symptom_data %>%      # Counting Only Sore Throats + Not Hospitalized
  filter(sym_fev == "No",
         sym_mya == "No",
         sym_lts == "No",
         sym_cog == "No",
         sym_hdc == "No",
         sym_sot == "Yes",
         hospitalized == "No") %>%
  summarise(n=n()) # 335

symptom_data %>%      # Counting All instances of Sore Throats + Not Hospitalized
  select(sym_sot, hospitalized) %>%
  filter(sym_sot == "Yes",
         hospitalized == "No") %>%
  summarise(n=n()) # 6444 - 335 = 6109

# ____________________----

# WORKING WITH DATA----

# Creating objects for a tibble

# creating the order of symptoms

symptom <- c("fev", "fev", "mya", "mya", "lts", "lts", "cog", "cog", "hdc", "hdc", "sot", "sot")

# signifying if the value is for the symptom on its own or alongside others

n_pos <- c("solo", "multi", "solo", "multi", "solo", "multi", "solo", "multi", "solo", "multi", "solo", "multi")

# manually inputting the counts for rows with Yes values for hospitalization

hosp_Y <- c(53, 680, 29, 728, 38, 547, 132, 838, 21, 621, 11, 354)

# manually inputting the counts for rows with No values for hospitalizations

hosp_N <- c(502, 6715, 452, 10107, 1724, 9484, 956, 10032, 813, 10803, 335, 6109)

# creating the tibble for data with hospitalizations 

sym_count_Y <- tibble(symptom, n_pos, hosp_Y)

# creating the tibble for data without hospitalizations

sym_count_N <- tibble(symptom, n_pos, hosp_N)

# creating factors based on the value of cases with hospitalizations

sym_count_Y <- sym_count_Y %>%
  mutate(symptom = fct_relevel(symptom, "sot", "hdc", "mya", "lts", "fev", "cog"))

# creating labels for each symptom in accordance with the factors

Y_labs <- c("Sore Throat", "Headache", "Myalgia", "Loss of Taste and Smell", "Fever", "Cough")

# creating factors based on the value of cases without hospitalizations

sym_count_N <- sym_count_N %>%
  mutate(symptom = fct_relevel(symptom, "sot", "mya", "fev", "hdc", "cog", "lts"))

# creating labels for each symptom in accordance with the factors

N_labs <- c("Sore Throat", "Myalgia", "Fever", "Headache", "Cough", "Loss of Taste and Smell")

# ____________________----

# BUILDING THE FIGURE----

# creating the plot with hospitalizations with the ggplot2 function

plot_Y <- ggplot(sym_count_Y, aes(fill = n_pos, y = symptom, x = hosp_Y))+      # defining the variables to be used as parameters
  geom_col(position ='stack', width = 1.0, color = "black", show.legend = FALSE)+     # selecting a stacked bar-plot with borders, removing the legend as it will be shown on the second plot
  scale_fill_manual(values = c("deepskyblue", "darkblue"))+     # adding color
  scale_y_discrete(labels = Y_labs)+      # providing the Y axis labels for the symptoms
  labs(x = "Number of cases that led to Hospitalizations",      # labeling the axis of the plot, providing a title and subtitle 
       y = "Symptom",
       title = "Occurrence of COVID-19 Symptoms leading to hospitalizations",
       subtitle = "Coughing symptoms were present in the majority of hospitalizations")+
  theme_classic()     # setting a plot theme

# creating the plot without hospitalizations with the ggplot2 function

plot_N <- ggplot(sym_count_N, aes(fill = n_pos, y = symptom, x = hosp_N))+      # defining the variables to be used as parameters
  geom_col(position ='stack', width = 1.0, colour = "black")+     # selecting a stacked bar-plot with borders
  scale_fill_manual(labels = c("With additional Symptoms", "Only specific Symptom"), values = c("deepskyblue", "darkblue"))+      # labeling the legend, adding color
  scale_y_discrete(labels = N_labs)+      # providing the Y axis labels for the symptoms
  labs(x = "Number of cases that did not lead to Hospitalizations",      # labeling the axis of the plot and titling the legend, title omitted as it is already included in the first plot
       y = "",
       fill = "Included Symptoms")+
  theme_classic()     # setting a plot theme

# patching together the plots with the patchwork function

plot_F <- (plot_Y+plot_N)

# saving the plot

ggsave("figures/madiyar_symptom_barplot.png",
       plot = plot_F,
       dpi = 300,
       width = 21,
       height = 9)
  

