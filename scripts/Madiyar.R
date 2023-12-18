# Script for 5023Y Group Project----
## Script author: Madiyar Seidaly
### Topic: investigating the different symptoms and rate of hospitalizations 

# ____________________----

# IMPORTING CLEAN DATA----

source("scripts/cleaning_data.R")

library(patchwork)

library(ggplot2)

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

# Counting each instance

symptom_data %>%
  filter(sym_fev == "Yes",
         sym_mya == "No",
         sym_lts == "No",
         sym_cog == "No",
         sym_hdc == "No",
         sym_sot == "No",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 53

symptom_data %>%
  select(sym_fev, hospitalized) %>%
  filter(sym_fev == "Yes",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 733 - 53 = 680

symptom_data %>%
  filter(sym_fev == "Yes",
         sym_mya == "No",
         sym_lts == "No",
         sym_cog == "No",
         sym_hdc == "No",
         sym_sot == "No",
         hospitalized == "No") %>%
  summarise(n=n()) # 502

symptom_data %>%
  select(sym_fev, hospitalized) %>%
  filter(sym_fev == "Yes",
         hospitalized == "No") %>%
  summarise(n=n()) # 7217 - 502 = 6715

symptom_data %>%
  filter(sym_fev == "No",
         sym_mya == "Yes",
         sym_lts == "No",
         sym_cog == "No",
         sym_hdc == "No",
         sym_sot == "No",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 29

symptom_data %>%
  select(sym_mya, hospitalized) %>%
  filter(sym_mya == "Yes",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 757 - 29 = 728

symptom_data %>%
  filter(sym_fev == "No",
         sym_mya == "Yes",
         sym_lts == "No",
         sym_cog == "No",
         sym_hdc == "No",
         sym_sot == "No",
         hospitalized == "No") %>%
  summarise(n=n()) # 452

symptom_data %>%
  select(sym_mya, hospitalized) %>%
  filter(sym_mya == "Yes",
         hospitalized == "No") %>%
  summarise(n=n()) # 10559 - 452 = 10107

symptom_data %>%
  filter(sym_fev == "No",
         sym_mya == "No",
         sym_lts == "Yes",
         sym_cog == "No",
         sym_hdc == "No",
         sym_sot == "No",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 38

symptom_data %>%
  select(sym_lts, hospitalized) %>%
  filter(sym_lts == "Yes",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 585 - 38 = 547

symptom_data %>%
  filter(sym_fev == "No",
         sym_mya == "No",
         sym_lts == "Yes",
         sym_cog == "No",
         sym_hdc == "No",
         sym_sot == "No",
         hospitalized == "No") %>%
  summarise(n=n()) # 1724

symptom_data %>%
  select(sym_lts, hospitalized) %>%
  filter(sym_lts == "Yes",
         hospitalized == "No") %>%
  summarise(n=n()) # 11208 - 1724 = 9484

symptom_data %>%
  filter(sym_fev == "No",
         sym_mya == "No",
         sym_lts == "No",
         sym_cog == "Yes",
         sym_hdc == "No",
         sym_sot == "No",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 132

symptom_data %>%
  select(sym_cog, hospitalized) %>%
  filter(sym_cog == "Yes",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 970 - 132 = 838

symptom_data %>%
  filter(sym_fev == "No",
         sym_mya == "No",
         sym_lts == "No",
         sym_cog == "Yes",
         sym_hdc == "No",
         sym_sot == "No",
         hospitalized == "No") %>%
  summarise(n=n()) # 956

symptom_data %>%
  select(sym_cog, hospitalized) %>%
  filter(sym_cog == "Yes",
         hospitalized == "No") %>%
  summarise(n=n()) # 10988 - 956 = 10032

symptom_data %>%
  filter(sym_fev == "No",
         sym_mya == "No",
         sym_lts == "No",
         sym_cog == "No",
         sym_hdc == "Yes",
         sym_sot == "No",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 21

symptom_data %>%
  select(sym_hdc, hospitalized) %>%
  filter(sym_hdc == "Yes",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 642 - 21 = 621

symptom_data %>%
  filter(sym_fev == "No",
         sym_mya == "No",
         sym_lts == "No",
         sym_cog == "No",
         sym_hdc == "Yes",
         sym_sot == "No",
         hospitalized == "No") %>%
  summarise(n=n()) # 813

symptom_data %>%
  select(sym_hdc, hospitalized) %>%
  filter(sym_hdc == "Yes",
         hospitalized == "No") %>%
  summarise(n=n()) # 11616 - 813 = 10803

symptom_data %>%
  filter(sym_fev == "No",
         sym_mya == "No",
         sym_lts == "No",
         sym_cog == "No",
         sym_hdc == "No",
         sym_sot == "Yes",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 11

symptom_data %>%
  select(sym_sot, hospitalized) %>%
  filter(sym_sot == "Yes",
         hospitalized == "Yes") %>%
  summarise(n=n()) # 365 - 11 = 354

symptom_data %>%
  filter(sym_fev == "No",
         sym_mya == "No",
         sym_lts == "No",
         sym_cog == "No",
         sym_hdc == "No",
         sym_sot == "Yes",
         hospitalized == "No") %>%
  summarise(n=n()) # 335

symptom_data %>%
  select(sym_sot, hospitalized) %>%
  filter(sym_sot == "Yes",
         hospitalized == "No") %>%
  summarise(n=n()) # 6444 - 335 = 6109

# ____________________----

# WORKING WITH DATA----

# Creating objects for a tibble

symptom <- c("fev", "fev", "mya", "mya", "lts", "lts", "cog", "cog", "hdc", "hdc", "sot", "sot")

n_pos <- c("solo", "multi", "solo", "multi", "solo", "multi", "solo", "multi", "solo", "multi", "solo", "multi")

hosp_Y <- c(53, 680, 29, 728, 38, 547, 132, 838, 21, 621, 11, 354)

hosp_N <- c(502, 6715, 452, 10107, 1724, 9484, 956, 10032, 813, 10803, 335, 6109)

sum_pos <- c(53, 680, 502, 6715, 29, 728, 452, 10107, 38, 547, 1724, 9484, 132, 838, 956, 10032, 21, 621, 813, 10803, 11, 354, 335, 6109) 

sym_count_Y <- tibble(symptom, n_pos, hosp_Y)

sym_count_N <- tibble(symptom, n_pos, hosp_N)

sym_count_Y <- sym_count_Y %>%
  mutate(symptom = fct_relevel(symptom, "sot", "hdc", "mya", "lts", "fev", "cog"))

Y_labs <- c("Sore Throat", "Headache", "Myalgia", "Loss of Taste and Smell", "Fever", "Cough")

sym_count_N <- sym_count_N %>%
  mutate(symptom = fct_relevel(symptom, "sot", "mya", "fev", "hdc", "cog", "lts"))

N_labs <- c("Sore Throat", "Myalgia", "Fever", "Headache", "Cough", "Loss of Taste and Smell")

# ____________________----

# BUILDING THE FIGURE----

plot_Y <- ggplot(sym_count_Y, aes(fill = n_pos, y = symptom, x = hosp_Y))+
  geom_col(position ='stack', width = 1.0, color = "black", show.legend = FALSE)+
  scale_fill_manual(values = c("deepskyblue", "darkblue"))+
  scale_y_discrete(labels = Y_labs)+
  labs(x = "Number of cases that led to Hospitalizations",
       y = "Symptom",
       title = "Occurrence of COVID-19 Symptoms leading to hospitalizations",
       subtitle = "Coughing symptoms were present in the majority of hospitalizations")+
  theme_classic()

plot_N <- ggplot(sym_count_N, aes(fill = n_pos, y = symptom, x = hosp_N))+
  geom_col(position ='stack', width = 1.0, colour = "black")+
  scale_fill_manual(labels = c("With additional Symptoms", "Only specific Symptom"), values = c("deepskyblue", "darkblue"))+
  scale_y_discrete(labels = N_labs)+
  labs(x = "Number of cases that did not lead to Hospitalizations",
       y = "",
       fill = "Included Symptoms")+
  theme_classic()

plot_F <- (plot_Y+plot_N)

ggsave("figures/madiyar_symptom_barplot.png",
       plot = plot_F,
       dpi = 300,
       width = 21,
       height = 9)
  

