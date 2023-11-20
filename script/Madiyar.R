# Script for 5023Y Group Project----
## Script author: Madiyar Seidaly
### Topic:

# ____________________----

# IMPORTING CLEAN DATA----

source("script/cleaning_data.R")

# ____________________----

library(tidyverse)

temp_covid %>%
  summarise(n=n(),
            num_country = n_distinct(iso_code)

            