# Taxi grupo

library(arrow)
library(ggcorrplot)
library(dplyr)
library(tidyr)
library(hms)
library(scales)

yellow <- open_dataset("C:/Users/macha/OneDrive/Desktop/projetos/taxi/data") %>%
  collect()

#########
yellow <- yellow %>%
  separate(tpep_pickup_datetime,
           into = c("pickup_date", "pickup_time"),
           sep = " ") %>%
  separate(tpep_dropoff_datetime,
           into = c("dropoff_date", "dropoff_time"),
           sep = " ")

# Set a coluna de data como data format
yellow$pickup_date <- as.Date(yellow$pickup_date)
yellow$dropoff_date <- as.Date(yellow$dropoff_date)




