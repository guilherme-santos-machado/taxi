# Taxi grupo

library(arrow)
library(dplyr)

yellow <- open_dataset("C:/Users/macha/OneDrive/Desktop/projetos/taxi/data") %>%
  collect()
