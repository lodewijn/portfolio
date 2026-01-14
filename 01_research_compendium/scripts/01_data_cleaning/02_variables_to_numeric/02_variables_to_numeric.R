# Convert_time_variables_to_numeric_all_waves

# Load Required Packages

library(tidyverse)
library(here)


# Load Cleaned Data From All Waves
w1 <- read.csv(here("data/cleandata/w1_clean.csv"))
w2 <- read.csv(here("data/cleandata/w2_clean.csv"))
w4 <- read.csv(here("data/cleandata/w4_clean.csv"))
w5 <- read.csv(here("data/cleandata/w5_clean.csv"))
w6 <- read.csv(here("data/cleandata/w6_clean.csv"))
w7 <- read.csv(here("data/cleandata/w7_clean.csv"))


# Convert all age and year variables to numeric
# Put all data frames in a list to mutate them all at once
waves <- list(w1 = w1, w2 = w2, w4 = w4, w5 = w5, w6 = w6, w7 = w7)

# Convert the variables to numeric
waves <- lapply(waves, \(df) {
  df |>
    mutate(dn003_ = as.character(as.numeric(dn003_)), # year of birth
           ph009_1 = as.numeric(ph009_1), # age heart attack
           ph009_4 = as.numeric(ph009_4), # age stroke
           dn015_ = as.character(as.numeric(dn015_)), # year marriage
           dn018_ = as.character(as.numeric(dn018_)), # year divorce
           dn019_ = as.character(as.numeric(dn019_))) # year widowed
})

# Put them back into original objects
w1 <- waves$w1
w2 <- waves$w2
w4 <- waves$w4
w5 <- waves$w5
w6 <- waves$w6
w7 <- waves$w7

# Update the csv files containing the clean data from all waves
write.csv(w1, here("data/cleandata/w1_clean.csv"))
write.csv(w2, here("data/cleandata/w2_clean.csv"))
write.csv(w4, here("data/cleandata/w4_clean.csv"))
write.csv(w5, here("data/cleandata/w5_clean.csv"))
write.csv(w6, here("data/cleandata/w6_clean.csv"))
write.csv(w7, here("data/cleandata/w7_clean.csv"))


