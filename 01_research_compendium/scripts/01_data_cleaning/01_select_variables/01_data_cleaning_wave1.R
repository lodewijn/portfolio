# Data Preprocessing Wave 1

## Load Required Pacakges

library(foreign) # required to load SPSS data
library(tidyverse) # required for data wrangling
# install.packages("here")
library(here) # locate working directory

## Wave 1 - Load Data From SPSS Files
# Physical Health
w1_ph_raw <- read.spss(here("data/rawdata/wave1/sharew1_rel9-0-0_ph.sav"), 
                       to.data.frame=TRUE)

# Demographics
w1_dn_raw <- read.spss(here("data/rawdata/wave1/sharew1_rel9-0-0_dn.sav"), 
                       to.data.frame=TRUE)

# Behavioural Risks
w1_br_raw <- read.spss(here("data/rawdata/wave1/sharew1_rel9-0-0_br.sav"), 
                       to.data.frame=TRUE)

# General Health
w1_health_raw <- read.spss(here("data/rawdata/wave1/sharew1_rel9-0-0_gv_health.sav"), 
                           to.data.frame=TRUE)


## Select only the relevant variables for this study

# Physical Health
w1_ph <- w1_ph_raw |>
  select(mergeid,          # respondent ID
         ph006d1,          # heart attack ever
         ph006d4,          # stroke ever
         ph009_1,          # age heart attack
         ph009_4)          # age stroke

# Demographics
w1_dn <- w1_dn_raw |>
  select(mergeid,          # respondent ID
         dn014_,           # marital status
         dn002_,           # birth month
         dn003_,           # birth year
         dn010_,           # education
         dn042_,           # sex
         country,          # country 
         dn015_,           # year of cohabiting marriage
         dn018_,           # since when divorced
         dn019_            # since when widowed
  )

# Behavioural Risks
w1_br <- w1_br_raw |>
  select(mergeid,          # respondent ID
         br001_,           # ever smoked daily
         br002_,           # currently smoking
         br003_,           # years smoked
         #br004_,           # age stopped
         br005d1,          # smoke type 1
         br005d2,          # smoke type 2
         br005d3,          # smoke type 3
         br006_,           # average cigarettes per day
         br007_,           # average pipes per day
         br008_)           # average cigars per day

# General Health
w1_health <- w1_health_raw |>
  select(mergeid,          # respondent ID
         phactiv)          # physical activity

# Merge all datasets of Wave 1 together using mergeid
w1 <- w1_ph %>%
  left_join(w1_dn, by = "mergeid") %>%
  left_join(w1_br, by = "mergeid") %>%
  left_join(w1_health, by = "mergeid")

# Write csv file containing the selected data from Wave 1
write.csv(w1, here("data/cleandata/w1_clean.csv"))


