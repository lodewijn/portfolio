# Data Preprocessing Wave 7
  
## Load Required Pacakges

library(foreign) # required to load SPSS data
library(tidyverse) # required for data wrangling
# install.packages("here")
library(here) # locate working directory

## Wave 7 - Load Data From SPSS Files
# Physical Health
w7_ph_raw <- read.spss(here("data/rawdata/wave7/sharew7_rel9-0-0_ph.sav"), to.data.frame=TRUE)

# Demographics
w7_dn_raw <- read.spss(here("data/rawdata/wave7/sharew7_rel9-0-0_dn.sav"), to.data.frame=TRUE)

# Behavioural Risks
w7_br_raw <- read.spss(here("data/rawdata/wave7/sharew7_rel9-0-0_br.sav"), to.data.frame=TRUE)

# General Health
w7_health_raw <- read.spss(here("data/rawdata/wave7/sharew7_rel9-0-0_gv_health.sav"), to.data.frame=TRUE)

# End-of-Life
w7_xt_raw <- read.spss(here("data/rawdata/wave7/sharew7_rel9-0-0_xt.sav"), to.data.frame=TRUE)


## Select only the relevant variables for this study

# Physical Health
w7_ph <- w7_ph_raw |>
  select(mergeid,          # respondent ID
         ph006d1,          # heart attack ever
         ph006d4,          # stroke ever
         ph009_1,          # age heart attack
         ph009_4,          # age stroke
         #ph067_1,         # doesn't exist
         #ph067_2,         # doesn't exist
         ph071_1,          # how many heart attacks since last
         ph071_2,          # how many strokes since last interview
         ph072_1,          # heart attack since last interview
         ph072_2)          # stroke since last interview

# Demographics
w7_dn <- w7_dn_raw |>
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

# Behavioural risks
w7_br <- w7_br_raw |>
  select(mergeid,          # respondent ID
         br001_,           # ever smoked daily
         br002_,           # currently smoking
         br003_            # years smoked
         #br004_,          # doesn't exist
         #br005d1,         # doesn't exist
         #br005d2,         # doesn't exist
         #br005d3,         # doesn't exist
         #br006_,          # doesn't exist
         #br007_,          # doesn't exist
         #br008_           # doesn't exist
  )          

# General health
w7_health <- w7_health_raw |>
  select(mergeid,          # respondent ID
         phactiv)          # physical activity

# End-of-life
w7_xt <- w7_xt_raw |>
  select(mergeid,          # respondent ID
         xt011_) |>        # cause of death
  filter(xt011_ == "A stroke" |
           xt011_ == "A heart attack") # select only the rows in which stroke / heart attack are the cause of death

# Merge all datasets of Wave 7 together using mergeid
w7 <- w7_ph %>%
  left_join(w7_dn, by = "mergeid") |>
  left_join(w7_br, by = "mergeid") |>
  left_join(w7_health, by = "mergeid") |>
  left_join(w7_xt, by = "mergeid")

# Write csv file containing the selected data from Wave 2
write.csv(w7, here("data/cleandata/w7_clean.csv"))

