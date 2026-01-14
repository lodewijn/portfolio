# Data Preprocessing Wave 2

## Load Required Pacakges
library(foreign) # required to load SPSS data
library(tidyverse) # required for data wrangling
# install.packages("here")
library(here) # locate working directory

## Wave 2 - Load Data From SPSS Files
# Physical Health
w2_ph_raw <- read.spss(here("data/rawdata/wave2/sharew2_rel9-0-0_ph.sav"), 
                       to.data.frame=TRUE)

# Demographics
w2_dn_raw <- read.spss(here("data/rawdata/wave2/sharew2_rel9-0-0_dn.sav"), 
                       to.data.frame=TRUE)

# Behavioural Risks
w2_br_raw <- read.spss(here("data/rawdata/wave2/sharew2_rel9-0-0_br.sav"), 
                       to.data.frame=TRUE)

# General Health
w2_health_raw <- read.spss(here("data/rawdata/wave2/sharew2_rel9-0-0_gv_health.sav"), 
                           to.data.frame=TRUE)

# End-of-Life
w2_xt_raw <- read.spss(here("data/rawdata/wave2/sharew2_rel9-0-0_xt.sav"), 
                       to.data.frame=TRUE)


## Select only the relevant variables for this study
# Physical Health
w2_ph <- w2_ph_raw |>
  select(mergeid,            # respondent ID
         ph006d1,            # heart attack ever
         ph006d4,            # stroke ever
         ph009_1,            # age heart attack
         ph009_4,            # age stroke
         ph067_1,            # heart attack since last interview
         ph067_2,            # stroke recent since last interview
         ph068_1,            # heart attack before last interview
         ph068_2)  |>        # stroke before last interview
  mutate(ph072_1 = ph068_1,  # rename variable to match w4-w7
         ph072_2 = ph068_2)  # rename variable to match w4-w7

# Demographics
w2_dn <- w2_dn_raw |>
  select(mergeid,            # respondent ID
         dn014_,             # marital status
         dn002_,             # birth month
         dn003_,             # birth year
         dn010_,             # education
         dn042_,             # sex
         country,            # country 
         dn015_,             # year of cohabiting marriage
         dn018_,             # since when divorced
         dn019_              # since when widowed
  )

# Behavioural risks
w2_br <- w2_br_raw |>
  select(mergeid,            # respondent ID
         br001_,             # ever smoked daily
         br002_,             # currently smoking
         br003_,             # years smoked
         #br004_,            # age stopped
         br005d1,            # smoke type 1
         br005d2,            # smoke type 2
         br005d3,            # smoke type 3
         br006_,             # average cigarettes per day
         br007_,             # average pipes per day
         br008_)             # average cigars per day

# General health
w2_health <- w2_health_raw |>
  select(mergeid,            # respondent ID
         phactiv)            # physical activity

# End-of-life
w2_xt <- w2_xt_raw |>
  select(mergeid,            # respondent ID
         xt011_) |>          # cause of death
  filter(xt011_ == "A stroke" |
           xt011_ == "A heart attack") # select only the rows in which stroke / heart attack are the cause of death


# Merge all datasets of Wave 2 together using mergeid
w2 <- w2_ph %>%
  left_join(w2_dn, by = "mergeid") |>
  left_join(w2_br, by = "mergeid") |>
  left_join(w2_health, by = "mergeid") |>
  left_join(w2_xt, by = "mergeid")

# Write csv file containing the selected data from Wave 2
write.csv(w2, here("data/cleandata/w2_clean.csv"))
