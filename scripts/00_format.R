

# libraries --------------------------------------------------------------------
library(tidyverse)


# data -------------------------------------------------------------------------
hgns <- read.csv("data/hgns.csv") %>% slice(-1)
key <- read.csv("data/hgns.csv") %>% slice(1)


# 1. What variables predict if the surgeon used dissection as their first step?
# if they performed HGNS surgeries in residency (Q3)
# how many years they have been in practice (Q4)
# how many HGNS surgeries they performed post-training (Q5)
# how frequently they are performing HGNS surgeries (Q6)
# additional case time added (Q15)

# 2. Are there variables that predict the additional case time added?
# if they performed HGNS surgeries in residency (Q3)
# how many years they have been in practice (Q4)
# how many HGNS surgeries they performed post-training (Q5)
# how frequently they are performing HGNS surgeries (Q6)
# 1st step (Q11)
# being unable to resolve tongue retraction (Q14)


# format -----------------------------------------------------------------------
hgns <- hgns %>%
  
  mutate(
    practice = Q2
    ) %>%
  
  mutate(
    residency = Q3
  ) %>%
  
  mutate(
    years = case_when(
      Q4 == "< 1" ~ 1,
      Q4 == "1-5" ~ 2,
      Q4 == "6-10" ~ 3,
      Q4 == "10-15" ~ 4,
      Q4 == ">15" ~ 5,
      TRUE ~ NA
    )
  ) %>%
  
  mutate(
    posttraining = case_when(
      Q5 == "< 20" ~ 1,
      Q5 == "20-50" ~ 2,
      Q5 == "50-100" ~ 3,
      Q5 == "100-200" ~ 4,
      Q5 == "> 200" ~ 5,
      TRUE ~ NA
    )
  ) %>%
  
  mutate(
    frequency = case_when(
      Q6 == "< 1 per month" ~ 1,
      Q6 == "Once a month" ~ 2,
      Q6 == "Once every other week" ~ 3,
      Q6 == "1-2 per week" ~ 4,
      Q6 == "> 2 per week" ~ 5,
      TRUE ~ NA
    )
  ) %>%
  
  mutate(
    dissection = ifelse(
      Q11 == "Dissecting distal aspect to exclude one or more hypoglossal branches",
      1,
      0
    )
  ) %>%
  
  mutate(
    time = case_when(
      Q15 == "0-15 min" ~ 0,
      Q15 == "15-30 min" ~ 0,
      Q15 == "30-45 min" ~ 1,
      Q15 == "45-60 min" ~ 1,
      TRUE ~ NA
    )
  ) %>%
  
  mutate(
    resolved = ifelse(
      Q14 == "N/A: I have always been able to resolve any issues with retraction prior to closure",
      "Yes",
      "No"
    )
  ) %>%
  
  mutate(
    first = case_when(
      Q11 == "Dissecting distal aspect to exclude one or more hypoglossal branches" ~ "Dissection",
      Q11 == "Open window around cuff without removing" ~ "Open_window",
      Q11 == "Remove and replace the cuff with stimulation" ~ "Remove_replace_stimulation",
      Q11 == "Removing and replacing the cuff back in the same location" ~ "Remove_replace_location",
      Q11 == "Stimulating superior and/or deep aspects of the nerve" ~ "Stimulating_superior",
      TRUE ~ NA
    )
  )
