# Martin Holdrege

# Script started May 26, 2021

# Purpose of this code is to take the data compiled in
# '04_compile_ChemCorrect_output.R' and calculate proportional water 
# uptake

# Next steps--filter out cool samples with a negative slope (i.e. those)
# that shouldn't be hot, but have memory. 

# dependencies ------------------------------------------------------------

library(tidyverse)

# read in data ------------------------------------------------------------

raw1 <- read_csv("data-processed/hw_combined_cc_output.csv")

# parsing -----------------------------------------------------------------

raw2 <- raw1 %>% 
  # 2 sampling events
  mutate(sample_event = ifelse(lubridate::month(date_inject) == 5,
                               1 , 2))

raw3 <- raw2 %>% 
  filter(date != "2020-07-12")

# checking control data ---------------------------------------------------

# data was collected prior to event 1 to make sure no tracer was detected

control <- raw2 %>% 
  filter(date == "2020-07-12")

control %>%
  group_by(depth) %>% 
  summarize(cal_2h_mean = mean(cal_2h_mean)) 

hist(control$raw_2h_mean) # some high (contaminated?) values


