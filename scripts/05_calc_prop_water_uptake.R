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

# compiled chemcorrect output
cc1 <- read_csv("data-processed/hw_combined_cc_output.csv")

# compiled uncorrected data (for determining which samples are 'good')
raw1 <- read_csv("data-processed/hw_combined_picarro_output.csv") %>% 
  janitor::clean_names()

# parsing -----------------------------------------------------------------



cc2 <- cc1 %>% 
  # 2 sampling events
  mutate(sample_event = ifelse(lubridate::month(date_inject) == 5,
                               1 , 2),
         run = str_replace(run, "_\\d$", ""))



# categorizing good/bad samples ------------------------------------------

# calculate slope
calc_slope <- function(y) {
  
  if(sum(!is.na(y)) < 2) {
    return(NA_real_)
  }
  
  x <- 1:length(y)
  
  mod <- lm(y ~ x)
  mod$coefficients[2]
}



# creating a lookup vector, to convert line number into
# the unique sampling occasion. I.e. this is the nth vial, to be sampled
n_lines <- 694
vial_inj_lookup <- vector(mode = "numeric", length = n_lines)
# first vial is measured 10 times
vial_inj_lookup[1:10] <- 1
# remaining samples are measured 6 times
vial_inj_lookup[11:n_lines] <- 0:(n_lines-11) %/% 6 + 2

names(vial_inj_lookup) <- 1:n_lines

raw_means1 <- raw1 %>% 
  # vial number (i.e. consecutive numbering that vials were actually measured,
  # so have an identifier to tell what the 'previous' vial was, to measure
  # memory of hot samples)
  mutate(vial_nr = vial_inj_lookup[line]) %>% 
  # only keep last 3 samples
  filter(inj_nr > 3) %>% 
  group_by(run, port, vial_nr, identifier_1, identifier_2) %>% 
  summarize(slope = calc_slope(y = d_d_h_mean),
            d_d_h_sd = sd(d_d_h_mean, na.rm = TRUE),
            d_d_h_mean = mean(d_d_h_mean, na.rm = TRUE)) %>% 
  arrange(run, vial_nr) %>% 
  group_by(run) %>% 
  # difference between mean of this sample and the previous sample
  mutate(d_h_diff = c(NA, diff(d_d_h_mean)))

# samples are bad if the previous sample was very hot and there
# is a lot of difference between injections
raw_means2 <- raw_means1 %>%
  mutate(is_bad = ifelse(d_d_h_sd > 10 & slope < 0 & d_h_diff < -200,
                         TRUE, FALSE))

sum(raw_means2$is_bad, na.rm = TRUE)


# * removing bad values ---------------------------------------------------

cc3 <- raw_means2 %>% 
  filter(!(identifier_2 %in% c("Dummy", "Standard", "Tap"))) %>% 
  select(run, identifier_1, is_bad) %>% 
  mutate(identifier_1 = as.numeric(identifier_1)) %>% 
  right_join(cc2, by = c("run", "identifier_1" = "vial_num")) %>% 
  # excluding samples affected by drift from previous hot sample
  filter(!is_bad)
  



# checking control data ---------------------------------------------------

# data was collected prior to event 1 to make sure no tracer was detected

control <- cc3 %>% 
  filter(date == "2020-07-12")

control %>%
  group_by(depth) %>% 
  summarize(cal_2h_mean = mean(cal_2h_mean)) 

hist(control$raw_2h_mean) # some high (contaminated?) values
hist(control$raw_18o_mean)

cc4 <- cc3 %>% 
  filter(date != "2020-07-12")

