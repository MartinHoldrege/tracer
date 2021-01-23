# Martin Holdrege

# script started 1/23/21

# Purpose is to print out sheets of papers so that vials
# can be efficiently sorted

# dependencies-------------------------------------------------------------

library(tidyverse)
library(googlesheets4)


# load data ---------------------------------------------------------------

url <- "https://docs.google.com/spreadsheets/d/1ydpXqb52yzdgneyCcOjy8RBcGhiJJZMwipSG1JvrHgw/edit#gid=1385148614"

df1 <- read_sheet(url, sheet = "vial_num",
                  col_types = "cdDddd", na = c("", "NA"))


# check -------------------------------------------------------------------

# Continue working here:
# the problem is that ther are duplicates in the data
# sheet, but they must be data entry mistakes (ive checked a few and they
# were correctly written down in notebook). This indicates I made
# a mistake while entering the data.

df1$vial_num %>% sort()
sum(duplicated(df1$vial_num))
x <- df1 %>% 
  filter(duplicated(vial_num)) %>% 
  arrange(vial_num) %>% 
  pull(vial_num)

x <- x[!is.na(x)]
x[x%in% as.numeric(df1$sample_num)]
