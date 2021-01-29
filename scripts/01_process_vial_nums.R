# Martin Holdrege

# script started 1/23/21

# purpose is to process vial_num sheet

# and print out sheets of papers so that vials
# can be efficiently sorted

# dependencies-------------------------------------------------------------

library(tidyverse)
library(googlesheets4)


# load data ---------------------------------------------------------------

url <- "https://docs.google.com/spreadsheets/d/1ydpXqb52yzdgneyCcOjy8RBcGhiJJZMwipSG1JvrHgw/edit#gid=1385148614"

df1 <- read_sheet(url, sheet = "vial_num",
                  col_types = "cdDddd", na = c("", "NA"))


# problem vial nums -------------------------------------------------------

# vial numbers that are duplicated (should all be unique)
dup_nums <- df1$vial_num[duplicated(df1$vial_num)] %>% 
  unique() %>% 
  .[!is.na(.)] %>% 
  sort()

prob_rows <- df1 %>% 
  filter(vial_num %in% dup_nums) %>% 
  arrange(vial_num) %>% 
  mutate(adjacent_in_notebook = NA,
         entered_correctly = NA)

write_csv(prob_rows, "data-processed/vial_nums2check_v2.csv")

# checking if any values that are wrong are just simple switch problems
# (doesn't look like)
prob_nums <- prob_rows$vial_num %>% # duplicated vial nums
  unique()

# these ones might have sample vial num switched?
# vials numbers that are not found in sample numbers
switched_vial <- prob_nums[!prob_nums %in% as.numeric(df1$sample_num)]

x <- prob_rows %>% # sample nums corresponding to switched vials
  filter(vial_num %in% switched_vial) %>% 
  pull(sample_num)

# none of the sample nums could have been switched b/ they are
# also found in vial num
switched_sample <- x[!x%in% df1$vial_num]

# create better vial_num sheet --------------------------------------------

df2 <- df1 %>% 
  mutate(suspect_vial_num = vial_num %in% prob_nums)

write_csv(df2, "data-processed/vial_num.csv")


# create sorting sheets ---------------------------------------------------

# sheets for physically sorting out samples in order

sort1 <- df2 %>% 
  select(vial_num, is_duplicate, suspect_vial_num) %>% 
  arrange(vial_num) %>% 
  mutate(cut100 = cut(vial_num, 
                      breaks = seq(from = 0, to =  700, by = 100),
                      right = FALSE),
         cut25 = cut(vial_num, 
                     breaks = seq(from = 0, to =  700, by = 20),
                     right = FALSE),
         vial_pad = as.character(vial_num),
         vial_pad = str_pad(vial_pad, 
                            width = 6,
                            side = "right"),
         
  ) %>% 
  filter(!is.na(vial_num))

# fun for printing in order the vial numbers 
cut_fun <- function(df) {
  cut100u <- unique(df$cut100) %>% 
    sort()
  for(cut in cut100u) {
    print(cut)
    sort_sub <- df %>% 
      filter(cut100 == cut)
    cut25_sub <- unique(sort_sub$cut25) %>% 
      sort()
    for(c25 in cut25_sub) {
      vals <- sort_sub %>% 
        filter(cut25==c25) %>% 
        pull(vial_pad)
      print(paste(vals, collapse = "", sep = ""))
      print("")
      print("")
    }
    print("------------------------------")
    print("")
    
  }
}

# * good samples ----------------------------------------------------------

sort_good <- sort1 %>% 
  filter(!suspect_vial_num & (is_duplicate == 0 | is.na(is_duplicate)))

sink("sheets-for-sorting/vial_labels_for_sort_good.txt")

cut_fun(sort_good)

sink()

# * duplicate samples -----------------------------------------------------

sort_dup <- sort1 %>% 
  filter(!suspect_vial_num & is_duplicate == 1)

sink("sheets-for-sorting/vial_labels_for_sort_dup.txt")

cat("duplicated vials\n")
cut_fun(sort_dup)

sink()

# * mislabled samples -----------------------------------------------------

sort_mis <- sort1 %>% 
  filter(suspect_vial_num)

sink("sheets-for-sorting/vial_labels_for_sort_mislabled.txt")

cat("mislabled vials\n")
cut_fun(sort_mis)

sink()
