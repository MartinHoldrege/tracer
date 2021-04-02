# Martin Holdrege

# script started 3/16/21

# this script is for creating sample description files for use with Picarro



# which set (run) to focus on
set_num <- 4


# packages ----------------------------------------------------------------

library(tidyverse)
library(googlesheets4)

# load data ---------------------------------------------------------------

url <- "https://docs.google.com/spreadsheets/d/1ydpXqb52yzdgneyCcOjy8RBcGhiJJZMwipSG1JvrHgw/edit#gid=1385148614"

# entered data for run
tray1 <- read_sheet(url, sheet = "picarro_runs",
                  col_types = "dddcddc", na = c("", "NA"))

# sample info
sample1 <- read_sheet(url, sheet = "sample_description",
                      col_types = "Ddddcdcdcc", na = c("", "NA"))

# vial number/sample number key
vial_num1 <- read_sheet(url, sheet = "vial_num",
                        col_types = "cdDdddddd", 
                        na = c("", "NA"))


# data integrity checks ---------------------------------------------------
sample1 <- sample1 %>% 
  # if rows entirely duplicated that I must have just entered the data
  # twice in the notebook (i.e. not a problem sample)
  filter(!duplicated(.)) 


sum(duplicated(sample1$sample_num))
  # duplicated samples numbers (should all be unique) (excepting completely
  # duplicated rows which were filtered out abov)
dup_samples <- sample1$sample_num[duplicated(sample1$sample_num)]

dup_vials <- vial_num1$vial_num[duplicated(vial_num1$vial_num)]

# make info file ----------------------------------------------------------

# sample info
sample2 <- sample1 %>% 
  mutate(identifier_2 = paste0("plot", plot, "_", depth, "cm_", species),
         # labeling problem vials as such
         identifier_2 = ifelse(sample_num %in% dup_samples,
                              "not unique sample",
                              identifier_2)) %>% 
  select(sample_num, identifier_2) %>% 
  # removing duplicated rows
  filter(!duplicated(.))

# sample info + vial num
info1 <- vial_num1 %>% 
  # some samples were lost/damaged so no vial_num present
  filter(!is.na(vial_num)) %>% 
  # eg 231A/231B refer to the same original sample,
  # but too much sample so was transfered to to tubes before extraction
  mutate(sample_num = str_replace(sample_num, "[A-z]", ""),
         sample_num = as.numeric(sample_num)) %>% 
  select(sample_num, vial_num) %>% 
  left_join(sample2, by = "sample_num") %>% 
  # data entry/recording mistake for these duplicated vials
  # (how labels were created physical duplicates don't exist)
  mutate(identifier_2 = ifelse(vial_num %in% dup_vials,
                              "not unique vial",
                              identifier_2),
         # problem here is sample_num is present in vial_num sheet but not 
         # sample description sheet
         identifier_2 = ifelse(is.na(identifier_2),
                               "missing sample info",
                               identifier_2))


# identifiers
id2 <- info1 %>% 
  rename(identifier_1 = vial_num) %>% 
  select(identifier_1, identifier_2) %>% 
  mutate(identifier_1 = as.character(identifier_1))

# prep tray file ----------------------------------------------------------

# just the set of interest
tray2 <- tray1 %>% 
  filter(set == set_num)

cols1 <- tray2 %>% 
  select(tray...2, vial...3, identifier...4) %>% 
  rename(tray = tray...2, vial = vial...3, identifier_1 = identifier...4)

cols2 <- tray2 %>% 
  select(tray...5, vial...6, identifier...7) %>% 
  rename(tray = tray...5, vial = vial...6, identifier_1 = identifier...7) %>% 
  drop_na(vial)

tray3 <- bind_rows(cols1, cols2)

# join id2  ---------------------------------------------------------------

# identifier 2 juast says standard
stand_vec <- c("Tap" = "Tap", "Dummy" = "Standard", "Low" = "Standard",
               "Medium" = "Standard", "High" = "Standard")

tray4 <- tray3 %>% 
  left_join(id2, by = "identifier_1") %>% 
  mutate(identifier_2 = ifelse(is.na(identifier_2),
                               stand_vec[identifier_1],
                               identifier_2)
  )



# data checks -------------------------------------------------------------

# checking for duplicates in try vials
sum_dups <- tray4$identifier_1[!tray4$identifier_1 %in% names(stand_vec)] %>% 
  duplicated() %>% 
  sum()

if(sum_dups > 0) {
  stop("duplicated identifier 1")
}

if(any(is.na(tray4$identifier_1))) {
  stop("identifier 1 missing")
}

if(any(is.na(tray4$identifier_2))) {
  stop("identifier 2 missing")
}

# samples nums that are not unique or missing
prob_samples <- str_detect(tray4$identifier_2, "missing|unique")

if (any(prob_samples)) {
  warning(sum(prob_samples), " ambiguous sample(s) present")
  print(tray4[prob_samples, ])
}

# saving files ------------------------------------------------------------

tray5 <- tray4 %>% 
  rename(Tray = tray, Vial = vial, `Identifier 1` = identifier_1,
         `Identifier 2` = identifier_2)

out1 <- tray5 %>% 
  filter(Tray == 1)


out2 <- tray5 %>% 
  filter(Tray == 2)

# check
stopifnot(
  nrow(out1) == 54,
  nrow(out2) == 50,
  all(!is.na(tray5$`Identifier 2`))
)

set_string <- str_pad(set_num, width = 2, side = "left", "0")
out1_path <- paste0("data-processed/sample_descriptions/hw",
                    set_string, "_1.csv")


out2_path <- paste0("data-processed/sample_descriptions/hw",
                    set_string, "_2.csv")

if (TRUE){
  write_csv(out1, out1_path)
  write_csv(out2, out2_path)
}

