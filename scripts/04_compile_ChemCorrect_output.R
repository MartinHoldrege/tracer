# Martin Holdrege

# script started May 26, 2021

# The goal here is to extract the necessary columns from the chemcorrect
# .xls files, combine together and then add in sample descriptions

# dependencies ------------------------------------------------------------

library(tidyverse)
library(readxl)

# this just makes available a function that knows which plots
# at HW ranch had what treatments
source("https://raw.githubusercontent.com/MartinHoldrege/Rfunctions/master/assign_trmts_functions.r")

# read in data ------------------------------------------------------------

# * chemcorrect output ----------------------------------------------------

cc_paths <- list.files("data-processed/chemcorrect_output",
                       full.names = TRUE)

run_name <- str_extract(cc_paths, "hw\\d+_\\d")
names(cc_paths) <- run_name

# check if multiple files for a given run
if(any(duplicated(run_name))) {
  warning("duplicated chemcorrect output files")
}

raw1 <- map(cc_paths, read_xls, sheet = "Summary", skip = 3)

# source data that went into chem correct (ie uncorrected/unaveraged)
# want to use raw 180 values
cc_source <- map(cc_paths, read_xls, sheet = "Source", skip = 2)

# * sample descriptions ---------------------------------------------------

# sample and vial info joined together
# created in "scripts/02_create_sample_descriptions.R"
sample_desc <- read_csv("data-processed/sample_and_vial_info.csv")

# clean -------------------------------------------------------------------

# avg raw 180 values

cc_source_mean <- map(cc_source, function(df) {
  df %>% 
    group_by(Sample, `Identifier 1`) %>% 
    summarize(raw_18o_mean = mean(as.numeric(`d(18_16)Mean`)),
              raw_2h_mean = mean(as.numeric(`d(D_H)Mean`))) %>% 
    mutate(Sample = as.numeric(Sample))
})

# combining summary cc output and the source (uncorrected values)
raw2 <- map2(raw1, cc_source_mean, .f = left_join, 
             by = c("Sample", "Name" = "Identifier 1"))

# combining chem correct files
cc1 <- bind_rows(raw2, .id = "run")

names(cc1) <- janitor::make_clean_names(names(cc1))

cc2 <- cc1 %>% 
  rename(cal_18o_mean = calibrated_d_sup_18_sup_o_mean,
         cal_2h_mean = calibrated_d_sup_2_sup_h_mean)

# remove standards
cc3 <- cc2 %>% 
  filter(!name %in% c("Low", "Medium", "High", "Dummy", "Tap"))

# join chemcorrect output w/ descriptions ---------------------------------

# combined file
comb1 <- sample_desc %>% 
  mutate(vial_num = as.character(vial_num)) %>% 
  right_join(cc3, by = c("vial_num" = "name"))

# check for issues
if (any(is.na(comb1$identifier_2))) {
  stop("some vial nums not joined")
}

# joined file exluding vials with non-unique taxa etc. 
comb_good1 <- comb1 %>% 
  # excluding vials where there was ambiguity in vials or sample numbers
  filter(str_detect(identifier_2, "plot"))

# n samples discarded:
nrow(comb1) - nrow(comb_good1)
nrow(comb1)

names(comb_good1) %>% 
  str_replace_all("_sup_|_sub_", "")

# add PFT ---------------------------------------------------------------
x <- comb1$species %>% unique %>% sort
pft_lookup <- c('achmil' = 'forb', 
                'arttri' = 'shrub', 
                'astpan' = 'forb', 
                'brocom' = 'grass', 
                'chrvis' = 'shrub', 
                'cirsium' = 'forb', 
                'comp' = 'all', 
                'elytra' = 'grass', 
                'forb' = 'forb', 
                'grass' = 'grass', 
                'koecri' = 'grass', 
                'leyceu' = 'grass', 
                'luparg' = 'forb', 
                'medsat' = 'forb', 
                'passmi' = 'grass', 
                'penspe' = 'forb', 
                'poacom' = 'grass', 
                'poapra' = 'grass', 
                'psespi' = 'grass', 
                'symchi' = 'forb', 
                'vida' = 'forb', 
                'viospp' = 'forb')

stopifnot(
  all(comb_good1$species %in% names(pft_lookup))
)

comb_good1$pft <- pft_lookup[comb_good1$species]


# add trmt label ----------------------------------------------------------

comb_good1$trmt <- trmts_HWRanch(comb_good1$plot, dump_label = TRUE)
comb_good1$lohi <- lohi_HWRanch(comb_good1$plot)

# save files --------------------------------------------------------------

write_csv(comb_good1, "data-processed/hw_combined_cc_output.csv")


