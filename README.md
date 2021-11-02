# tracer
deuterium tracer study at hardware ranch

pre and post processing phalaborwa picarro data

# Description of folders:
scripts > where my R scripts live

data-raw > misc raw data files (eg sample descriptions)--idea is that these are files that I don't edit--just load into R picarro_output > raw output files from picarro

sample_descriptions > sample descriptions that have been cleaned up in R such that they can be read by the picarro.
These are label files, that are used in the picarro, when it is run (i.e. label numbers, etc.)

data-processed > output files from some analysis etc. e.g. when something is done to a raw data file this is where it ends up. hw_bad_vials_v1 > each picarro run data from some vials is thrown out(e.g. those that were flagged by picarro or had all missing values), these are csvs documenting from what vials data was discarded prior to further cleaning 

data-processed > clean_4cc > files that have been pre-processed for chem correct 

data-processed > chemcorrect_output: here are the files (xls) that chemcorrect creates. These excel files 
are then combined etc. 04_compile_ChemCorrect_output.R. Files in this folder are have following naming convention:
then end in hwXX_Y.xls, where XX is the run, and Y (takes values of 1 or 2), is the file number for that run. Y is necessary
because chemcorrect can only handle small files, and so they had to be split in two. 


# description of select data files found in data-processed
data_processed/hw_combined_picarro_output.csv--the raw combined picarro output (Vials Id's are correct). This file
is created in the 04_Process4ChemChorrect.R script. This would be the best file to use if you want to see 'raw'
data. 

## data_processed/hw_combined_cc_output.csv--the combined chemcorrect output with sample descriptions joined in. 
This is a good starting point for downstream analysis. 
Some important columns in this file include:

date--date sample collected

plot--plot number--this is the large 8x8 m plot (ie with rain-out shelter),
and 4 subplots were located in each plot, each subplot and a fixed injection
depth. 

depth--depth (cm) at which injection took place for the given subplot. Subplots
were located in each quadrant of the plot, and the depth was assigned pseudo-randomly. 

date_inject--date on which the injection took place. 

days_since_inject--number of days between injection and sample collection. 

sample_num--the unique identifier of the original vegetation sample
collected in the field. 

vial_num--unique identifier of the vial (there can be multiple vials per sample). 

species--species of sample. 
distance--for sagebrush only--three distance categories 1 (50 cm from subplot center), 2 (within subplot but not within 50 cm), 3 (within 50 cm of the subplot circle--ie outside the plot). Maybe worth dropping distance 3 in some data analyses.

Correction_made--enter 1 if data entry mistake was found, and the value was corrected (e.g., vial_num value corrected). Enter 2 if no change made but there may be a mistake (i.e illegible handrwriting), otherwise put a 0 if values look ok.

cal_18o_mean--mean calibrated 18o (oxygen), level (per mil). This is the corrected value, as per chemcorrect. The 
corrections of 18o are suspect. therefore using uncalibrated may be prudent. 

cal_2h_mean--mean calibrated 2h (deuterium), level. Most important data. --i.e. we injected deuterium. 

raw_18o--uncalibrated (i.e. from picarro) 18o. Not adjusted by chem correct. 

raw_2h_mean--uncalibrated deuterium levels (per mil). 

pft--plant functional type (based on the species column). 

trmt--treatment (as in ecology paper)

lohi--does the treatment fall in the high or low precipitation intensity category. 

## Descriptions of scripts

Note: leading numbers on script names denotes the order in which the scripts are used.

01_process_vial_nums.R--was just used to print sheets of paper for sorting vials. 

02_create_sample_descriptions.R--this script is for creating sample description files for use with Picarro
These files are needed everytime the picarro is run. 

03_Process4ChemCorrect.R--takes the raw output from the picarro, and removes some obviously 
bad samples, and creates the files needed to input into chemcorrect. Among other things. 

04_compile_ChemCorrect_output--combines chem correct output and adds information
about the samples (like plot, species, depth, etc. ). There are some vial number issues
that I attempt to deal with also. 

05_calc_prop_water_uptake--work in progress. Started to calculate proportional water, with depth, for plant functional
types. 


## Notes of caution about the data--

If the previous vial was hot, then the next vial measured can be inaccurate
because of a carry over effect. By default, i've thrown out the first three measurements
of each vial, and kept the second three. This takes care of most but not all of this problem.
Examining drift in the hw_compined_picarro_output.csv file would be a way to find these cases. 

