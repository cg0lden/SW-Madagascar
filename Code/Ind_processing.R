################################################################################
# Code: Import Individual Data
# Date: 1/4/2024
#
# Programmer: Emma Crenshaw
#
# Purpose:
#   This code imports the individual-level data for the ARMS study
#   and combines data from different time points and data managers
#
# Input:
#   Raw individual-level data files as .xlsx
#
# Output:
#  
#---------------------------------------------------------------------
# Notes:
#   - Each user will have to update their own working directory (where
#     their data is saved), as well as the location of the data dictionary
#     and each data file
#
#   - It is important to note the structure of the data: each excel document
#     has multiple sheets which must all be imported. Most of these have nonsense
#     names (begin_repeat_string of letters) and must be identified by the variables
#     they contain. These are caused by the 'repeat' sections in the survey and
#     thus may contain more than one response per participant. Additionally,
#     the same repeat section may be separated by month of data collection.
#     These repeat sections can be combined.
#
#   - It is vital that a correct and up to date data dictionary is used,
#     as that will define the variable types
################################################################################

# Import packages
library(tidyverse)
library(readxl)
library(stringr)
library(here)
library(lubridate)

# Load utilities
source(here("Code", "Ind_utils.R"))


# Data dictionary and translation files
dd_file = here("Data Dictionary","Ind_DataDictionary.xlsx")
trans_file <- here("Data Dictionary", "Translation_formats.xlsx")

###################################################
###################################################
#                   F1 and F2
###################################################
###################################################

# File locations

list_files = c(here("IndData", "Raw","F1F2","madeleine_ind_april.xlsx"), here("IndData", "Raw","F1F2","eric_ind_april.xlsx"), 
               here("IndData", "Raw","F1F2","marc_ind_april.xlsx"), here("IndData", "Raw","F1F2","romario_ind_april.xlsx"), 
               here("IndData", "Raw","F1F2","andrea_ind_april.xlsx"))

# Variable to indicate the time point of data, used to name the files below
timepoint = "F1F2"

# Import and process the data (this step may take a few minutes)
F1F2data <- load_ARMSData(dd_file, trans_file, list_files, timepoint)

#!IMPORTANT! At this step, make sure there is only one dataset per excel sheet/variable group. If there are more, 
# it means that they had different variables. Confirm why below before combining:
if (length(unique(names(F1F2data))) != length(names(F1F2data))){
  warning("Lists are not unique")
}

# Look at why the member datasets aren't being seen as 'the same'
var_member1 <- ls(F1F2data[[24]])
var_member2 <- ls(F1F2data[[25]])
var_member3 <- ls(F1F2data[[26]])
all.equal(var_member1, var_member2) #these are different (there is one extra variable)
all.equal(var_member3, var_member2) # these are the same

var_member1[! var_member1 %in% var_member2] # added the variable "blood_pressure", so it's still ok to stack them
var_member2[! var_member2 %in% var_member1]

## Combine the datasets that hadn't been combined before
F1F2data[[24]] <- bind_rows(F1F2data[[24]], F1F2data[[25]], F1F2data[[26]])
F1F2data[[26]] <- NULL #delete
F1F2data[[25]] <- NULL #delete

# Bring the datasets into the global environment
list2env(F1F2data, envir = .GlobalEnv)


###################################################
###################################################
#                   F3
###################################################
###################################################

list_files = c(here("IndData", "Raw","F3","madeleine_ind_0723.xlsx"), here("IndData", "Raw","F3","eric_ind_0723.xlsx"), 
               here("IndData", "Raw","F3","marc_ind_0723.xlsx"), here("IndData", "Raw","F3","romario_ind_0723.xlsx"),
               here("IndData", "Raw","F3","mahefa_ind_0723.xlsx"))

timepoint = "F3"

F3data <- load_ARMSData(dd_file, trans_file, list_files, timepoint) #36 datasets, good to go

# Check that there aren't repeats
if (length(unique(names(F3data))) != length(names(F3data))){
  warning("Lists are not unique")
}

# Bring the datasets into the global environment
list2env(F3data, envir = .GlobalEnv)


###################################################
###################################################
#                   F4
###################################################
###################################################

list_files = c(here("IndData", "Raw","F4","madeleine_ind_1023.xlsx"), here("IndData", "Raw","F4","eric_ind_1023.xlsx"),
               here("IndData", "Raw","F4","marc_ind_1023.xlsx"), here("IndData", "Raw","F4","romario_ind_1023.xlsx"), 
               here("IndData", "Raw","F4","mahefa_ind_1023.xlsx"))

timepoint = "F4"

F4data <- load_ARMSData(dd_file, trans_file, list_files, timepoint) #34 datasets, good to go

# Check that there aren't repeats
if (length(unique(names(F3data))) != length(names(F3data))){
  warning("Lists are not unique")
}

# Bring the datasets into the global environment
list2env(F4data, envir = .GlobalEnv)


###################################################
###################################################
#                   F5
###################################################
###################################################

list_files = c(here("IndData", "Raw","F5","madeleine_ind_0124.xlsx"), here("IndData", "Raw","F5","eric_ind_0124.xlsx"),
               here("IndData", "Raw","F5","marc_ind_0124.xlsx"), here("IndData", "Raw","F5","romario_ind_0124.xlsx"), 
               here("IndData", "Raw","F5","mahefa_ind_0124.xlsx"))

timepoint = "F5"

F5data <- load_ARMSData(dd_file, trans_file, list_files, timepoint) #34 datasets, good to go

# Check that there aren't repeats
if (length(unique(names(F4data))) != length(names(F4data))){
  warning("Lists are not unique")
}

# Bring the datasets into the global environment
list2env(F5data, envir = .GlobalEnv)



###################################################
###################################################
#                   Stack All
###################################################
###################################################

# Not all waves of data collection have data from every possible repeating section
# Some repeating sections have no data yet, so I have commented them out. If they have data in future waves, make sure to run those lines

beans_sauce_repeat = bind_rows(F1F2_beans_sauce_repeat, F3_beans_sauce_repeat, F4_beans_sauce_repeat, F5_beans_sauce_repeat)
beans_repeat = bind_rows(F1F2_beans_repeat, F3_beans_repeat, F4_beans_repeat, F5_beans_repeat)
beans_outside_repeat = bind_rows(F3_beans_outside_repeat, F4_beans_outside_repeat, F5_beans_outside_repeat)
beverage_repeat = bind_rows(F1F2_beverage_repeat, F3_beverage_repeat, F4_beverage_repeat, F5_beverage_repeat)
beverage_outside_repeat = bind_rows(F1F2_beverage_outside_repeat, F3_beverage_outside_repeat, F4_beverage_outside_repeat, F5_beverage_outside_repeat)
bread_repeat = bind_rows(F1F2_bread_repeat, F3_bread_repeat, F4_bread_repeat, F5_bread_repeat)
bread_outside_repeat = bind_rows(F1F2_bread_outside_repeat, F3_bread_outside_repeat, F4_bread_outside_repeat, F5_bread_outside_repeat)
condiment_repeat = bind_rows(F1F2_condiment_repeat, F3_condiment_repeat, F4_condiment_repeat, F5_condiment_repeat)
#corn_sauce_repeat = bind_rows(F4_corn_sauce_repeat)
dairy_repeat = bind_rows(F1F2_dairy_repeat, F3_dairy_repeat, F5_dairy_repeat)
dairy_outside_repeat = bind_rows(F1F2_dairy_outside_repeat, F3_dairy_outside_repeat, F5_dairy_outside_repeat)
fat_food_repeat = bind_rows(F5_fat_food_repeat)
fish_sauce_repeat = bind_rows(F1F2_fish_sauce_repeat, F3_fish_sauce_repeat, F4_fish_sauce_repeat, F5_fish_sauce_repeat)
fish_repeat = bind_rows(F1F2_fish_repeat, F3_fish_repeat, F4_fish_repeat, F5_fish_repeat)
fish_outside_repeat = bind_rows(F1F2_fish_outside_repeat, F3_fish_outside_repeat, F4_fish_outside_repeat, F5_fish_outside_repeat)
fishing_area_repeat = bind_rows(F1F2_fishing_area_repeat, F3_fishing_area_repeat, F4_fishing_area_repeat, F5_fishing_area_repeat)
fruits_repeat = bind_rows(F1F2_fruits_repeat, F3_fruits_repeat, F4_fruits_repeat, F5_fruits_repeat)
fruits_outside_repeat = bind_rows(F1F2_fruits_outside_repeat, F3_fruits_outside_repeat, F4_fruits_outside_repeat, F5_fruits_outside_repeat)
greens_sauce_repeat = bind_rows(F1F2_greens_sauce_repeat, F3_greens_sauce_repeat, F4_greens_sauce_repeat, F5_greens_sauce_repeat)
greens_repeat = bind_rows(F1F2_greens_repeat, F3_greens_repeat, F4_greens_repeat, F5_greens_repeat)
greens_outside_repeat = bind_rows(F3_greens_outside_repeat, F4_greens_outside_repeat, F5_greens_outside_repeat)
guest_repeat = bind_rows(F1F2_guest_repeat, F3_guest_repeat, F4_guest_repeat, F5_guest_repeat)
marine_inverts_sauce_repeat = bind_rows(F1F2_marine_inverts_sauce_repeat, F3_marine_inverts_sauce_repeat, F4_marine_inverts_sauce_repeat, F5_marine_inverts_sauce_repeat)
marine_inverts_repeat = bind_rows(F1F2_marine_inverts_repeat, F3_marine_inverts_repeat, F4_marine_inverts_repeat, F5_marine_inverts_repeat)
marine_inverts_outside_repeat = bind_rows(F1F2_marine_inverts_outside_repeat, F3_marine_inverts_outside_repeat, F4_marine_inverts_outside_repeat, F5_marine_inverts_outside_repeat)
meat_sauce_repeat = bind_rows(F1F2_meat_sauce_repeat, F3_meat_sauce_repeat, F4_meat_sauce_repeat, F5_meat_sauce_repeat)
meat_repeat = bind_rows(F1F2_meat_repeat, F3_meat_repeat, F4_meat_repeat, F5_meat_repeat)
meat_outside_repeat = bind_rows(F3_meat_outside_repeat, F4_meat_outside_repeat, F5_meat_outside_repeat)
member = bind_rows(F1F2_member, F3_member, F4_member, F5_member)
occupation_repeat = bind_rows(F1F2_occupation_repeat, F3_occupation_repeat, F4_occupation_repeat, F5_occupation_repeat)
rice_sauce_repeat = bind_rows(F1F2_rice_sauce_repeat)
snacks_repeat = bind_rows(F1F2_snacks_repeat, F3_snacks_repeat, F4_snacks_repeat, F5_snacks_repeat)
snacks_outside_repeat = bind_rows(F1F2_snacks_outside_repeat, F3_snacks_outside_repeat, F4_snacks_outside_repeat, F5_snacks_outside_repeat)
main = bind_rows(F1F2_main, F3_main, F4_main, F5_main)
tubers_repeat = bind_rows(F1F2_tubers_repeat, F3_tubers_repeat, F4_tubers_repeat, F5_tubers_repeat)
#tubers_sauce_repeat = bind_rows(F4_tubers_sauce_repeat)
tubers_outside_repeat = bind_rows(F1F2_tubers_outside_repeat, F3_tubers_outside_repeat, F4_tubers_outside_repeat, F5_tubers_outside_repeat)
vegetables_sauce_repeat = bind_rows(F1F2_vegetables_sauce_repeat, F3_vegetables_sauce_repeat, F4_vegetables_sauce_repeat, F5_vegetables_sauce_repeat)
vegetables_repeat = bind_rows(F1F2_vegetables_repeat, F3_vegetables_repeat, F4_vegetables_repeat, F5_vegetables_repeat)
vegetables_outside_repeat = bind_rows(F3_vegetables_outside_repeat, F4_vegetables_outside_repeat, F5_vegetables_outside_repeat)


###################################################
###################################################
###################################################
#                 Add variables
###################################################
###################################################
main$collection_wave <- ifelse('2023-01-01' <= main$today & main$today <= '2023-02-18', 1,
                               ifelse('2023-04-01' <= main$today & main$today <= '2023-05-18', 2,
                                      ifelse('2023-07-01' <= main$today & main$today <= '2023-08-18', 3,
                                             ifelse('2023-10-10' <= main$today & main$today <= '2023-11-18', 4,
                                                    ifelse('2024-01-10' <= main$today & main$today <= '2024-02-14', 5, 99)))))


###################################################
###################################################
###################################################
#                   Save All 
###################################################
###################################################

# if you 'added' a new file in the section above make sure you also add it here so it saves
save(beans_sauce_repeat, file = here("IndData", 'beans_sauce_repeat.RData'))
save(beans_repeat, file = here("IndData", 'beans_repeat.RData'))
save(beans_outside_repeat, file = here("IndData", 'beans_outside_repeat.RData'))
save(beverage_repeat, file = here("IndData", 'beverage_repeat.RData'))
save(beverage_outside_repeat, file = here("IndData", 'beverage_outside_repeat.RData'))
save(bread_repeat, file = here("IndData", 'bread_repeat.RData'))
save(bread_outside_repeat, file = here("IndData", 'bread_outside_repeat.RData'))
save(condiment_repeat, file = here("IndData", 'condiment_repeat.RData'))
#save(corn_sauce_repeat, file = here("IndData", 'corn_sauce_repeat.RData'))
save(dairy_repeat, file = here("IndData", 'dairy_repeat.RData'))
save(dairy_outside_repeat, file = here("IndData", 'dairy_outside_repeat.RData'))
save(fat_food_repeat, file = here("IndData", 'fat_food_repeat.RData'))
save(fish_sauce_repeat, file = here("IndData", 'fish_sauce_repeat.RData'))
save(fish_repeat, file = here("IndData", 'fish_repeat.RData'))
save(fish_outside_repeat, file = here("IndData", 'fish_outside_repeat.RData'))
save(fishing_area_repeat, file = here("IndData", 'fishing_area_repeat.RData'))
save(fruits_repeat, file = here("IndData", 'fruits_repeat.RData'))
save(fruits_outside_repeat, file = here("IndData", 'fruits_outside_repeat.RData'))
save(greens_sauce_repeat, file = here("IndData", 'greens_sauce_repeat.RData'))
save(greens_repeat, file = here("IndData", 'greens_repeat.RData'))
save(greens_outside_repeat, file = here("IndData", 'greens_outside_repeat.RData'))
save(guest_repeat, file = here("IndData", 'guest_repeat.RData'))
save(marine_inverts_sauce_repeat, file = here("IndData", 'marine_inverts_sauce_repeat.RData'))
save(marine_inverts_repeat, file = here("IndData", 'marine_inverts_repeat.RData'))
save(marine_inverts_outside_repeat, file = here("IndData", 'marine_inverts_outside_repeat.RData'))
save(meat_sauce_repeat, file = here("IndData", 'meat_sauce_repeat.RData'))
save(meat_repeat, file = here("IndData", 'meat_repeat.RData'))
save(meat_outside_repeat, file = here("IndData", 'meat_outside_repeat.RData'))
save(member, file = here("IndData", 'member.RData'))
save(occupation_repeat, file = here("IndData", 'occupation_repeat.RData'))
save(rice_sauce_repeat, file = here("IndData", 'rice_sauce_repeat.RData'))
save(snacks_repeat, file = here("IndData", 'snacks_repeat.RData'))
save(snacks_outside_repeat, file = here("IndData", 'snacks_outside_repeat.RData'))
save(main, file = here("IndData", 'main.RData'))
save(tubers_repeat, file = here("IndData", 'tubers_repeat.RData'))
#save(tubers_sauce_repeat, file = here("IndData", 'tubers_sauce_repeat.RData'))
save(tubers_outside_repeat, file = here("IndData", 'tubers_outside_repeat.RData'))
save(vegetables_sauce_repeat, file = here("IndData", 'vegetables_sauce_repeat.RData'))
save(vegetables_repeat, file = here("IndData", 'vegetables_repeat.RData'))
save(vegetables_outside_repeat, file = here("IndData", 'vegetables_outside_repeat.RData'))

