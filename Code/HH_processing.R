################################################################################
# Code: HH_processing
# Date: 1/4/2024
#
# Programmer: Emma Crenshaw
#
# Purpose:
#   This code imports the household-level data for the ARMS study
#   and combines data from different time points and data managers
#
# Input:
#   Raw data HH data files as .xlsx
#
# Output:
#   9 datasets combining each wave and individual data collector's .xlsx files
#     - main: contains the single-response-per-participant data
#     - absent_repeat: contains the data from the repeated section about absent members triggered by the response to "abs_member"
#     - animal_repeat: contains the data from the repeated section about domestic animals triggered by the response to "howmany_other_animal"
#     - crops_repeat: contains the data from the repeated section about crops triggered by the response to "crops"
#     - income_repeat: contains the data from the repeated section about income triggered by the response to "source_of_income"
#     - salary_repeat: contains the data from the repeated section about salaried income triggered by the response to "job"
#     - wage_repeat: contains the data from the repeated section about wage-derived income triggered by the response to "wage_labor"
#     - member_new_repeat: contains the data from the repeated section about new members triggered by the response to "new_member"
#     - member_left_repeat: contains the data from the repeated section about members that have left triggered by the response to "left_member"
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

# Load utilities
source(here("Code", "HH_utils.R"))

###################################################
###################################################
#                   F1 and F2
###################################################
###################################################

# File locations
dd_file = here("Data Dictionary","HH_DataDictionary.xlsx")

list_files = c(here("HHData", "Raw","F1F2","madeleine_hh_april.xlsx"), here("HHData", "Raw","F1F2","eric_hh_april.xlsx"), 
               here("HHData", "Raw","F1F2","marc_hh_april.xlsx"), 
               here("HHData", "Raw","F1F2","romario_hh_april.xlsx"), here("HHData", "Raw","F1F2","andrea_hh_april.xlsx"))

# Variable to indicate the time point of data, used to name the files below
timepoint = "F1F2"

# Import and process the data
F1F2data <- load_ARMSData(dd_file, list_files, timepoint)

#!IMPORTANT! At this step, make sure there are only 9 datasets in the list (one per group). If there are more, 
# it means that they had different variables. Confirm why below before combining:
if (length(unique(names(F1F2data))) != length(names(F1F2data))){
  warning("Lists are not unique")
}

# Look at why the crops_repeat datasets and main datasets aren't being seen as 'the same'
var_crop1 <- ls(F1F2data[[7]])
var_crop2 <- ls(F1F2data[[8]])
all.equal(var_crop1,var_crop2)  # they look the same - this was fixed by adding 'mesurement' to some of the crops_repeat datasets when fixing variable types

var_main1 <- ls(F1F2data[[9]])
var_main2 <- ls(F1F2data[[10]])
all.equal(var_main1, var_main2) # the first has one additional variation
var_main1[! var_main1 %in% var_main2] # added the variable "ot_market", so it's still ok to stack them
var_main2[! var_main2 %in% var_main1]

## Combine the datasets that hadn't been combined before
F1F2data[[9]] <- bind_rows(F1F2data[[9]], F1F2data[[10]])
F1F2data[[10]] <- NULL #delete one

F1F2data[[7]] <- bind_rows(F1F2data[[7]], F1F2data[[8]])
F1F2data[[8]] <- NULL #delete one


# Bring the datasets into the global environment
list2env(F1F2data, envir = .GlobalEnv)


###################################################
###################################################
#                   F3
###################################################
###################################################

dd_file = here("Data Dictionary","HH_DataDictionary.xlsx")

list_files = c(here("HHData", "Raw","F3","madeleine_hh_0723.xlsx"), here("HHData", "Raw","F3","eric_hh_0723.xlsx"),
               here("HHData", "Raw","F3","marc_hh_0723.xlsx"), here("HHData", "Raw","F3","romario_hh_0723.xlsx"), 
               here("HHData", "Raw","F3","mahefa_hh_0723.xlsx"))

timepoint = "F3"

F3data <- load_ARMSData(dd_file, list_files, timepoint) #9 datasets, good to go

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

dd_file = here("Data Dictionary","HH_DataDictionary.xlsx")

list_files = c(here("HHData", "Raw","F4","madeleine_hh_1023.xlsx"), here("HHData", "Raw","F4","eric_hh_1023.xlsx"),
               here("HHData", "Raw","F4","marc_hh_1023.xlsx"), 
               here("HHData", "Raw","F4","romario_hh_1023.xlsx"), here("HHData", "Raw","F4","mahefa_hh_1023.xlsx"))

timepoint = "F4"

F4data <- load_ARMSData(dd_file, list_files, timepoint) #9 datasets, good to go

if (length(unique(names(F1F2data))) != length(names(F1F2data))){
  warning("Lists are not unique")
}

# Bring the datasets into the global environment
list2env(F4data, envir = .GlobalEnv)



###################################################
###################################################
#                   F5
###################################################
###################################################

dd_file = here("Data Dictionary","HH_DataDictionary.xlsx")

list_files = c(here("HHData", "Raw","F5","madeleine_hh_0124.xlsx"), here("HHData", "Raw","F5","eric_hh_0124.xlsx"),
               here("HHData", "Raw","F5","marc_hh_0124.xlsx"), 
               here("HHData", "Raw","F5","romario_hh_0124.xlsx"), here("HHData", "Raw","F5","mahefa_hh_0124.xlsx"))

timepoint = "F5"

F5data <- load_ARMSData(dd_file, list_files, timepoint) 

if (length(unique(names(F1F2data))) != length(names(F1F2data))){
  warning("Lists are not unique")
}

# Bring the datasets into the global environment
list2env(F5data, envir = .GlobalEnv)


###################################################
###################################################
###################################################
#                   Stack All
###################################################
###################################################

main = bind_rows(F1F2_main, F3_main, F4_main, F5_main)
absent_repeat = bind_rows(F1F2_absent_repeat, F3_absent_repeat, F4_absent_repeat, F5_absent_repeat)
animal_repeat = bind_rows(F1F2_animal_repeat, F3_animal_repeat, F4_animal_repeat, F5_animal_repeat)
crops_repeat= bind_rows(F1F2_crops_repeat, F3_crops_repeat, F4_crops_repeat, F5_crops_repeat)
income_repeat = bind_rows(F1F2_income_repeat, F3_income_repeat, F4_income_repeat, F5_income_repeat)
salary_repeat = bind_rows(F1F2_salary_repeat, F3_salary_repeat, F4_salary_repeat, F5_salary_repeat)
wage_repeat = bind_rows(F1F2_wage_repeat, F3_wage_repeat, F4_wage_repeat, F5_wage_repeat)
member_new_repeat = bind_rows(F1F2_member_new_repeat, F3_member_new_repeat, F4_member_new_repeat, F5_member_new_repeat)
member_left_repeat = bind_rows(F1F2_member_left_repeat, F3_member_left_repeat, F4_member_left_repeat, F5_member_left_repeat)

save(main, file = here("HHData", "main.RData"))
save(absent_repeat, file = here("HHData", "absent_repeat.RData"))
save(animal_repeat, file = here("HHData", "animal_repeat.RData"))
save(crops_repeat, file = here("HHData", "crops_repeat.RData"))
save(income_repeat, file = here("HHData", "income_repeat.RData"))
save(salary_repeat, file = here("HHData", "salary_repeat.RData"))
save(wage_repeat, file = here("HHData", "wage_repeat.RData"))
save(member_new_repeat, file = here("HHData", "member_new_repeat.RData"))
save(member_left_repeat, file = here("HHData", "member_left_repeat.RData"))
