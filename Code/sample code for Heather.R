################################################################################
# Code: Sample code for Heather
# Date: 6/3/2024
#
# Programmer: Emma Crenshaw
#
# Purpose:
#   This code shows an example of how to merge datasets and create an
#   indicator if there is an oil sauce
#
# Input:
#   
#
# Output:
#  
#---------------------------------------------------------------------
# Notes:
################################################################################

# Import packages
library(tidyverse)
library(readxl)
library(stringr)
library(here)
library(lubridate)


# Load data
load(here("IndData", 'vegetables_repeat.RData'))
load(here("IndData", 'vegetables_sauce_repeat.RData'))
load(here("IndData", 'main.RData'))


#######################################################################################
# Example of how to make an indicator variable for whether the sauce is oil
#   the function "grepl" looks for patterns in strings; if it is found, the if/else
#   statement returns a 1, otherwise it returns a 0
#######################################################################################
veggie <- vegetables_repeat
veggie$oil <- ifelse(grepl("oil", veggie$sauce_type_vegetables), 1, 0) 

# If you only care about the sauces that are oil, you can restrict the sauce dataset to just that
oil_sauce <- vegetables_sauce_repeat[vegetables_sauce_repeat$vegetables_sauce_selected=="oil",]

# You can check if this is the same as the number of veggies indicated as having an oil sauce:
# sum(veggie$oil)

#######################################################################################
# Example of how to merge datasets
#   You want to merge based on ID and submission day (the submission time may differ based on 
#   how quickly they filled out the survey)
#
#   I'll show two options - one uses the variable names currently in the data,
#   the other creates a new variable to merge on
#
# IMPORTANT NOTE: if someone reports more than one vegetable on a given day, you'll start getting multiple entries per person
#
#######################################################################################

# get submission day
main$merge_day <- date(main$`_submission_time`)
veggie$merge_day <- date(veggie$`_submission__submission_time`)
oil_sauce$merge_day <- date(oil_sauce$`_submission__submission_time`)

#################################################
# Option 1: use the variable names as they are
#################################################

# now you can merge them together
veggie_option1 <- full_join(veggie, oil_sauce[, c("_submission__id", "merge_day", "vegetables_sauce_selected", "vegetables_sauce_serving", "vegetables_sauce_serving_size")], 
                            by = c("_submission__id", "merge_day"))
# restricting to just the variables we want for sanity's sake
veggie_res_option1 <- veggie_option1[, c("vegetables_selected","vegetables_serving","vegetables_serving_size","vegetables_cooking_mode",       
                              "sauce_type_vegetables", "ot_sauce_vegetables", "nb_sauce_vegetables", "_submission__id", "oil",                           
                              "merge_day", "vegetables_sauce_selected", "vegetables_sauce_serving", "vegetables_sauce_serving_size" )]

option1 <- full_join(main, veggie_res_option1, by = c("_id" = "_submission__id", "merge_day") )

#################################################
# Option 2: create a new variable to merge on called "dayperson", a single variable that combines the id and date
#################################################

veggie_res_option2 <- veggie_res_option1 %>% mutate(dayperson = paste(veggie_res_option1$`_submission__id`, veggie_res_option1$merge_day, sep = "_"))
main$dayperson <- paste(main$`_id`, main$merge_day, sep = "_")

option2 <- full_join(main, veggie_res_option2, by = c("dayperson"))


