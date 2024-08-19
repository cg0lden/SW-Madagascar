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

#####################################################
# Load nutritional composition data
#####################################################

nutcomp <- read_xlsx(here("NutritionComp", "ARMS Nut composition_edited.xlsx"))

nutcomp2 <- nutcomp %>%
  pivot_longer(cols = c(6:21), 
               names_to = "serving", 
               values_to = "grams") %>%
  select("new_group", "old_group", "varname", "eng_name", "kcal_100g", "serving", "grams")

nutcomp_long <- nutcomp2[!is.na(nutcomp2$grams),]
nutcomp_long <- nutcomp_long[nutcomp_long$new_group!= "(do not include)",]
nutcomp_long$serving <- tolower(nutcomp_long$serving)

#####################################################
# Load Individual data
#####################################################

indfiles = list.files(path=here("IndData"), pattern=".RData")
lapply(indfiles, function(x) load(file = here("IndData", x), envir = .GlobalEnv))

indfiles_list <- gsub(".RData", "", indfiles)

#####################################################
# Merge nutritional composition data
#####################################################
# Get list of food files
#### either don't include or must be treated differently
indfiles_list_foods <- indfiles_list[!grepl("sauce|member|fishing_area|guest|main|occupation|condiment|beverage", indfiles_list)]
#### data not ready yet
indfiles_list_foods <- indfiles_list_foods[!grepl("inverts|fish", indfiles_list_foods)]

vargroups = gsub("_repeat", "", indfiles_list_foods)


NA_serving = data.frame(dataset = c(), selected = c(), serving = c())

for (i in 1:length(indfiles_list_foods)){
  dat <- get(indfiles_list_foods[i])
  vargroup = vargroups[i]
  
  print(vargroup)
  
  if(grepl("dairy", vargroup)==T){
    colnames(dat) <- gsub("diary", "dairy", colnames(dat))
  }
  
  if(grepl("tubers", vargroup)==T){
    colnames(dat) <- gsub("type_tubers", "selected", colnames(dat))
  }
  
  colnames(dat) <- gsub(paste0(gsub("_outside", "", vargroup),"_"), "", colnames(dat))
  colnames(dat) <- gsub("_outside", "", colnames(dat))
  
  dat2 <- left_join(dat, nutcomp_long[nutcomp_long$old_group==gsub("_outside", "", vargroup),], by = c("selected" = "varname", "serving" = "serving")) %>% 
                              select(-c("_parent_table_name", "_submission__id",            
                              "_submission__validation_status", "_submission__notes",
                               "_submission__status", "_submission__submitted_by","_submission___version__", "_submission__tags")) %>% 
                              mutate(tot_kcal = serving_size * grams * kcal_100g/100)
  
  nocal = unique(dat2[is.na(dat2$tot_kcal),c("selected", "serving")])
  nocal$dataset = rep(vargroup, length(nocal$selected))
  
  NA_serving = rbind(NA_serving, nocal)
  
  assign(indfiles_list_foods[i], dat2)
}


write.csv(NA_serving, here("NutritionComp", "NA_nutcomp.csv"), row.names = FALSE)


