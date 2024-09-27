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

nutcomp <- read_xlsx(here("NutritionComp", "ARMS Nut composition_edited_Sep_25.xlsx"))

nutcomp2 <- nutcomp %>%
  pivot_longer(cols = c(6:21), 
               names_to = "serving", 
               values_to = "grams") %>%
  select("new_group", "old_group", "varname", "eng_name", "kcal_100g", "serving", "grams")

nutcomp_long <- nutcomp2[!is.na(nutcomp2$grams),]
nutcomp_long <- nutcomp_long[nutcomp_long$new_group!= "(do not include)",]
nutcomp_long$serving <- tolower(nutcomp_long$serving)
nutcomp_long$serving <- ifelse(nutcomp_long$serving == "teaspoon", "tea_spoon", nutcomp_long$serving)
nutcomp_long$serving <- ifelse(nutcomp_long$serving == "tablespoon", "table_spoon", nutcomp_long$serving)

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
indfiles_list_foods <- indfiles_list[!grepl("sauce|member|fishing_area|guest|main|occupation|condiment", indfiles_list)]
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
  
  
  if(grepl("meat", vargroup)==T){
    dat$selected <- tolower(dat$selected)
  }
  
  if(grepl("beverage", vargroup)==T){
    coff_tea <- dat[dat$selected %in% c("coffee", "tea"),]
    dat <- dat[!(dat$selected %in% c("coffee", "tea")),]
  }
  

  dat$merge_day = date(dat$`_submission__submission_time`)
  dat$person = dat$`_submission__id`
  dat$person_day = paste0(dat$`_submission__id`, "_", dat$merge_day)
  
  dat2 <- left_join(dat, nutcomp_long[nutcomp_long$old_group==gsub("_outside", "", vargroup),], by = c("selected" = "varname", "serving" = "serving")) %>% 
                              select(-c("_parent_table_name", "_submission__id",  "_submission__uuid",            
                              "_submission__validation_status", "_submission__notes", "_submission__submission_time",
                               "_submission__status", "_submission__submitted_by","_submission___version__", "_submission__tags")) %>% 
                              mutate(tot_kcal = serving_size * grams * kcal_100g/100)
  
  nocal = unique(dat2[is.na(dat2$tot_kcal),c("selected", "serving")])
  nocal$dataset = rep(vargroup, length(nocal$selected))
  
  NA_serving = rbind(NA_serving, nocal)
  
  assign(indfiles_list_foods[i], dat2)
  
  
}

NA_serving[!(NA_serving$selected %in% c("coffee", "tea")),]


#####################################################
# Look at oil in sauces
#####################################################
# Get list of sauce files
indfiles_list_sauce <- indfiles_list[grepl("sauce", indfiles_list)]
saucegroups = gsub("_repeat", "", indfiles_list_sauce)

for (i in 1:length(indfiles_list_sauce)){
  dat <- get(indfiles_list_sauce[i])
  saucegroup = saucegroups[i]
  
  print(saucegroup)
  
  
  colnames(dat) <- gsub(paste0(gsub("_outside", "", saucegroup),"_"), "", colnames(dat))
  colnames(dat) <- gsub("_outside", "", colnames(dat))
  
  dat <- dat[dat$selected=="oil",]
  dat$merge_day = date(dat$`_submission__submission_time`)
  dat$person = dat$`_submission__id`
  dat$person_day = paste0(dat$`_submission__id`, "_", dat$merge_day)
  
  dat2 <- left_join(dat, nutcomp_long[nutcomp_long$old_group=="added from sauce add ons",], by = c("selected" = "varname", "serving" = "serving")) %>% 
    select(-c("_parent_table_name", "_submission__id",  "_submission__uuid",            
              "_submission__validation_status", "_submission__notes", "_submission__submission_time",
              "_submission__status", "_submission__submitted_by","_submission___version__", "_submission__tags")) %>% 
    mutate(tot_kcal = serving_size * grams * kcal_100g/100)

  assign(indfiles_list_sauce[i], dat2)
}

#####################################################
# Get corn/rice data from member file
#####################################################
# rice
mem_rice_in <- member[,c("_submission__id", "_submission__submission_time", 
                           "rice_serving", "rice_serving_size")]
mem_rice_out <- member[,c("_submission__id", "_submission__submission_time", 
                           "rice_serving_outside", "rice_serving_size_outside")]

mem_rice_in2 <- left_join(mem_rice_in[!is.na(mem_rice_in$rice_serving_size),], nutcomp_long[nutcomp_long$varname=="rice",], 
                          by = c("rice_serving" = "serving")) %>%
                          mutate(kcal = rice_serving_size * grams * kcal_100g/100, 
                                  day = date(`_submission__submission_time`), person = `_submission__id`, 
                                 person_day = paste0(`_submission__id`, "_", date(`_submission__submission_time`))) %>%
                          select(person, day,person_day, new_group,  kcal)


mem_rice_out2 <- left_join(mem_rice_out[!is.na(mem_rice_out$rice_serving_size_outside),], nutcomp_long[nutcomp_long$varname=="rice",], 
                          by = c("rice_serving_outside" = "serving")) %>%
                          mutate(kcal = rice_serving_size_outside * grams * kcal_100g/100, 
                                 day = date(`_submission__submission_time`), person = `_submission__id`, 
                                 person_day = paste0(`_submission__id`, "_", date(`_submission__submission_time`))) %>%
                          select(person, day, person_day, new_group, kcal)

# Corn
mem_corn_in <- member[,c("_submission__id", "_submission__submission_time", 
                           "corns_serving", "corns_serving_size")]

mem_corn_out <- member[,c("_submission__id", "_submission__submission_time", 
                          "corns_serving_outside", "corns_serving_size_outside")]


mem_corn_in2 <- left_join(mem_corn_in[!is.na(mem_corn_in$corns_serving_size),], nutcomp_long[nutcomp_long$varname=="corns",], 
                          by = c("corns_serving" = "serving")) %>%
                          mutate(kcal = corns_serving_size * grams * kcal_100g/100, 
                                 day = date(`_submission__submission_time`), person = `_submission__id`, 
                                 person_day = paste0(`_submission__id`, "_", date(`_submission__submission_time`))) %>%
                          select(person, day,person_day, new_group, kcal)


mem_corn_out2 <- left_join(mem_corn_out[!is.na(mem_corn_out$corns_serving_size_outside),], nutcomp_long[nutcomp_long$varname=="corns",], 
                           by = c("corns_serving_outside" = "serving")) %>%
                            mutate(kcal = corns_serving_size_outside * grams * kcal_100g/100, 
                                   day = date(`_submission__submission_time`), person = `_submission__id`, 
                                   person_day = paste0(`_submission__id`, "_", date(`_submission__submission_time`))) %>%
                            select(person, day,person_day, new_group, kcal)


member_nut = rbind(mem_rice_in2, mem_rice_out2, mem_corn_in2, mem_corn_out2)


#####################################################
# Aggregate kcal by person/day and new group
#####################################################

#### either don't include or must be treated differently
indfiles_list_foods <- indfiles_list[!grepl("member|fishing_area|guest|main|occupation|condiment", indfiles_list)]
#### data not ready yet
indfiles_list_foods <- indfiles_list_foods[!grepl("inverts|fish", indfiles_list_foods)]

vargroups = gsub("_repeat", "", indfiles_list_foods)

aggregated = data.frame(person = NA, day = NA, person_day = NA, new_group = NA, kcal = NA)

for (i in 1:length(indfiles_list_foods)){
  dat <- get(indfiles_list_foods[i])
  vargroup = vargroups[i]
  
  print(vargroup)

  new <- aggregate(dat$tot_kcal, by = list(dat$person, dat$merge_day, dat$person_day, dat$new_group), FUN="sum")
  colnames(new) = c('person', 'day', 'person_day', 'new_group', 'kcal')
  aggregated <- rbind(aggregated, new)
}

aggregated <- rbind(aggregated, member_nut)

# Aggregate again to collect same group across datasets
aggregated2 <- aggregate(aggregated$kcal, by = list(aggregated$person, as.Date(aggregated$day), aggregated$person_day, aggregated$new_group), FUN="sum")
colnames(aggregated2) = c('person', 'day', 'person_day', 'new_group', 'kcal')


agg_wide <- aggregated2[!(is.na(aggregated2$person_day)),] %>%
  pivot_wider(id_cols = c('person', 'day', 'person_day'), names_from = new_group, values_from = kcal, names_glue = "{new_group}_kcal") 



######## Add in other variables
# Note: only nutrition data if the person is a food preparer

#### Guest data:
guest <- guest_repeat[, c("_submission__id", "_submission__submission_time", "guest_sex", "guest_age")] %>%
  group_by(`_submission__id`, `_submission__submission_time`) %>%
  mutate(guest_num = row_number()) %>%
  ungroup()

colnames(guest) <- c("person", "day", "sex", "age", "guest_num")

# Reshape the dataset into wide format
guest_wide <- guest %>%
  pivot_wider(
    names_from = guest_num, 
    values_from = c(age, sex),
    names_glue = "guest{guest_num}_{.value}"
  )

guest_wide$day <- as.Date(guest_wide$day)

###
add_dat <- agg_wide %>%
  left_join(main[,c("_id", "start", "_submission_time", "village", "hh_name", "list_member", "collection_wave")], by = c("person" = "_id")) %>%
  filter(as.Date(day) >= as.Date(start) & as.Date(day) <= as.Date(`_submission_time`))

add_dat2 <- add_dat %>%
  left_join(guest_wide, by = c("person", "day"))


add_dat3 <- add_dat2 %>%
  left_join(member[,c("_submission__id", "_submission__submission_time", "member", "food_preparer","special_day", "nb_people_eating", "member_eating", "guest", "nb_guests_eating")],
            by = c("person" = "_submission__id")) %>% 
            filter(day == as.Date(`_submission__submission_time`))

nutrition_data <- add_dat3 %>%
  select(person, day, person_day, village, hh_name, list_member, member, food_preparer, everything())



save(nutrition_data, file = here("NutritionComp", "nutrition_data.RData"))



### NOT DONE YET

#####################################################
# Look at added sugar/dairy in beverages
#####################################################
# Get list of  beverage files
indfiles_list_bev <- indfiles_list[grepl("beverage", indfiles_list)]
bevgroups = gsub("_repeat", "", indfiles_list_bev)

for (i in 1:length(indfiles_list_bev)){
  dat <- get(indfiles_list_bev[i])
  bevgroup = bevgroups[i]
  
  print(bevgroup)
  
  coff_tea <- dat[dat$selected %in% c("coffee", "tea"),]
  other <- dat[!(dat$selected %in% c("coffee", "tea")),]
  
  colnames(coff_tea) <- gsub(paste0(gsub("_outside", "", bevgroup),"_"), "", colnames(coff_tea))
  colnames(coff_tea) <- gsub("_outside", "", colnames(coff_tea))
  
  coff_tea$added_sugar <- ifelse(grepl("sugar", coff_tea$sugar_milk), "added sugar", NA)
  coff_tea$added_milk <- ifelse(grepl("milk", coff_tea$sugar_milk), "added milk", NA)
  
  #coff_tea <- coff_tea %>% select(-c("serving_size", "grams", "kcal_100g", "new_group", "old_group", "eng_name"))
  
  #coff_tea2 <- left_join(coff_tea, nutcomp_long[nutcomp_long$old_group=="added from beverage add ons",], by = c("added_sugar" = "varname", "sugar_serving" = "serving")) #%>% 
    #mutate(tot_kcal_addedsugar = serving_size * grams * kcal_100g/100)
  
  #coff_tea3 <- left_join(coff_tea2, nutcomp_long[nutcomp_long$old_group=="added from beverage add ons",], by = c("added_milk" = "varname", "milk_serving" = "serving")) #%>% 
    #mutate(tot_kcal_addedmilk = serving_size * grams * kcal_100g/100)
  
  #dat2 <- rbind(other, coff_tea3)
  
  #assign(indfiles_list_bev[i], dat2)
}

nutcomp_long[nutcomp_long$old_group=="added from beverage add ons",]



try <- left_join(beverage_repeat, nutcomp_long[nutcomp_long$old_group=="added from beverage add ons",], by = c("added_sugar" = "varname", "sugar_serving" = "serving")) #
