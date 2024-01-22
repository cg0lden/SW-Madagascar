################################################################################
# Code: Ind_utils
# Date: 1/12/2024
#
# Programmer: Emma Crenshaw
#
# Purpose:
#   This code contains functions to import the individual-level data for the 
#   ARMS study and combines data from different time points and data managers
#
#
#---------------------------------------------------------------------
# Notes:
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



# Create a function to read in excel document with multiple sheets
multiplesheets <- function(fname) { 
  
  # getting info about all excel sheets 
  sheets <- readxl::excel_sheets(fname) 
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x)) 
  data_frame <- lapply(tibble, as.data.frame) 
  
  # assigning names to data frames 
  names(data_frame) <- sheets 
  return(list(data_frame, sheets)) # return the datasets and also a list of sheet names
} 


load_ARMSData <- function(dd_file, list_files, timepoint){
  ###################################################
  # Step 1: Import Data
  
  
  # Import data dictionary
  dd_hh <- multiplesheets(dd_file)
  sheets_dd <- dd_hh[[2]]
  ds_dd <- dd_hh[[1]]
  
  # Import all of the datasets and sheets
  comb_sheets = c()
  comb_ds = c()
  for(i in 1:length(list_files)){
    excel_dat <- multiplesheets(list_files[i])
    sheets <- excel_dat[[2]]
    ds <- excel_dat[[1]]
    
    # Combine all sheets/datasets into lists
    comb_sheets = c(comb_sheets, sheets)
    comb_ds = c(comb_ds, ds)
  }
  
  ###################################################
  # Step 2: Figure out which sheets contain the same variables
  
  # Variables in each dataset
  variable_sets <- lapply(comb_ds, function(x) colnames(x))
  
  # above as a dataset
  df_sets <- data.frame(df = names(variable_sets), vars = sapply(variable_sets, function(x) paste(x, collapse=", ")))
  df_sets2 <- df_sets[order(df_sets$vars),] # Order by variables contained - necessary so that we define groups of datasets by when the variables change
  
  # Define groups of data sets by variables
  ## Define 'hallmark' variables - i.e., variables only contained in that dataset that identify what it is
  ## Create true/false variables that identify which of the hallmark variables they contain
  ## Use regex "\\b" to indicate start/end of word so that bread_selected isn't indicated for bread_selected_outside (etc)
  df_sets3 <- df_sets2 %>% mutate(beans_sauce_repeat = grepl("beans_sauce_selected", df_sets2$vars),
                                  beans_repeat = grepl("\\bbeans_selected\\b", df_sets2$vars),
                                  beans_outside_repeat = grepl("beans_selected_outside", df_sets2$vars),
                                  beverage_repeat = grepl("\\bbeverage_selected\\b", df_sets2$vars),
                                  beverage_outside_repeat = grepl("beverage_selected_outside", df_sets2$vars),
                                  bread_repeat = grepl("\\bbread_selected\\b", df_sets2$vars),
                                  bread_outside_repeat = grepl("bread_selected_outside", df_sets2$vars),
                                  condiment_repeat = grepl("condiment_selected", df_sets2$vars),
                                  corn_sauce_repeat = grepl("corns_sauce_selected", df_sets2$vars),
                                  dairy_repeat = grepl("\\bdiary_selected\\b", df_sets2$vars),
                                  dairy_outside_repeat = grepl("diary_selected_outside", df_sets2$vars),
                                  fat_food_repeat = grepl("fat_food_selected", df_sets2$vars),
                                  fish_sauce_repeat = grepl("fish_sauce_selected", df_sets2$vars),
                                  fish_repeat = grepl("\\bfish_selected\\b", df_sets2$vars),
                                  fish_outside_repeat = grepl("fish_selected_outside", df_sets2$vars),
                                  fishing_area_repeat = grepl("fishing_area", df_sets2$vars),
                                  fruits_repeat = grepl("\\bfruits_selected\\b", df_sets2$vars),
                                  fruits_outside_repeat = grepl("fruits_selected_outside", df_sets2$vars),
                                  greens_sauce_repeat = grepl("greens_sauce_selected", df_sets2$vars),
                                  greens_repeat = grepl("\\bgreens_selected\\b", df_sets2$vars),
                                  greens_outside_repeat = grepl("greens_selected_outside", df_sets2$vars),
                                  guest_repeat = grepl("guest_sex", df_sets2$vars),
                                  marine_inverts_sauce_repeat = grepl("marine_inverts_sauce_selected", df_sets2$vars),
                                  marine_inverts_repeat = grepl("\\bmarine_inverts_selected\\b", df_sets2$vars),
                                  marine_inverts_outside_repeat = grepl("marine_inverts_selected_outside", df_sets2$vars),
                                  meat_sauce_repeat = grepl("meat_sauce_selected", df_sets2$vars),
                                  meat_repeat = grepl("\\bmeat_selected\\b", df_sets2$vars),
                                  meat_outside_repeat = grepl("meat_selected_outside", df_sets2$vars),
                                  member = grepl("death_cause", df_sets2$vars),
                                  occupation_repeat = grepl("occupation_selected", df_sets2$vars),
                                  rice_sauce_repeat = grepl("rice_sauce_selected", df_sets2$vars),
                                  snacks_repeat = grepl("\\bsnacks_selected\\b", df_sets2$vars),
                                  snacks_outside_repeat = grepl("snacks_selected_outside", df_sets2$vars),
                                  main = grepl("today", df_sets2$vars),
                                  tubers_repeat = grepl("\\btype_tubers\\b", df_sets2$vars),
                                  tubers_sauce_repeat = grepl("tubers_sauce_selected", df_sets2$vars),
                                  tubers_outside_repeat = grepl("type_tubers_outside", df_sets2$vars),
                                  vegetables_sauce_repeat = grepl("vegetables_sauce_selected", df_sets2$vars),
                                  vegetables_repeat = grepl("\\bvegetables_selected\\b", df_sets2$vars),
                                  vegetables_outside_repeat = grepl("vegetables_selected_outside", df_sets2$vars)) %>%
    mutate(sum_ds = beans_sauce_repeat + beans_repeat + beans_outside_repeat + beverage_repeat + beverage_outside_repeat + bread_repeat + 
           bread_outside_repeat + condiment_repeat + corn_sauce_repeat + dairy_repeat + dairy_outside_repeat  + 
           fat_food_repeat + fish_sauce_repeat + fish_repeat + fish_outside_repeat + fishing_area_repeat + 
           fruits_repeat + fruits_outside_repeat + greens_sauce_repeat + greens_repeat + greens_outside_repeat + guest_repeat + 
           marine_inverts_sauce_repeat + marine_inverts_repeat + marine_inverts_outside_repeat +  meat_sauce_repeat + meat_repeat + meat_outside_repeat +
           member + occupation_repeat + rice_sauce_repeat + snacks_repeat + snacks_outside_repeat + 
           main + tubers_repeat + tubers_sauce_repeat + tubers_outside_repeat + vegetables_sauce_repeat  + 
           vegetables_repeat + vegetables_outside_repeat
    ) #sum to make sure the variable is unique (these should all equal 1)
  # Check the sum variable
  if (length(df_sets3$vars[df_sets3$sum_ds != 1]) != 0) {
    stop("Uncategorized excel sheet")
  }
  

  
  # Create a loop that uses the variable sets and hallmark variables to define "groups" of datasets all all contain the same variables and should be stacked
  string_groups <- list()
  key_vars <- list()
  group_num = 1
  group = df_sets2$df[1]
  group_var <- df_sets2$vars[1]
  key_var <- colnames(df_sets3)[apply(df_sets3[1,1:ncol(df_sets3)-1] == TRUE, 2, any)] # get the key variable that is 'true' (but exclude the sum column; value = 1 shows up as true)
  for (i in 1:length(df_sets2$df)){
    if(group_var == df_sets2$vars[i]){
      group <- c(group, df_sets2$df[i])
    }
    else{
      string_groups[[group_num]] <- group
      key_vars[[group_num]] <- key_var
      
      group <- df_sets2$df[i]
      group_var <- df_sets2$vars[i]
      group_num <- group_num + 1
      key_var <- colnames(df_sets3)[apply(df_sets3[i,1:ncol(df_sets3)-1] == TRUE, 2, any)]
    }
    string_groups[[group_num]] <- group
    key_vars[[group_num]] <- key_var
  }
  
  ###################################################
  # Step 3: Harmonize variable types and stack data
  
  # before datasets can be stacked, their variables have to have all the correct type (i.e., can't stack a character variable on a date time)
  ## issue seems to come up mostly for date time variables that are imported as character and for character variables that are all missing and therefore defined as numeric
  ## easiest to define the type of variable something should be
  
  fix_vartype <- list()
  for (i in 1:length(key_vars)){
    fix_vartype[[i]] <- lapply(string_groups[[i]], function(dataset_name) {
      dataset <- get(dataset_name, comb_ds)  # Access the dataset by name
      
      dict <- get(key_vars[[i]], ds_dd)
      
      for (j in 1:nrow(dict)) {
        var_name <- dict$name[j]
        var_type <- dict$type[j]
        
        if (!any(names(dataset) == var_name)){
          # fix the name changes for the blood pressure measurements
          if(var_name == "blood_pressure_sys"){
            if(any(colnames(dataset) == "sys_blood_pressures")){
              colnames(dataset)[colnames(dataset)=="sys_blood_pressures"] <- "blood_pressure_sys" 
            }
            else{
              dataset$blood_pressure_sys <- NA
            }
          }
          else if(var_name == "blood_pressure_dia"){
            if(any(colnames(dataset) == "dia_blood_pressures")){
              colnames(dataset)[colnames(dataset)=="dia_blood_pressures"] <- "blood_pressure_dia"
            }
            else{
              dataset$blood_pressure_dia <- NA
            }
          }
          else{
            dataset[[var_name]] <- NA
            #print(var_name) # if you want to check which variables are missing from data sheets
          }
        }
        
        na_before = sum(is.na(dataset[[var_name]])) # check number of NA values before variable type change
        
        # Use eval and parse to dynamically set the variable type in data
        if (var_type == "datetime"){dataset[[var_name]] <- eval(parse(text =paste0("as_",var_type,"(dataset[[var_name]])")))}
        else {dataset[[var_name]] <- eval(parse(text =paste0("as.",var_type,"(dataset[[var_name]])")))}
        
        na_after = sum(is.na(dataset[[var_name]])) # check number of NA values after variable type change
        
        if(na_before < na_after){ #if the variable type change created new NA values, print the data sheet and variable
          print("New Coerced NA values")
          print(var_name)
          print(dataset_name)
          print(var_type)
        }
      }
      return(dataset)
    }) %>%
      bind_rows() 
  }
  
  # name the new datasets with they key name
  names(fix_vartype) = paste0(timepoint, "_", key_vars)
  
  
  return(fix_vartype)
}
