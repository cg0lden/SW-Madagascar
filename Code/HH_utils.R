################################################################################
# Code: Import HH Data
# Date: 1/4/2024
#
# Programmer: Emma Crenshaw
#
# Purpose:
#   This code contains functions to import the household-level data for the 
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
  ### Define 'hallmark' variables - i.e., variables only contained in that dataset that identify what it is
  ### absent_repeat : "reason_abs_member"
  ### income_repeat: "income_selected"
  ### member_left_repeat: "name_left_member"
  ### member_new_repeat: "name_new_member"
  ### animal_repeat: "ot_animal"
  ### salary_repeat: "salary_selected"
  ### crops_repeat: "selected_crops"
  ### wage_repeat: "wage_selected"
  ### main: "gps_hh"
  
  # Create true/false variables that identify which of the hallmark variables they contain
  df_sets3 <- df_sets2 %>% mutate(absent_repeat = grepl("reason_abs_member", df_sets2$vars), 
                                  income_repeat = grepl("income_selected", df_sets2$vars),
                                  member_left_repeat = grepl("name_left_member", df_sets2$vars),
                                  member_new_repeat = grepl("name_new_member", df_sets2$vars),
                                  animal_repeat = grepl("ot_animal", df_sets2$vars),
                                  salary_repeat = grepl("salary_selected", df_sets2$vars),
                                  crops_repeat = grepl("selected_crops", df_sets2$vars),
                                  wage_repeat = grepl("wage_selected", df_sets2$vars),
                                  main = grepl("gps_hh", df_sets2$vars)) %>%
    mutate(sum_ds = absent_repeat + income_repeat + member_left_repeat +
             member_new_repeat + animal_repeat + salary_repeat +
             crops_repeat + wage_repeat + main) #sum to make sure the variable is unique (these should all equal 1)
  
  
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
          dataset[[var_name]] <- NA
          #print(var_name) # if you want to check which variable some of these are missing
        }
        
        # Use eval and parse to dynamically set the variable type in data
        if (var_type == "datetime"){dataset[[var_name]] <- eval(parse(text =paste0("as_",var_type,"(dataset[[var_name]])")))}
        else {dataset[[var_name]] <- eval(parse(text =paste0("as.",var_type,"(dataset[[var_name]])")))}
        
      }
      return(dataset)
    }) %>%
      bind_rows() 
  }
  
  # name the new datasets with they key name
  names(fix_vartype) = paste0(timepoint, "_", key_vars)
  
  
  return(fix_vartype)
}
