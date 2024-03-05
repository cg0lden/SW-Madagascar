# Code Documentation

## Code relevant to the HH Data

* _HH_processing.R_: R code that reads in the original data files, processes them, and saves them as RData files. This code can be updated when further data is collected.
* _HH_utils.R_: R code for the two functions I use in data processing. One reads the multi-sheet Excel documents, and the other function identifies the data contained in the various Excel sheets, aggregates them by topic (i.e., main data, income, etc), and harmonizes the data types.
  * PLEASE NOTE: this code requires an __accurate__ and up to date data dictionary, like HH_DataDictionary (under the Data Dictionary folder) to inform it what data type each variable should be. Inaccuracies in the data type listed in the data dictionary can lead to errors in the data processing.

## Code relevant to the individual data
* _Ind_processing.R_: R code that reads in the original data files, processes them, and saves them as RData files. This code can be updated when further data is collected.
* _Ind_utils.R_: R code for the two functions I use in data processing. One reads the multi-sheet Excel documents, and the other function identifies the data contained in the various Excel sheets, aggregates them by topic (i.e., main data, member data, etc), and harmonizes the data types.
  * PLEASE NOTE: this code requires an __accurate__ and up to date data dictionary, like Ind_DataDictionary (under the Data Dictionary folder) to inform it what data type each variable should be. Inaccuracies in the data type listed in the data dictionary can lead to errors in the data processing.
  
## Code for survey weights

* _SurveyWeight_processing.R_: code that reads in lists of each household determined to be farmer/nonfarmer/fisher/nonfisher, add the survey weights, and merge those onto the main data. This code outputs a dataset that can be merged onto the main.RData household data file by "hh_name" and "date"


## Notes
There are a few errors/warnings that you should pay attention to while using this code:
* "In eval(parse(text = paste0("as.", var_type, "(dataset[[var_name]])"))) :
  NAs introduced by coercion" - this indicates that the data type for some variable in the data dictionary is incompatible with the data and is coercing the data to be missing. This should be fixed by changing the data type listed in the data dictionary
* "Uncategorized excel sheet" - this is a custom warning I created to indicate that one of the data sheets in the Excel document is not an identified type of data sheet (i.e., it doesn't include variables I have identified as characterizing a specific part of the survey). This is likely due to adding new variables or repeating sections to the survey. To remedy this, you should update the data dictionary with the new survey section and create a new survey section variable using regular expressions in the Ind_utils.R code where the "df_sets3" dataset is created (around line 87).
