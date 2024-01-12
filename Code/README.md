# Code Documentation

## Code relevant to the HH Data

* _HH_processing.R_: R code that reads in the original data files, processes them, and saves them as RData files. This code can be updated when further data is collected.
* _HH_utils.R_: R code for the two functions I use in data processing. One reads the multi-sheet Excel documents, and the other function identifies the data contained in the various Excel sheets, aggregates them by topic (i.e., main data, income, etc), and harmonizes the data types.
  * PLEASE NOTE: this code requires an __accurate__ and up to date data dictionary, like HH_DataDictionary (under the Data Dictionary folder) to inform it what data type each variable should be. Inaccuries in the data type listed in the data dictionary can lead to erros in the data processing.
