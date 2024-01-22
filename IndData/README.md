# IndData Documentation

This file contains basic information for understanding the individual-level data of the ARMS Study.

## Raw Data

### Data sets
There are multiple data sets for the aggregated raw individual data, which combine data from four collection waves and multiple data managers. The data has been split into data which has one response per participant per data collection time point and data sets which contain information from the repeating sections of the survey.

* _main_: this contains the administrative data for the individual level survey, like the village
* _member_: this contains the bulk of the data from the individual-level survey
* Data sets for repeating sections of the data

The data is available in its original form in the folder "Raw", separated by data collection wave.

### Data Linkage
The main dataset and repeated datasets can be linked together via the respondent's ID number, which is "_id" in the main data and "_submission__id" in the repeated data. Data between waves can be identified by the date part of "_submission_time" in the main data and "_submission_submission__time" in the repeated data.

### Data Idiosyncracies
I discovered a few things in the data that should be noted:
* The variable 'blood_pressure' in the _member_ dataset was only collected a few times in the first wave of data collection before the variable was switched to 'sys_blood_pressure' and 'dia_blood_pressure'. These variables were renamed for data collection waves 3 and 4 to 'blood_pressure_sys' and 'blood_pressure_dia'. The variables have been named in the aggregated data according to the later naming convention.
* Several metadata variables are made meaningless by the data aggregation. Namely, the variables "index" and "parent_index" refer to row numbers in the original raw data, so they are meaningless in the combined data.
