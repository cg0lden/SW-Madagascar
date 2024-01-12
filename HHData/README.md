# HHData Documentation

This file contains basic information for understanding the household-level data of the ARMS Study.

## Raw Data

### Data sets
There are 9 data sets for the aggregated raw HH data, which combine data from four collection waves and multiple data managers. The data has been split into data which has one response per participant per data collection time point, and 8 data sets which contain information from the repeating sections of the survey.

* _main_: this contains the bulk of the HH data and consists of the data that has one response per participant per data collection wave
* _absent_repeat_: data the respondent reported on one (or more) absent household members
* _member_new_repeat_: data that the respondent reported on one (or more) new household members
* _member_left_repeat_: data that the respondent reported on one (or more) household members who have left
* _animal_repeat_: data that the respondent reported on one (or more) domesticated animals not otherwise covered in the survey
* _crops_repeat_: data that the respondent reported on one (or more) crops not otherwise covered in the suvey
* _income_repeat_: data that the respondent reported on one (or more) sources of income
* _salary_repeat_: data the respondent reported on one (or more) types of salaried labor. This section of the survey is a repeated loop
* _wage_repeat_: data the respondent reported on one (or more) types of wage labor

The data is available in its original form in the folder "Raw", separated by data collection wave.

### Data Linkage
The main dataset and repeated datasets can be linked together via the respondent's ID number, which is "_id" in the main data and "_submission__id" in the repeated data. Data between waves can be identified by the date part of "_submission_time" in the main data and "_submission_submission__time" in the repeated data.

### Data Idiosyncracies
I discovered a few things in the data that should be noted:
* The variable 'mesurement' in the crops_repeated data is only available for data collected during or after April 2023
* Several metadata variables are made meaningless by the data aggregation. Namely, the variables "index" and "parent_index" refer to row numbers in the original raw data, so they are meaningless in the combined data.
