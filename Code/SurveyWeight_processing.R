################################################################################
# Code: SurveyWeight_processing
# Date: 1/22/2024
#
# Programmer: Emma Crenshaw
#
# Purpose: Merge in sample weights for households
#   
#
# Input: 8 sheets containing information on farmer/nonfarmer and fisher/nonfisher
#         households. Additionally, Heather sent weights for farmer/nonfarmers
#   
#
# Output: Data sheet combining the household names/dates with weights
#   
#     
#---------------------------------------------------------------------
# Notes:
#   - 
#
################################################################################

# Import packages
library(tidyverse)
library(readxl)
library(stringr)
library(readr)
library(here)

#######################################################################

######################################
# Import data
######################################
weights <- readxl::read_excel(here("SurveyWeights", "ARMS sampling weights.xlsx")) # sampling weights for fisher/nonfishers

fisher_L5 <- read.table(here("SurveyWeights", "fisher_less5.csv"), sep = ";", header = T)
fisher_NL5 <- read.table(here("SurveyWeights", "fisher_nonless5.csv"), sep = ";", header = T)
nonfisher_L5 <- read.table(here("SurveyWeights", "nonfisher_less5.csv"), sep = ";", header = T)

# Note - this file wasn't reading in properly. I had to add an EOL and then fix all the quotations it read in with
nonfisher_NL5 <- read_csv2(here("SurveyWeights", "nonfisher_nonless5.csv"), quote="")
names(nonfisher_NL5) <- gsub('"', '',names(nonfisher_NL5))

for (i in 1:nrow(nonfisher_NL5)){
  for (j in c(1,2,3,5,6)){
    nonfisher_NL5[i,j] <- gsub('"', '',nonfisher_NL5[i,j])
  }
}

farmer_L5 <- read.table(here("SurveyWeights", "farmer_less5.csv"), sep = ";", header = T)
farmer_NL5 <- read.table(here("SurveyWeights", "farmer_nonless5.csv"), sep = ";", header = T)
nonfarmer_L5 <- read.table(here("SurveyWeights", "nonfarmer_less5.csv"), sep = ";", header = T)
nonfarmer_NL5 <- read.table(here("SurveyWeights", "nonfarmer_nonless5.csv"), sep = ";", header = T)


######################################
# Prepare to merge
######################################

# fix variables names in weight dataset
weights_fix = rename(weights, "num_household_total" = "# household total", "num_households_sampled" = "# households sampled",
                     "perc_sample_of_category" = "% of sample of each category", "village" = "Village")
colnames(weights_fix)[1] <- 'WEIGHTS'

# Add weights for farmers/nonfarmers
weights_farmers <- data.frame(WEIGHTS = c("Farmer_Child5", "Farmer_NoChild5", "NoFarmer_Child5", "NoFarmer_NoChild5",
                                          "Farmer_Child5", "Farmer_NoChild5", "NoFarmer_Child5", "NoFarmer_NoChild5"), 
                              num_household_total = c(249, 249, 6, 6,
                                                      288, 288, 65, 65), 
                              num_households_sampled = c(39, 39, 6, 6, 
                                                         32, 32, 13, 13),
                              village = c("Akatrakatraky", "Akatrakatraky", "Akatrakatraky", "Akatrakatraky",
                                          "Ranobe", "Ranobe", "Ranobe", "Ranobe"))

weights_farmers$perc_sample_of_category <- weights_farmers$num_households_sampled/weights_farmers$num_household_total * 100

all_weights =  rbind(weights_fix, weights_farmers)

# Add an indicator of weight type
fisher_L5$WEIGHTS <- "Fisher_Child5"
fisher_NL5$WEIGHTS <- "Fisher_NoChild5"
nonfisher_L5$WEIGHTS <- "NoFisher_Child5"
nonfisher_NL5$WEIGHTS <- "NoFisher_NoChild5"

farmer_L5$WEIGHTS <- "Farmer_Child5"
farmer_NL5$WEIGHTS <- "Farmer_NoChild5"
nonfarmer_L5$WEIGHTS <- "NoFarmer_Child5"
nonfarmer_NL5$WEIGHTS <- "NoFarmer_NoChild5"

nonfisher_NL5$index <- as.numeric(nonfisher_NL5$index)
farmer_L5$index <- as.numeric(farmer_L5$index)

# Combine all types of weights into a single data set
all_types <- bind_rows(fisher_L5, fisher_NL5, nonfisher_L5, nonfisher_NL5, farmer_L5, 
                       farmer_NL5, nonfarmer_L5, nonfarmer_NL5)

######################################
# Merge
######################################

weight_data <- left_join(all_types, weights_fix, by = c("WEIGHTS" = "WEIGHTS", "village" = "village"))

# Remove the ID/index variables 
weight_data$index <- NULL

# Get unique rows
weight_data_unique <- unique(weight_data)

# Look at the rows that were removed because they are duplicates (once you ignore ID)
duplicated <- weight_data[duplicated(weight_data),]

weight_data[weight_data$label_hoh=="safidy_olasoa_m_1990",]
weight_data[weight_data$label_hoh=="patrick_m_1993",]
weight_data[weight_data$label_hoh=="sambiainy_besofy_m_1968",]

######################################
# Merge with household data
######################################

load("~/Desktop/ARMS analysis/SW-Madagascar/HHData/main.RData")

# ensure the villages have the same capitalization
main$village_merge = tolower(main$village)
weight_data_unique$village_merge = tolower(weight_data_unique$village)

# merge together
merged_data <- merge(weight_data_unique, main, by.x = c("label_hoh", "village_merge"), by.y = c("hh_name","village_merge")) 

# remove the majority of variables from main.RData
merged_data_res <- merged_data %>% 
  select("WEIGHTS", "village_merge", "label_hoh", "num_household_total", "num_households_sampled", "perc_sample_of_category", "date")

######################################
# Sanity Checks
######################################

# check the number of villages (14 in total)
length(unique(merged_data$village.x))

excluded = unique(main$hh_name[!(main$hh_name %in% merged_data$label_hoh)])
excluded #it's zero, that's great

######################################
# Output data
######################################

save(merged_data_res, file = here("SurveyWeights", "survey_weights.RData"))
