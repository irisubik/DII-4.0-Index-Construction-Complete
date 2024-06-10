# Previous Packages
# ANSWER NO IF ASKED!!!

# install.packages("writexl", type = "binary")
# install.packages("readxl", type = "binary")
# install.packages("tidyverse", type = "binary")
# install.packages("dplyr", type = "binary")
# install.packages("rqdatatable", type = "binary")
# install.packages("countrycode", type = "binary")
# install.packages("zoo", type = "binary")
# install.packages("imputeTS", type = "binary")
# install.packages("ggplot2", type = "binary")
# install.packages("rqdatatable")

# Load Packages
library(imputeTS)
library(rquery)
library(zoo)
library(tidyverse)
library(dplyr)
library(rqdatatable)
library(writexl)
library(countrycode)
# library(ggplot2)
library(rqdatatable)

##################################################################################################################################

# STEP 0.1: Create vector with variable names to be estimated

# This code turns the "unique" entries in the column "Code" of dataframe "Index_Rebuild" into a character vector
# Watch this object appear in the values pane

VARIABLES_TO_BE_ESTIMATED <- unique(Index_Rebuild$Code)

##################################################################################################################################

#filter the supply(or other specific pillar) indicators
first_nkeep_columns <- names(Ex_complete_database)[1:9]
first_nkeep_columns
# Combine first n columns with unique codes
columns_to_keep <- c(first_nkeep_columns, VARIABLES_TO_BE_ESTIMATED)

# Subset dataframe to keep only the specified columns
# Filter out any columns that might not exist in your_dataframe
columns_to_keep <- columns_to_keep[columns_to_keep %in% names(Ex_complete_database)]

# Create the new data frame
Ex_complete_database <- Ex_complete_database[, columns_to_keep]

# Filter the list to include only columns that exist in Ex_complete_database
VARIABLES_TO_BE_ESTIMATED <- intersect(VARIABLES_TO_BE_ESTIMATED, colnames(Ex_complete_database))

# Stineman's method, with NA in places where the linear interpolation (na.approx) resulted in NA. Essentially, 
# it's a safety check—if linear interpolation couldn't estimate a value (still NA), then the corresponding Stineman interpolated value is also set to NA.

##################################################################################################################################

# STEP 1: Stine Interpolation

Ex_Database_RTMI_1 <- Ex_complete_database %>%
  # order rows by country, then year
  arrange(Country, Year) %>%
  # treat each set of country matches as independent datasets
  group_by(Country) %>% 
  # stine interpolation on vector "VARIABLES_TO_BE_ESTIMATED"
  mutate_at(VARIABLES_TO_BE_ESTIMATED, funs(if(sum(!is.na(.))<2) {.} else{replace(na_interpolation(., option = "stine"), is.na(na.approx(., na.rm=FALSE)), NA)})) %>%
  # remove group restrictions for future functions
  ungroup() %>%
  arrange(desc(WB_Region), `Income Group 2022`, Country, Year)

##################################################################################################################################

# STEP 1.1: Define Dataset with interpolated intermediate values as CORE DATASET used for RTMI
# This step creates a dataset with only observed values and interpolated values from which to generate means for our RTMI rounds

RTMI_CORE_DATASET <- Ex_Database_RTMI_1

# STEP 1.2: Define indicator order
# Creates a vector with default order for indicators (will be used to rearrange indicator columns after RTMI rounds)

DATASET_INDICATOR_ORDER <- names(Ex_Database_RTMI_1)

################################################################################################################################

# STEP 2: FILL (LVCF) where applicable

Ex_Database_RTMI_2 <- Ex_Database_RTMI_1 %>%
  # order rows by country, then year
  arrange(Country, Year) %>%
  # treat each set of country matches as independent datasets
  group_by(Country) %>% 
  # Fill, first "down" from last value, then "up" from first value on vector "VARIABLES_TO_BE_ESTIMATED"
  fill(VARIABLES_TO_BE_ESTIMATED, .direction = "downup") %>%
  # remove group restrictions for future functions
  ungroup()%>%
  arrange(desc(WB_Region), `Income Group 2022`, Country, Year)

##################################################################################################################################


# STEP 3: RTMI Round 1: Income Group - Geographic WB_Region intersect

# 3.1: Create indicator means for each income group- WB_Region intersect group
# RTMI_IGGR: Income Group- Geographic WB_Region intersect

RTMI_IGGR <- RTMI_CORE_DATASET %>%
  # treat each set of income group-WB_Region-year matches as independent datasets
  group_by(WB_Region, `Income Group 2022`, Year) %>%
  # IF variable is numeric, reduce all rows to mean for each income group-WB_Region-year match
  summarise_if(is.numeric, function(x) mean(x, na.rm = T)) %>%
  # Remove group restrictions for future functions
  ungroup() %>%
  # replace NaN NA type with NA numeric (weird R neccessity)
  mutate_if(is.numeric, function(x) ifelse(is.nan(x), NA, x))

# "NATURAL JOIN" (Replace NA Values of DF1 with datapoints from DF2) for income group-WB_Region-year matches
# In other words, the dataset matches our missing value dataset (DF1) with our intersect mean dataset (DF2) ALONG income group-WB_Region-year row matches. 
# IF the row is a missing value, the natural join subs the NA for the intersect mean from DF2

Ex_Database_RTMI_3 <- natural_join(Ex_Database_RTMI_2, RTMI_IGGR, by = c("WB_Region", "Income Group 2022", "Year"), jointype = "FULL")

# Rearrange columns to default order, and arrange rows by listed sort hierarchy

Ex_Database_RTMI_3 <- Ex_Database_RTMI_3[DATASET_INDICATOR_ORDER] %>%
  arrange(desc(WB_Region), `Income Group 2022`, Country, Year)

##################################################################################################################################


# STEP 4: Stine Interpolation Round 2

Ex_Database_RTMI_4 <- Ex_Database_RTMI_3 %>%
  arrange(Country, Year) %>%
  group_by(Country) %>% 
  mutate_at(VARIABLES_TO_BE_ESTIMATED, funs(if(sum(!is.na(.))<2) {.} else{replace(na_interpolation(., option = "stine"), is.na(na.approx(., na.rm=FALSE)), NA)})) %>%
  ungroup() %>%
  arrange(desc(WB_Region), `Income Group 2022`, Country, Year)

#####################################################################################################################################

# STEP 5: FILL (LVCF) Round 2 where applicable

Ex_Database_RTMI_5 <- Ex_Database_RTMI_4 %>%
  arrange(Country, Year) %>%
  group_by(Country) %>% 
  fill(VARIABLES_TO_BE_ESTIMATED, .direction = "downup") %>%
  ungroup() %>%
  arrange(desc(WB_Region), `Income Group 2022`, Country, Year)

# Check nan values
total_na_values <- sum(is.na(Ex_Database_RTMI_5))
total_na_values