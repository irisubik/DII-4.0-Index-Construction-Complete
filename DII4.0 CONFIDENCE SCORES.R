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
# install.packages("data.table", type = "binary")

# Load Packages

library(rqdatatable)
library(tidyverse)
library(dplyr)
library(reshape2)
# library(ggplot2)
library(data.table)

rm(list=ls(pattern="^INDEX_CS_"))

#replace all existing values with score indicating data type

#check NA values for all RTMI files
# Create an empty vector to store the total NA values for each file
total_na_values <- numeric(7)

# Iterate through the files using a loop
for (i in 1:7) {
  # Get the RTMI file from the workspace
  file_name <- paste0("Ex_Database_RTMI_", i)
  data <- get(file_name)
  
  # Calculate the total NA values for the current file and store it in the vector
  total_na_values[i] <- sum(is.na(data))
}

# Print the total NA values for each file
total_na_values
################ CONFIDENCE SCORES ######################

## This method produces a "confidence score" for every single datapoint in our database. Observations get 
## a perfect score of 10. Scores decline as we proceed through successive rounds of estimation, following
## the RTMI principle that estimation accuracy decreases as we loosen intersect group restrictions for mean
## substituion. In short, some missingness is better than others. Better missingness (easier to estimate) gets
## a higher score. Yet another, simpler way to understand is that more accurate estimates get higher scores. The current
## schema allows for a maximum of 10 estimation rounds, but this can be increased by shifting each round score up an 
## integer for each additional estimation round required. Similarly, rounds can be decreased by shifting down score values.
## A final principal to understand is that the drop off in accuracy for each subsequent round of analysis is not neccessarily
## equivalent. It is the analysts final decision to code scores based on how accurate he/she believes the estimates
## produced in each round are.

## The script works by replacing non-NA values in each "round" dataset with confidence scores. Natural join is used to create a new dataset,
## mirroring the estimation rounds but with values indicating the round in which an estimate was produced rather than the value itself.

AIV <- unique(Index_Rebuild$Code)

Countries <- unique(Ex_complete_database$Country)

######### REPLACE NON-NA VALUES with 10 #########

INDEX_CS_10 <- Ex_complete_database %>%
  mutate_at(AIV, function(x) ifelse(!is.na(x), 10, x))

######### REPLACE NON-NA VALUES with 9 #########


INDEX_CS_9 <- Ex_Database_RTMI_1 %>%
  mutate_at(AIV, function(x) ifelse(!is.na(x), 9, x))

######### NATURAL JOIN 10-NON NA WITH 9 NON-NA #########


INDEX_CS <- natural_join(INDEX_CS_10, INDEX_CS_9, by = c("Country", "Year"), jointype = "FULL")

######### REPLACE NON-NA VALUES with 8 #########

INDEX_CS_8 <- Ex_Database_RTMI_2 %>%
  mutate_at(AIV, function(x) ifelse(!is.na(x), 8, x))

INDEX_CS <- natural_join(INDEX_CS, INDEX_CS_8, by = c("Country", "Year"), jointype = "FULL")

INDEX_CS <- INDEX_CS[DATASET_INDICATOR_ORDER] %>%
  arrange(desc(WB_Region), `Income Group 2022`, Country, Year)

######## REPLACE NON-NA VALUES with 7 #########


INDEX_CS_7 <- Ex_Database_RTMI_3 %>%
  mutate_at(AIV, function(x) ifelse(!is.na(x), 7, x))

INDEX_CS <- natural_join(INDEX_CS, INDEX_CS_7, by = c("Country", "Year"), jointype = "FULL")

######### REPLACE NON-NA VALUES with 6 ######### skip Ex_Database_RTMI_4 since it's same as Ex_Database_RTMI_3

INDEX_CS_6 <- Ex_Database_RTMI_4 %>%
  mutate_at(AIV, function(x) ifelse(!is.na(x), 6, x))

INDEX_CS <- natural_join(INDEX_CS, INDEX_CS_6, by = c("Country", "Year"), jointype = "FULL")


INDEX_CS <- INDEX_CS[DATASET_INDICATOR_ORDER] %>%
  arrange(desc(WB_Region), `Income Group 2022`, Country, Year)

######### REPLACE NON-NA VALUES with 5 #########

INDEX_CS_5 <- Ex_Database_RTMI_5 %>%
  mutate_at(AIV, function(x) ifelse(!is.na(x), 5, x))

INDEX_CS <- natural_join(INDEX_CS, INDEX_CS_5, by = c("Country", "Year"), jointype = "FULL")

INDEX_CS <- INDEX_CS[DATASET_INDICATOR_ORDER] %>%
  arrange(desc(WB_Region), `Income Group 2022`, Country, Year)

######### REPLACE NON-NA VALUES with 4 #########

INDEX_CS_4 <- Ex_Database_RTMI_6 %>%
  mutate_at(AIV, function(x) ifelse(!is.na(x), 4, x))

INDEX_CS <- natural_join(INDEX_CS, INDEX_CS_4, by = c("Country", "Year"), jointype = "FULL")

INDEX_CS <- INDEX_CS[DATASET_INDICATOR_ORDER] %>%
  arrange(desc(WB_Region), `Income Group 2022`, Country, Year)

######### REPLACE NON-NA VALUES with 3 #########

INDEX_CS_3 <- Ex_Database_RTMI_7 %>%
  mutate_at(AIV, function(x) ifelse(!is.na(x), 3, x))

INDEX_CS <- natural_join(INDEX_CS, INDEX_CS_3, by = c("Country", "Year"), jointype = "FULL")

INDEX_CS <- INDEX_CS[DATASET_INDICATOR_ORDER] %>%
  arrange(desc(WB_Region), `Income Group 2022`, Country, Year)

# ######### REPLACE NON-NA VALUES with 2 #########

INDEX_CS_2 <- Ex_Database_RTMI_8 %>%
  mutate_at(AIV, function(x) ifelse(!is.na(x), 2, x))

INDEX_CS <- natural_join(INDEX_CS, INDEX_CS_2, by = c("Country", "Year"), jointype = "FULL")

INDEX_CS <- INDEX_CS[DATASET_INDICATOR_ORDER] %>%
  arrange(desc(WB_Region), `Income Group 2022`, Country, Year)


INDEX_CS_125Country <- INDEX_CS %>%
  filter(Country %in% DII_Country$Country)
  # filter(Country != "HKG") %>%
  # filter(Country != "SGP")
  

total_na_values <- sum(is.na(INDEX_CS_125Country))
total_na_values
# Convert to Long

INDEX_CS_LONG <- melt(INDEX_CS_125Country, id = (c("Country", "CountryName","WB_Region", "MC_Region", "Income Group 2022", "Year", "SubRegion", "Income Group Num", "LGINC")))

# Define the specific countries and indicators to skip, since we manullay input the values, so skip those when cal CS
countries_to_skip <- c("HKG", "SGP")
indicators_to_skip <- c('dispi_rural','ibuy_rural','ipay_rural','mdply_rural','acnt_rural', 'rdply_rural')  # Replace with your actual indicator names

# Skip specific countries and indicators
INDEX_CS_LONG_FILTERED <- INDEX_CS_LONG %>%
  filter(!(Country %in% countries_to_skip & variable %in% indicators_to_skip))


INDEX_K_CS_LONG <- INDEX_CS_LONG_FILTERED %>%
  rename(Code = variable, CS = value) %>%
  arrange(Country, Code, Year) %>%
  mutate(Country = as.character(Country)) %>%
  mutate(Year = as.character(Year)) %>%
  mutate(Year = as.numeric(Year))

##### Produces Confidence Scores for Indicators

INDEX_K_CS_LONG_CODE <- INDEX_K_CS_LONG %>%
  mutate(CS = as.numeric(CS)) %>% 
  group_by(Code) %>%
  summarise(CS = mean(CS) / 10) %>%
  filter(Code %in% AIV) %>%
  ungroup()

INDEX_Code_CS <- INDEX_K_CS_LONG_CODE

#### Produces Confidence Scores for Countries ###########

INDEX_K_CS_LONG_ISO3C <- INDEX_K_CS_LONG %>%
  filter(Code %in% AIV) %>%
  group_by(Country) %>%
  summarise(CS = mean(CS) / 10) %>%
  ungroup()

write_xlsx(INDEX_Code_CS, paste0("Database Prints/IndicatorCode_CS_weights", Sys.Date(), ".xlsx"), format_headers = F)

# ###################################################################################################################################################
# 
# ######################################## ESTIMATION HEATMAPS ######################################################################################
# 
# ###################################################################################################################################################
# 
# 
# #Generate Missmap by Indicator
# 
# ## ################ highlight below ############# ##
# 
# INDEX_CS_WIDE <- INDEX_K_CS_LONG %>% mutate(CS = factor(CS))
# 
# INDEX_CS_WIDE_IND <- reshape2::dcast(INDEX_CS_WIDE, Country + Year ~ Code) %>%
#   mutate_at(AIV, function(x) factor(x, levels = sort(unique(as.numeric(x)), decreasing = T)))
# 
# 
# heatmap <- function(df, .x_var, .y_var, .z_var) {
#   
#   x_var <- sym(.x_var)
#   y_var <- sym(.y_var)
#   z_var <- sym(.z_var)
#   
#   ggplot(df, aes_string(x = x_var, y = y_var, fill = z_var)) + 
#     geom_tile(colour = "black") +
#     scale_color_viridis_d() +
#     scale_x_continuous(aes(min(Year):max(Year), breaks = as.character(c(min(Year):max(Year))), labels = as.character(c(min(Year):max(Year))))) +
#     theme(axis.text.y = element_text(size = 6)) +
#     ylab("Country")
#   ggsave(filename = paste(z_var, ".png", sep = ""), height = 10, width = 6, dpi = 300, path = "Heatmaps/Indicators")
# }
# 
# dir.create(file.path("Heatmaps","Indicators"), recursive = T)
# 
# INDEX_CS_heatmap_list <- AIV %>%
#   map( ~ heatmap(INDEX_CS_WIDE_IND, "Year", "Country" , .x ))
# 
# 
# ## ^^^^^^^ Highlight Above ^^^^^^^^^^^^^^^ ##
# 
# # Generate CS Heatmap by Country
# 
# ## ################ highlight below ############# ##
# 
# INDEX_CS_WIDE_ISO3C <- reshape2::dcast(INDEX_CS_WIDE, Code + Year ~ Country) 
# 
# heatmap <- function(df, .x_var, .y_var, .z_var) {
#   
#   x_var <- sym(.x_var)
#   y_var <- sym(.y_var)
#   z_var <- sym(.z_var)
#   
#   ggplot(df, aes_string(x = x_var, y = y_var, fill = z_var)) + 
#     geom_tile(colour = "black") +
#     scale_color_viridis_d() +
#     scale_x_continuous(aes(min(Year):max(Year), breaks = as.character(c(min(Year):max(Year))), labels = as.character(c(min(Year):max(Year))))) +
#     theme(axis.text.y = element_text(size = 6)) +
#     ylab("Variable")
#   ggsave(filename = paste(z_var, ".png", sep = ""), height = 10, width = 6, dpi = 300, path = "Heatmaps/Countries")
# }
# 
# dir.create(file.path("Heatmaps","Countries"), recursive = T)
# 
# INDEX_CS_heatmap_list <- Countries %>%
#   map( ~ heatmap(INDEX_CS_WIDE_ISO3C, "Year", "Code" , .x ))
# 
# ## ^^^^^^^ Highlight Above ^^^^^^^^^^^^^^^ ##
# 
# rm(list=ls(pattern="^INDEX_CS"))
# rm(list=ls(pattern="^INDEX_K"))

