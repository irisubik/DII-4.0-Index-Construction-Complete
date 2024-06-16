# Previous Packages

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
# install.packages("scales", type = "binary")

# New Packages

# install.packages("tidyselect")

# Load Packages

library(tidyverse)
library(dplyr)
library(zoo)
library(ggplot2)
# library(ggrepel)
library(readxl)
library(writexl)
library(countrycode)
library(reshape2)
library(scales)
library(rqdatatable)
library(tidyselect)

### Define Useful functions used later #####


mult15add50 <- function(x) (x*15 + 50)
stretch3add50 <- function(x) (x*3 + 50)
subtract50divide3 <- function(x) ((x-50)/3)
round2 <- function(x) round(X, digits = 2)

#### Define useful vectors used later ######

Drivers_Vector <- unique(Ex_Database_RTMI_LONG_Drivers$Driver)
Components_Vector <- unique(Ex_Database_RTMI_LONG_Components$Component)
Clusters_Vector <- unique(Ex_Database_RTMI_LONG_Clusters$Cluster)
Annual_Values_Vector <- c(Drivers_Vector, Components_Vector, Clusters_Vector)

### Transform long cluster, component, and final score sheets into wide format dataframes, and merge ###

Ex_INDEX_WIDE_Clusters <- dcast(Ex_Database_RTMI_LONG_Clusters[c("Country", "Year", "Cluster", "value")], Country + Year ~ Cluster)
Ex_INDEX_WIDE_Components <- dcast(Ex_Database_RTMI_LONG_Components[c("Country", "Year", "Component", "value")], Country + Year ~ Component)
Ex_INDEX_WIDE_Drivers <- dcast(Ex_Database_RTMI_LONG_Drivers[c("Country", "Year", "Driver", "value")], Country + Year ~ Driver)

Ex_INDEX_EP_Combined <- merge(Ex_INDEX_WIDE_Components, Ex_INDEX_WIDE_Clusters, by = c("Country", "Year"), all = T)
Ex_INDEX_EP_Combined <- merge(Ex_INDEX_WIDE_Drivers, Ex_INDEX_EP_Combined, by = c("Country", "Year"), all = T)

##########################################################################################################################################

################################### Calculating Momentum and Ranks #######################################################################

##########################################################################################################################################


############ This block of code generates momentum columns


Ex_INDEX_EP_Combined <- Ex_INDEX_EP_Combined %>%
  arrange(Country, Year) %>%
  # Group by country (treat each country group as independent dataframe)
  group_by(Country) %>%
  # Apply following function to each variable listed in Annual_Values_Vector: Multiply all values by 3, and add 50 (See discussion on momentum for why)
  mutate_at(Annual_Values_Vector, stretch3add50) %>%
  # create "time elapsed in years" column for CAGR calculations
  mutate(t = Year - 2009) %>%
  # Apply following function to each variable listed in Annual_Values_Vector: CAGR using a 2011/2012 averaged base year period
  # NOTE: 2012 in this sheet is the SECOND year of time series. Make adjustments for future indices accordingly
  ## Beautiful function: this construction creates new columns that end with "Momentum". See function name following funs()
  mutate_at(Annual_Values_Vector, .funs = funs(Momentum = (((. / (rollmean(.,2,fill=0,align="right"))[Year == 2009])^(1/t)) - 1) * 100)) %>%
  # Remove group restrictions for future functions
  ungroup() %>%
  # Remove "time elapsed" column
  select(-t) %>%
  # Apply following function to each variable listed in Annual_Values_Vector: subtract 50, divide by 3 (see discussion on momentum for why)
  mutate_at(Annual_Values_Vector, subtract50divide3) %>%
  # Group by country (treat each country group as independent dataframe)
  group_by(Country) %>%
  # Apply following function to each variable containing the character string "Momentum": Recode two-year base period momentum in 2011/2012 to first CAGR year 2013
  # See discussion on momentum for why
  mutate_at(vars(contains("Momentum")), funs(case_when(Year %in% 2008:2010 ~ (.[Year == 2010]), TRUE ~ as.numeric(.)))) %>%
  # Remove group restrictions for future functions
  ungroup() %>%
  # Apply following function to each variable containing the character string "Momentum": Rescale from 0 to 100
  mutate_at(vars(contains("Momentum")), function(x) rescale(x, to = c(0, 100))) %>%
  # Apply following function to each variable containing the character string "Momentum": MAX Stretch and Shift (see discussion on scaling for why)
  # mutate_at(vars(contains("Momentum")), function(x) x * floor((50 / max(abs(x), na.rm = T))) + 50) %>%
  #rescale
  mutate_at(Annual_Values_Vector, function(x) rescale(x, to = c(0, 100))) %>%
  # Replace all "_" in variable names with ""
  select_all(funs(gsub("_", " ", .)))

### Define combined vector which lists all components, clusters, scores and their respective momentum columns

Momentum_Vector <-  unname(vars_select(names(Ex_INDEX_EP_Combined), contains("Momentum")))
Annual_and_Momentum_Vector <- c(Annual_Values_Vector, Momentum_Vector)

# Calculate mean and standard deviation for the specified columns
means <- colMeans(Ex_INDEX_EP_Combined[, Annual_Values_Vector], na.rm = TRUE)
std_devs <- apply(Ex_INDEX_EP_Combined[, Annual_Values_Vector], 2, sd, na.rm = TRUE)

# Create a new data frame for the results
result <- data.frame(mean = means, std_dev = std_devs)

# Print the results
print(result)
############ This block of code generates rank columns

Ex_INDEX_EP_Combined <- Ex_INDEX_EP_Combined %>%
  # Group by country (treat each country group as independent dataframe)
  group_by(Year) %>%
  # Apply following function to each variable listed in Annual_Values_Vector: create descending Rank column
  mutate_at(Annual_Values_Vector, .funs = funs(Rank = dense_rank(-.))) %>% 
  # Apply following function to each variable listed in Momentum_Vector: create descending Rank column
  mutate_at(Momentum_Vector, .funs = funs(Rank = dense_rank(-.))) %>% 
  # Remove group restrictions
  ungroup() %>%
  # Replace all "_" in variable names with ""
  select_all(funs(gsub("_", " ", .)))

### Define vector with desired variable order for organizational purposes later

Order_Vector <-  names(Ex_INDEX_EP_Combined)

###############################################################################################################################################################

################################################## Calculate Zones ##############################################################################################

####################################################################################################################################################################

# This block of code acts like a simple series of if, then functions. We tell the function that if for the given variable, a country is above the median momentum and 
# above the median score, code that country in the new column as "Stand Out", and so on, and so on
# See data cleaning for a description of case_when function

# Ex_INDEX_EP_Combined <- Ex_INDEX_EP_Combined %>%
#   mutate(IsCountry = 1) %>%
#   group_by(Year) %>%
#   mutate(`Digital Index Zone` = case_when(`Digital Mini Index Score` >= median(`Digital Mini Index Score`) & `Digital Mini Index Score Momentum` >= median(`Digital Mini Index Score Momentum`) ~ "Stand Out", 
#                           `Digital Mini Index Score` >= median(`Digital Mini Index Score`) & `Digital Mini Index Score Momentum` <= median(`Digital Mini Index Score Momentum`) ~ "Stall Out",
#                           `Digital Mini Index Score` < median(`Digital Mini Index Score`) & `Digital Mini Index Score Momentum` <= median(`Digital Mini Index Score Momentum`) ~ "Watch Out",
#                           `Digital Mini Index Score` < median(`Digital Mini Index Score`) & `Digital Mini Index Score Momentum` >= median(`Digital Mini Index Score Momentum`) ~ "Break Out")) %>%
#   mutate(`Digital Demand Zone` = case_when(`Digital Demand` >= median(`Digital Demand`) & `Digital Demand Momentum` >= median(`Digital Demand Momentum`) ~ "Stand Out", 
#                                    `Digital Demand` >= median(`Digital Demand`) & `Digital Demand Momentum` <= median(`Digital Demand Momentum`) ~ "Stall Out",
#                                    `Digital Demand` < median(`Digital Demand`) & `Digital Demand Momentum` <= median(`Digital Demand Momentum`) ~ "Watch Out",
#                                    `Digital Demand` < median(`Digital Demand`) & `Digital Demand Momentum` >= median(`Digital Demand Momentum`) ~ "Break Out")) %>%
#   mutate(`Digital Supply Zone` = case_when(`Digital Supply` >= median(`Digital Supply`) & `Digital Supply Momentum` >= median(`Digital Supply Momentum`) ~ "Stand Out", 
#                                    `Digital Supply` >= median(`Digital Supply`) & `Digital Supply Momentum` <= median(`Digital Supply Momentum`) ~ "Stall Out",
#                                    `Digital Supply` < median(`Digital Supply`) & `Digital Supply Momentum` <= median(`Digital Supply Momentum`) ~ "Watch Out",
#                                    `Digital Supply` < median(`Digital Supply`) & `Digital Supply Momentum` >= median(`Digital Supply Momentum`) ~ "Break Out")) %>%
#   ungroup()


# ----
Ex_INDEX_EP_Combined <- Ex_INDEX_EP_Combined %>%
  mutate(IsCountry = 1) %>%
  group_by(Year) %>%
  mutate(
    Bottom_2_3_Supply_Score = quantile(`Supply`, probs = 2/3, na.rm = TRUE),
    Median_Supply_Score_Momentum = median(`Supply Momentum`, na.rm = TRUE),
    `Supply Zone` = case_when(
      `Supply` > Bottom_2_3_Supply_Score & `Supply Momentum` >= Median_Supply_Score_Momentum ~ "Stand Out",
      `Supply` > Bottom_2_3_Supply_Score & `Supply Momentum` < Median_Supply_Score_Momentum ~ "Stall Out",
      `Supply` <= Bottom_2_3_Supply_Score & `Supply Momentum` < Median_Supply_Score_Momentum ~ "Watch Out",
      `Supply` <= Bottom_2_3_Supply_Score & `Supply Momentum` >= Median_Supply_Score_Momentum ~ "Break Out"
    )
  ) %>%
  mutate(
    Bottom_2_3_Demand_Score = quantile(`Demand`, probs = 2/3, na.rm = TRUE),
    Median_Demand_Score_Momentum = median(`Demand Momentum`, na.rm = TRUE),
    `Demand Zone` = case_when(
      `Demand` > Bottom_2_3_Demand_Score & `Demand Momentum` >= Median_Demand_Score_Momentum ~ "Stand Out",
      `Demand` > Bottom_2_3_Demand_Score & `Demand Momentum` < Median_Demand_Score_Momentum ~ "Stall Out",
      `Demand` <= Bottom_2_3_Demand_Score & `Demand Momentum` < Median_Demand_Score_Momentum ~ "Watch Out",
      `Demand` <= Bottom_2_3_Demand_Score & `Demand Momentum` >= Median_Demand_Score_Momentum ~ "Break Out"
    )
  ) %>%
  mutate(
    Bottom_2_3_Institutional_Environment_Score = quantile(`Institutional Environment`, probs = 2/3, na.rm = TRUE),
    Median_Institutional_Environment_Score_Momentum = median(`Institutional Environment Momentum`, na.rm = TRUE),
    `Institutional Environment Zone` = case_when(
      `Institutional Environment` > Bottom_2_3_Institutional_Environment_Score & `Institutional Environment Momentum` >= Median_Institutional_Environment_Score_Momentum ~ "Stand Out",
      `Institutional Environment` > Bottom_2_3_Institutional_Environment_Score & `Institutional Environment Momentum` < Median_Institutional_Environment_Score_Momentum ~ "Stall Out",
      `Institutional Environment` <= Bottom_2_3_Institutional_Environment_Score & `Institutional Environment Momentum` < Median_Institutional_Environment_Score_Momentum ~ "Watch Out",
      `Institutional Environment` <= Bottom_2_3_Institutional_Environment_Score & `Institutional Environment Momentum` >= Median_Institutional_Environment_Score_Momentum ~ "Break Out"
    )
  ) %>%
  mutate(
    Bottom_2_3_Innovation_Score = quantile(`Innovation`, probs = 2/3, na.rm = TRUE),
    Median_Innovation_Score_Momentum = median(`Innovation Momentum`, na.rm = TRUE),
    `Innovation Zone` = case_when(
      `Innovation` > Bottom_2_3_Innovation_Score & `Innovation Momentum` >= Median_Innovation_Score_Momentum ~ "Stand Out",
      `Innovation` > Bottom_2_3_Innovation_Score & `Innovation Momentum` < Median_Innovation_Score_Momentum ~ "Stall Out",
      `Innovation` <= Bottom_2_3_Innovation_Score & `Innovation Momentum` < Median_Innovation_Score_Momentum ~ "Watch Out",
      `Innovation` <= Bottom_2_3_Innovation_Score & `Innovation Momentum` >= Median_Innovation_Score_Momentum ~ "Break Out"
    )
  ) %>%
  ungroup() %>%
  # Optionally, remove the helper columns if they are no longer needed
  select(-Bottom_2_3_Supply_Score, -Median_Supply_Score_Momentum, -Bottom_2_3_Demand_Score, -Median_Demand_Score_Momentum,-Bottom_2_3_Institutional_Environment_Score, -Median_Institutional_Environment_Score_Momentum, -Bottom_2_3_Innovation_Score, -Median_Innovation_Score_Momentum)
  

## Merge to country reference info spreadsheet

Ex_INDEX_EP_Combined <- merge(Ex_INDEX_EP_Combined, increg_wbmc, by = c("Country"), all.x = T, all.y = F)

#####################################################################################################################################################################
dir.create("Booklet Prints")

write_xlsx(Ex_INDEX_EP_Combined, paste0("Booklet Prints/Driver_level_Index_Booklet_WIDE_", Sys.Date(), ".xlsx"), format_headers = F)

############################################## Calculate Global, Region, Zone, and Income Medians ##################################################################

####################################################################################################################################################################

# This series of code blocks generates a short dataframe with just the medians for a number of grouping variable: global, regional, by income, and by zone. We then
# bind the rows of the shorter columns to the bottom of our core dataframe 

## Income Group Medians ##

Ex_INDEX_EP_Combined_x <- Ex_INDEX_EP_Combined %>%
  # group by Year and Income Group (why? We want medians for each income group in each year)
  group_by(Year, `Income Group 2022`) %>%
  # Summarise each column listed in Annual_and_Momentum_Vecotr with the following function: reduce to median
  summarise_at(Annual_and_Momentum_Vector, function(x) median(x)) %>%
  # remove group restrictions
  ungroup() %>%
  # create column denoting that these values are NOT countries
  mutate(IsCountry = 0) %>%
  # code country name as respective region + median
  mutate(CountryName = paste(`Income Group 2022`, "median", sep = " ")) %>%
  # bind to the bottom of our core dataframe 
  bind_rows(Ex_INDEX_EP_Combined, .)

## Region Medians

Ex_INDEX_EP_Combined_y <- Ex_INDEX_EP_Combined %>%
  group_by(Year, MC_Region) %>%
  summarise_at(Annual_and_Momentum_Vector, function(x) median(x)) %>%
  ungroup() %>%
  mutate(IsCountry = 0) %>%
  mutate(CountryName = paste(MC_Region, "median", sep = " ")) %>%
  bind_rows(Ex_INDEX_EP_Combined_x, .)

# attach X ^

## Region Means

Ex_INDEX_EP_Combined_y2 <- Ex_INDEX_EP_Combined %>%
  group_by(Year, MC_Region) %>%
  summarise_at(Annual_and_Momentum_Vector, function(x) mean(x)) %>%
  ungroup() %>%
  mutate(IsCountry = 0) %>%
  mutate(CountryName = paste(MC_Region, "mean", sep = " ")) %>%
  bind_rows(Ex_INDEX_EP_Combined_y, .)

## Zone Medians ##
# Ex_INDEX_EP_Combined_z1 <- Ex_INDEX_EP_Combined %>%
#   group_by(Year, `Zone`) %>%
#   summarise_at(Annual_and_Momentum_Vector, function(x) median(x)) %>%
#   ungroup() %>%
#   mutate(IsCountry = 0) %>%
#   mutate(CountryName = paste(`Zone`, "median", sep = " ")) %>%
#   bind_rows(Ex_INDEX_EP_Combined_y2, .)


# attach y ^

## Global Medians ##

Ex_INDEX_EP_Combined_q <- Ex_INDEX_EP_Combined %>%
  group_by(Year) %>%
  summarise_at(Annual_and_Momentum_Vector, function(x) median(x)) %>%
  ungroup() %>%
  mutate(IsCountry = 0) %>%
  mutate(CountryName = "World Wide - Median") %>%
  bind_rows(Ex_INDEX_EP_Combined_y2, .)

# attach z ^

## Global Means ##

Ex_INDEX_EP_Combined_q2 <- Ex_INDEX_EP_Combined %>%
  group_by(Year) %>%
  summarise_at(Annual_and_Momentum_Vector, function(x) mean(x)) %>%
  ungroup() %>%
  mutate(IsCountry = 0) %>%
  mutate(CountryName = "World Wide - Mean") %>%
  bind_rows(Ex_INDEX_EP_Combined_q, .)

##### Import dataframe with corresponding country codes for each median countryname

Booklet_Median_Codes <- read_excel("Booklet_Median_Codes.xlsx")

#### Natural join country code dataframe for medians to our core dataframe (replace median NA codes with specific median codes)

Ex_INDEX_EP_Combined <- natural_join(Ex_INDEX_EP_Combined_q2, Booklet_Median_Codes, by = c("CountryName"), jointype = "FULL")

# merge q with code booklet ^

# remove x, y, z, and q

rm(list=ls(pattern="^Ex_INDEX_EP_Combined_"))

#### Organize columns!!! ###

Ex_INDEX_EP_Combined <- Ex_INDEX_EP_Combined %>%
  # order vector first, then everything else
  select(Order_Vector, everything()) %>%
  # any columns from reference info sheet before column year
  relocate(names(increg_wbmc), .before = "Year") %>%
  # Zone columns after column year
  relocate(unname(vars_select(names(Ex_INDEX_EP_Combined), contains("Zone"))), .after = "Year") %>%
  # IsCountry? Indicator column immediately after country code
  relocate("IsCountry", .after = "Country") %>%
  # Sort observations by following hierarchy
  arrange(desc(IsCountry), Country, Year)

################################################################################################################

Driver_Wide <- Ex_INDEX_EP_Combined

dir.create("Booklet Prints")

write_xlsx(Ex_INDEX_EP_Combined, paste0("Booklet Prints/Driver_Level_Index_Booklet_WIDE_Region", Sys.Date(), ".xlsx"), format_headers = F)

#####################################################################################################################################################################


################################################################################################################################################

################################### Generate BoxPlot Statistics and Score/Momentum Quartiles ##################################################

##################################################################################################################################################

# I will not bother explaining how this code works-- it's complicated! As long as your character vectors are correctly coded and you have the right
# packages loaded, it should work for you, too.

# For more information on what statistics this code is generating, research "box plot" hinges and whiskers
  
Ex_INDEX_EP_Quants <- Ex_INDEX_EP_Combined %>%
  filter(IsCountry %in% 1) %>%
  select(Year, Annual_and_Momentum_Vector) %>%
  group_by(Year) %>%
  summarise_at(Annual_and_Momentum_Vector, function(x) list(setNames(boxplot.stats(x)$stats,
                                    c('lower_whisker','lower_hinge','median','upper_hinge','upper_whisker')))) %>%
  unnest_auto(Year) %>%
  unnest(cols = Annual_and_Momentum_Vector)


Ex_INDEX_EP_Quants <- as.data.frame(Ex_INDEX_EP_Quants) %>%
  group_by(Year) %>%
  mutate(Country = c("lower whisker", "lower hinge", "median", "upper hinge", "upper whisker")) %>%
  ungroup() %>%
  filter(!Country == "median") %>%
  mutate(CountryName = Country)

# Bind statistics to our core dataframe, and order columns!!!

Ex_INDEX_EP_Combined <- Ex_INDEX_EP_Quants %>%
  bind_rows(Ex_INDEX_EP_Combined, .)  %>%
  select(Order_Vector, everything()) %>%
  relocate(names(increg_wbmc), .before = "Year") %>%
  relocate(unname(vars_select(names(Ex_INDEX_EP_Combined), contains("Zone"))), .after = "Year") %>%
  relocate("IsCountry", .after = "Country") %>%
  arrange(desc(IsCountry), Country, Year)

rm(Ex_INDEX_EP_Quants)

### If then statement, much like we used to generate our zones, to sort countries into quartiles for each indicator listed in our
### Annual and Momentum Vector. Conditional on IsCountry binary "1"

Ex_INDEX_EP_Combined <- Ex_INDEX_EP_Combined %>%
  mutate(IsCountry = case_when(IsCountry %in% NA_real_ ~ 0, TRUE ~ IsCountry)) %>%
  mutate_at(Annual_and_Momentum_Vector, funs(Quartile = case_when(IsCountry == 1 & . == 100 ~ 4, 
                                         IsCountry == 1 & . <= .[Country == "lower hinge"] ~ 1,
                                         IsCountry == 1 & . > .[Country == "lower hinge"] & . < .[Country == "WWM"] ~ 2,
                                         IsCountry == 1 & . >= .[Country == "WWM"] & . <= .[Country == "upper hinge"] ~ 3,
                                         IsCountry == 1 & . >= .[Country == "upper hinge"] ~ 4))) %>%
  mutate_if(is.numeric, function(x) round(x, digits = 2)) %>%
  select_all(funs(gsub("_", " ", .)))

###########################################################################################################################

############################# Create Useful Sort, Type, and Class Columns Used To Search Values ###########################

###########################################################################################################################

### Melt wide format core dataset to long format

Ex_INDEX_EP_Combined_LONG <- melt(Ex_INDEX_EP_Combined, id = (c("Country", "IsCountry", "CountryName", "MC Region", "WB Region", "Income Group 2022", "Year")))

### If, then statement creating the new variable "Type". Type describes what type of value is in the value column. Is it a score, a quartile, a zone?

Ex_INDEX_EP_Combined_LONG <- Ex_INDEX_EP_Combined_LONG %>%
  mutate(Type = case_when(grepl("Momentum Rank", variable) ~ "Momentum Rank",
                          grepl("Momentum Quartile", variable) ~ "Momentum Quartile",
                          grepl("Rank", variable) ~ "Rank",
                          grepl("Quartile", variable) ~ "Quartile",
                          grepl("Momentum", variable) ~ "Momentum",
                          grepl("Zone", variable) ~ "Zone",
                          TRUE ~ "Score"))

### Multiple mutation function which trims words like "Momentum", "Rank", and "Qaurtile" from variable to create new "variable parent" column. Variable parent column


Ex_INDEX_EP_Combined_LONG <- Ex_INDEX_EP_Combined_LONG %>%
  mutate(variable_name = variable) %>%
  mutate(variable_name = gsub(" Momentum", "", variable_name), 
         variable_name = gsub(" Zone", "", variable_name),
         variable_name = gsub(" Quartile", "", variable_name),
         variable_name = gsub(" Rank", "", variable_name))

## Mutate function which classifies entries in "variable parent" column as either clusters or components, or overall scores

Ex_INDEX_EP_Combined_LONG <- Ex_INDEX_EP_Combined_LONG %>%
  mutate(Class = case_when(variable_name %in% unique(Index_Rebuild_Drivers$Driver) ~ "Driver",
                           variable_name %in% unique(Index_Rebuild_Components$Component) ~ "Component",
                           variable_name %in% unique(Index_Rebuild_Clusters$Cluster) ~ "Cluster",
                           TRUE ~ variable_name))

#### Uses the Index Rebuild Sheets to attach cluster-component hierarchy to core dataset #####

Hierarchy_Table_Clusters <- Index_Rebuild_Clusters %>%
  select(Cluster, Component) %>%
  rename(variable_name = Cluster)

Ex_INDEX_EP_Combined_LONG <- merge(Ex_INDEX_EP_Combined_LONG, Hierarchy_Table_Clusters, by = "variable_name", all.x = T, all.y = F)

Hierarchy_Table_Components <- Index_Rebuild_Components %>%
  select(Component, Driver) %>%
  rename(variable_name = Component)

Ex_INDEX_EP_Combined_LONG <- merge(Ex_INDEX_EP_Combined_LONG, Hierarchy_Table_Components, by = "variable_name", all.x = T, all.y = F)

###########  Final small adjustments and organize columns ##############################

Ex_INDEX_EP_Combined_LONG <- Ex_INDEX_EP_Combined_LONG %>%
  mutate(Driver = case_when(Driver %in% NA ~ variable_name, TRUE ~ Driver)) %>%
  select(Country, IsCountry, CountryName, `WB Region`, `MC Region`, `Income Group 2022`, Year, variable_name, variable, Type, value, Class, Component, Driver) %>%
  rename(variable_parent = variable_name) %>%
  arrange(desc(IsCountry), Country, Class, Driver, Component, variable_parent, Type)


################################################################################################################################

############################ Printing Booklets ##################################################################################

################################################################################################################################

dir.create("Booklet Prints")

write_xlsx(Ex_INDEX_EP_Combined_LONG, paste0("Booklet Prints/Driver_Booklet_FULL_", Sys.Date(), ".xlsx"), format_headers = F)

