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
# install.packages("scales", type = "binary")

# Load Packages

library(tidyverse)
library(dplyr)
library(zoo)
# library(ggplot2)
# library(ggrepel)
library(readxl)
library(writexl)
library(countrycode)
library(reshape2)
library(scales)

### Import all three tabs from Rebuild File ########

Index_Rebuild <- read_excel("DII 4.0 Evolution Rebuild 240711.xlsx")
Index_Rebuild_Clusters <- read_excel("DII 4.0 Evolution Rebuild 240711.xlsx", sheet = "Clusters")
Index_Rebuild_Components <- read_excel("DII 4.0 Evolution Rebuild 240711.xlsx", sheet = "Components")

###### Filter Rebuild file for only indicators with codes ####

Index_Rebuild <- Index_Rebuild %>%
  # filter(Index_Rebuild$Pillar == 'Innovation') %>%
  filter(!Code %in% NA) %>%
  filter(Driver != "Reference") %>%
  arrange(Component, Cluster, Code)

#### Merge IndicatorInfo tab to Confidence Scores for each Indicator by Code column ###

Index_Rebuild <- merge(Index_Rebuild, INDEX_Code_CS, by = "Code", all.x = T, all.y = F)

#### Rescale Indicator confidence scores to 0.75 to 1, which translates to "the indicator with the worst data availability
#### will be weighted in the data avaialbility metric 3/4 as much as the indicator with the best"

Index_Rebuild <- Index_Rebuild %>%
  # rescale CS indicator scores to 0.75 to 1
  mutate(CS = rescale(CS, to = c(0.75, 1))) %>%
  # Aggregate data centrality, data type, and data avaialability weights into a single weight for each indicator
  mutate(Weight = rowMeans(select(., c(Weight_Centrality, Weight_Type, CS))))

#### Create vector with all names of indicators which will need to be inversed after scaling ####

highbad <- Index_Rebuild %>%
  filter(`Inverse (Y/N)` == "Y")

highbad <- highbad$Code

###################################################################################################################

#################### PREP DATABASE FOR AGGREGATION ##############################################################

##################################################################################################################

###### Melt Final RTMI-filled database into long format, specifying the reference info and year columns as ids not be melted ####

Ex_Database_RTMI_LONG <- melt(Ex_Database_RTMI_8_125country, id = c("Country", "CountryName", "MC_Region", "WB_Region", "Income Group 2022", "Year", "Income Group Num", "SubRegion", "LGINC")) %>%
  rename(Code = variable)

#### Merge melted databse with index rebuild sheet, which has the weights for the indicators #######

Ex_Database_RTMI_LONG <- merge(Ex_Database_RTMI_LONG, Index_Rebuild[c("Code",	"Cluster", "Component",	"Weight")], by = c("Code"), all.x = F, all.y = F) 

#### Remove any reference indicators from the database ####

Ex_Database_RTMI_LONG <- Ex_Database_RTMI_LONG %>%
  filter(!Cluster %in% "Reference")

n_distinct(Ex_Database_RTMI_LONG$Cluster)
#### Scan database for unexpected NA values ######

# which(is.na(Ex_Database_RTMI_LONG), arr.ind=TRUE)

total_na_values <- sum(is.na(Ex_Database_RTMI_LONG))
total_na_values
which(is.na(Ex_Database_RTMI_LONG), arr.ind=TRUE)
##############################################################################################################

######################## AGGREGATION #########################################################################

#################################################################################################################

## Aggregation involves the averaging of indicators into clusters, components, and drivers
## The process follows a simple, three step recursive process for each tier:
## 1. Scale: scale indicators into z-scored values (standardized)
## 2. Weight: Multiply scaled values by weights
## 3. Aggregate: Take the arithmetic average of each tier group (clusters, components, drivers)

Ex_Database_RTMI_LONG_Indicators <- Ex_Database_RTMI_LONG %>%
  #### SCALE ####
# Treat each code group as an independent database 
  group_by(Code) %>%
  # standardize into z-scores
  mutate(value = scale(value)) %>% 
  # # set a threshold below which values are considered too small and set them to zero instead of NA
  # mutate(value_scaled = ifelse(abs(value_scaled) < 1e-10, 0, value_scaled)) %>%
  #  the presence of infinite values in the original dataset could potentially cause NA value
  # mutate(value = ifelse(is.infinite(value), NA, value)) %>%
  # inverse values indicated by highbad vector
  mutate(value = ifelse(Code %in% highbad, value * -1, value)) %>%
  # remove group by restriction
  ungroup()

total_na_values <- sum(is.na(Ex_Database_RTMI_LONG_Indicators))
total_na_values

################
###############lower the indicator weights to QA the THA&IND momentum issue#######################################################################
Ex_Database_RTMI_LONG_Indicators <- Ex_Database_RTMI_LONG_Indicators %>%
  mutate(Weight = ifelse(Code %in% c('gov_mbl','remits_mbl', 'adlrt'), 0, Weight))


# dir.create("Analysis")
# 
# write_xlsx(Ex_Database_RTMI_LONG_Indicators, paste0("Analysis/Rescaled_Indicators_", Sys.Date(), ".xlsx"), format_headers = F)
# 

Ex_Database_RTMI_LONG_Clusters <- Ex_Database_RTMI_LONG_Indicators %>%
  #### Weight ####
# Multiply values through by indicator weight
mutate(value = value * Weight) %>%
  #### AGGREGATE ####
# Group by country, year, and cluster (why? We want the average for each cluster for each country for each year)
group_by(Country, Year, Cluster) %>%
  # Summarise grouped value column into arithmetic mean
  summarise(value = mean(value)) %>%
  # remove group restictions
  ungroup() %>%
  ### Scale ####
group_by(Cluster) %>%
  mutate(value = scale(value)) %>%
  arrange(Country, Cluster, Year) %>%
  ungroup()

total_na_values <- sum(is.na(Ex_Database_RTMI_LONG_Clusters))
total_na_values

n_distinct(Ex_Database_RTMI_LONG_Clusters$Cluster) 
#### Attach index cluster rebuild tab with cluster weights ####

# Convert 'Cluster' column to lowercase in both dataframes
# Ex_Database_RTMI_LONG_Clusters$Cluster <- tolower(Ex_Database_RTMI_LONG_Clusters$Cluster)
# Index_Rebuild_Clusters$Cluster <- tolower(Index_Rebuild_Clusters$Cluster)

# fix the names -> DII2 TO DII3 names (follow DII3)
# Ex_Database_RTMI_LONG_Clusters <- Ex_Database_RTMI_LONG_Clusters %>%
#   mutate(Cluster = ifelse(Cluster == "bureacracy", "bureaucracy", Cluster)) %>%
#   mutate(Cluster = ifelse(Cluster == "start up capacity", "startup capacity", Cluster))

Ex_Database_RTMI_LONG_Clusters <- merge(Ex_Database_RTMI_LONG_Clusters, Index_Rebuild_Clusters[c("Driver", "Cluster", "Component", "ClusterWeight")], by = "Cluster", all.x = T, all.y = F) %>% 
  arrange(Country, Cluster, Year)

total_na_values <- sum(is.na(Ex_Database_RTMI_LONG_Clusters))
total_na_values

which(is.na(Ex_Database_RTMI_LONG_Clusters), arr.ind=TRUE)

n_distinct(Ex_Database_RTMI_LONG_Clusters$Cluster) 

Ex_Database_RTMI_LONG_Components <- Ex_Database_RTMI_LONG_Clusters %>%
  ### Weight ####
mutate(value = value * ClusterWeight) %>%
  #### AGGREGATE ####
group_by(Country, Year, Component) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  ### Scale ####
group_by(Component) %>%
  mutate(value = scale(value)) %>%
  arrange(Country, Component, Year) %>%
  ungroup()


#### Attach index cluster rebuild tab with Component weights ####

Ex_Database_RTMI_LONG_Components <- merge(Ex_Database_RTMI_LONG_Components, Index_Rebuild_Components, by = "Component") %>% 
  arrange(Country, Component, Year)

Ex_Database_RTMI_LONG_Scores <- Ex_Database_RTMI_LONG_Components %>%
  ### Weight ####
mutate(value = value * CompWeight) %>%
  #### AGGREGATE ####
group_by(Country, Year) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  ### Scale ####
mutate(value = scale(value)) %>%
  arrange(Country, Year) %>%
  ungroup()



#### WE NOW HAVE OUR AGGREGATED SCORE SHEETS #####

#### GO ON TO CODE SHEET Index_Booklet_Builder_Code to CONTINUE TUTORIAL ###

############################################################################################################################################