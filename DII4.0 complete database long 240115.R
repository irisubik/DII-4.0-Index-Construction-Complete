# Previous Packages
# ANSWER NO IF ASKED!!!
# 
# install.packages("writexl", type = "binary")
# install.packages("readxl", type = "binary")
# install.packages("dplyr", type = "binary")
# install.packages("reshape2", type = "binary")
# install.packages("tidyverse", type = "binary")
# install.packages("rqdatatable", type = "binary")
# install.packages("countrycode", type = "binary")
# install.packages("zoo", type = "binary")
# install.packages("imputeTS", type = "binary")
# install.packages("ggplot2", type = "binary")

# Load Packages
# ANSWER NO IF ASKED!!!


#####check datalibrary(readxl)
library(writexl)
library(countrycode)
library(dplyr)
library(tidyverse)
library(reshape2)
library(readxl)


##################### 2. Prep and Export Pre-RTMI Database #########################################

####################################################################################################

# 1. Import country reference info excel
increg_mconly <- read_excel("increg_mc.xlsx")
#modified the reference table here.WB_Region refers Income group
WB_Region <- read_excel("income group 2022.xlsx") %>%
  select(-CountryName)

increg_wbmc <- merge(increg_mconly[c("Country", "CountryName", "MC_Region", "SubRegion")], WB_Region, by = "Country", all = T)

#in prior file, LGINC column is incorrect
increg_wbmc <- increg_wbmc %>%
  rename("WB_Region" = "Region") %>%
  mutate(`Income Group Num` = case_when(
    `Income Group 2022` == "Low income" ~ 1,
    `Income Group 2022` == "Lower middle income" ~ 2,
    `Income Group 2022` == "Upper middle income" ~ 3,
    `Income Group 2022` == "High income" ~ 4,
    TRUE ~ NA_integer_ )) %>%
  mutate(LGINC = ifelse(`Income Group Num` %in% c(1:2), 1, 2)) 
  
Ex_complete_database <- read_excel("merged_all_year_ind_240709.xlsx", guess_max = 5000) 

# latest_data_years <- Ex_complete_database %>%
#   summarise(across(where(is.numeric), ~ max(Year[!is.na(.)], na.rm = TRUE))) %>%
#   pivot_longer(
#     cols = everything(), 
#     names_to = "Code", 
#     values_to = "latest_year"
#   )
# 
# latest_data_years <- latest_data_years %>%
#   left_join(Index_Rebuild, by = 'Code')
# 

n_distinct(Ex_complete_database$Country)

# Ex_complete_database$glorgcm <- as.numeric(Ex_complete_database$glorgcm)
# glorgcm_data <- Ex_complete_database[Ex_complete_database$Year == 2021, c('Country', 'Year', 'glorgcm')]
# print(glorgcm_data)
################################### Fix the issues of the original data files ###################################

Ex_complete_database <- Ex_complete_database %>%
  mutate(fourg_gsma = if_else(Year %in% c(2008, 2009), 0, fourg_gsma))

# fourg_gsma_data <- Ex_complete_database[Ex_complete_database$Year %in% c(2008, 2009), c('Country', 'Year', 'fourg_gsma')]
# print(fourg_gsma_data)

# Setting 'nopos' to NA for 'PHL' between 2020 and 2023---> Euromonitor corrected their data and no need to do this anymore. 
# but 2023 EGY data is wrong, modify it below
Ex_complete_database <- Ex_complete_database %>%
  mutate(nopos = ifelse(Country == 'EGY' & Year == 2023, 198.8, nopos))


egy_pos_data <- Ex_complete_database[Ex_complete_database$Country == 'EGY',
c('Country', 'Year', 'nopos')]

#####################################Modify the Chile and Egypt atmpc data in 2023
Ex_complete_database <- Ex_complete_database %>%
  mutate(atmpc = ifelse(Country == 'CHL' & Year == 2023, 7.5, atmpc)) %>%
  mutate(atmpc = ifelse(Country == 'EGY' & Year == 2023, 21.9, atmpc)) 


egy_pos_data <- Ex_complete_database[Ex_complete_database$Country == 'EGY',
                                     c('Country', 'Year', 'nopos')]
#######################################################################################################
## -> Rebuild file tells us some indicators needs to be transformed by dividing through by normalizer
# Import InidcatorInfo tab of Rebuild File and filter to exclude the indicators that already normalized
Index_Rebuild <- read_excel("DII 4.0 Evolution Rebuild 240711.xlsx")
Index_Rebuild <- Index_Rebuild %>%
  #Those indicators are already normalized
  filter(!(Index_Rebuild$Code %in% c("cpvpc", "cptpc")))


# ppp_converter

Ex_complete_database <- Ex_complete_database %>%
  rename(ppp_cons = ppp_usd)

#####################################Modify the VEN PPP data from 0 to actual values
# New gni_pc values
new_VEN_PPP_values <- c(1.4, 1.5, 2.1, 2.7, 3, 4, 5.6,
                        NA, NA, NA, NA, NA, NA, 0.5, 1.4, 6.5)

# Years from 2008 to 2022
years <- 2008:2023

# Update gni_pc values for Croatia (HRV) for each year
for (i in 1:length(years)) {
  Ex_complete_database <- Ex_complete_database %>%
    mutate(ppp_cons = if_else(Country == 'VEN' & Year == years[i], new_VEN_PPP_values[i], ppp_cons))
}

#####################################Modify the VEN PPP data from 0 to actual values
# New gni_pc values
new_VEN_exchange_values <- c(2.1, 2.1, 2.6, 4.3, 4.3, 6, 6.3,
                        6.3, 9.3, 10, NA, 0.014, 0.32, 3.2, 6.7, 30.3)

# Years from 2008 to 2022
years <- 2008:2023

# 
for (i in 1:length(years)) {
  Ex_complete_database <- Ex_complete_database %>%
    mutate(exchange = if_else(Country == 'VEN' & Year == years[i], new_VEN_exchange_values[i], exchange))
}


# Check the updated data
check_data_VEN_no_estimate <- Ex_complete_database[Ex_complete_database$Country == 'VEN', c('Country', 'Year', 'ppp_cons','exchange')]
###########################################################
n_distinct(Ex_complete_database$Country)
#check if there any 0 PPP values
check_data <- Ex_complete_database[!is.na(Ex_complete_database$ppp_cons) & Ex_complete_database$ppp_cons == 0, c('Country', 'Year', 'ppp_cons')]


# change 0 ppp value to NA
# Ex_complete_database$ppp_cons <- ifelse(Ex_complete_database$ppp_cons == 0, NA, Ex_complete_database$ppp_cons)


Ex_complete_database <- Ex_complete_database %>%
  mutate(ppp_usd = exchange / ppp_cons)

sum(is.infinite(Ex_complete_database$indicator), na.rm = TRUE)

n_distinct(Ex_complete_database$Country)
#####################################Modify the Croatia currency unit to Euo
# New gni_pc values
new_gni_pc_values <- c(15169.4, 13754.0, 13153.4, 14101.4, 12936.8, 13686.2, 13804.1, 
                       12190.1, 12449.3, 13644.7, 15197.0, 15449.4, 14956.4, 17793.2, 18514.3, 21543.4)

# Years from 2008 to 2022
years <- 2008:2023

# Update gni_pc values for Croatia (HRV) for each year
for (i in 1:length(years)) {
  Ex_complete_database <- Ex_complete_database %>%
    mutate(gni_pc = if_else(Country == 'HRV' & Year == years[i], new_gni_pc_values[i], gni_pc))
}

# Check the updated data
check_data <- Ex_complete_database[Ex_complete_database$Country == 'HRV', c('Country', 'Year', 'cexpc','gni_pc')]

check_data <- check_data %>%
  mutate(cexpc_norm = cexpc/gni_pc)


#####################################Modify the 2 broadband 
# Create a data frame with the provided data
new_data <- data.frame(
  Country = c("MUS", "MUS", "MUS", "MUS", "MUS", "MLT", "MLT", "MLT", "MLT", "MLT", "SGP", "SGP", "SGP", "SGP", "SGP", "HKG", "HKG", "HKG", "HKG", "HKG", "BHR", "BHR", "BHR", "BHR", "BHR"),
  Year = c(2019, 2020, 2021, 2022, 2023, 2019, 2020, 2021, 2022, 2023, 2019, 2020, 2021, 2022, 2023, 2019, 2020, 2021, 2022, 2023, 2019, 2020, 2021, 2022, 2023),
  avgmx = c(NA, NA, NA, 93920, NA, NA, NA, NA, 121430, NA, 850490, 728740, 783630, 766530, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  avgbw = c(NA, NA, NA, 19690, 37442, NA, NA, NA, 117680, 103488, 190000, NA, 184650, 214230, 251152, NA, NA, 153960, 203460, 233674, NA, NA, NA, NA, 72040)
)

# Update the Ex_complete_database
for (i in 1:nrow(new_data)) {
  Ex_complete_database <- Ex_complete_database %>%
    mutate(
      avgmx = if_else(Country == new_data$Country[i] & Year == new_data$Year[i], new_data$avgmx[i], avgmx),
      avgbw = if_else(Country == new_data$Country[i] & Year == new_data$Year[i], new_data$avgbw[i], avgbw)
    )
}

check_data <- Ex_complete_database[Ex_complete_database$Country == 'HKG', c('Country', 'Year', 'avgmx', 'avgbw')]

# ictfdi, LUX LVCF from 2021
Ex_complete_database <- Ex_complete_database %>%
  mutate(ictfdi = ifelse(Country == 'LUX' & Year == 2022, NA, ictfdi))

check <- Ex_complete_database %>%
  select(Country, Year, ictfdi)

######################################
################################### set the real time transaction indicator values to 0 for NAs and 208-2014 ###################################

Ex_complete_database <- Ex_complete_database %>%
  mutate(realtime_pct = if_else(is.na(realtime_pct) | Year %in% 2008:2014, 0, realtime_pct))

summary(Ex_complete_database$realtime_pct)

check <- Ex_complete_database %>%
  select(Country, Year, realtime_pct)

######################################
################################### set the naistrat indicator values to 0 for 2008-2016 ###################################

Ex_complete_database <- Ex_complete_database %>%
  mutate(naistrat = if_else(Year %in% 2008:2016, 0, naistrat))

summary(Ex_complete_database$naistrat)

check <- Ex_complete_database %>%
  select(Country, Year, naistrat)

######################################
################################### set the naistrat indicator values to 0 for 2008-2016 ###################################

Ex_complete_database <- Ex_complete_database %>%
  mutate(vc_ai_compute = if_else(is.na(vc_ai_compute) | Year %in% 2008:2009, 0, vc_ai_compute))

summary(Ex_complete_database$vc_ai_compute)

check <- Ex_complete_database %>%
  select(Country, Year, vc_ai_compute)

######################################


###################################### change vc_ai normalizer from GDP to total 125 country investment, HKG and QAT use estimats

#next value carry backward
# Ex_complete_database <- Ex_complete_database %>%
#   arrange(Country, Year) %>%
#   group_by(Country) %>%
#   fill(vc_ai, .direction = "downup") %>%
#   ungroup()

# change the values  
# Initialize vc_ai values for 2008 and 2009 to a very small number (20+ zeroes)
# very_small_value <- 1e-25  # This represents 0.0000000000000000000000001
# Ex_complete_database <- Ex_complete_database %>%
#   mutate(vc_ai = if_else(is.na(vc_ai) & Year %in% 2008:2009, very_small_value, vc_ai))
# 
# # Set vc_ai to 0 for other years where data is missing
# Ex_complete_database <- Ex_complete_database %>%
#   mutate(vc_ai = if_else(is.na(vc_ai) & !Year %in% 2008:2009, 0, vc_ai))
# 
# vc_ai_total <- read_excel("reference_vc_ai_total.xlsx")

# reference_delta_gdp <- read_excel("reference_delta_gdp.xlsx") %>%
#   select(Year, Country, delta_gdp) %>%
#   mutate(Year = as.numeric(Year))
# 
# str(reference_delta_gdp)
# 
# str(Ex_complete_database)
# 
# Ex_complete_database <- Ex_complete_database %>%
#   left_join(reference_delta_gdp, by = c('Year', 'Country'))
# 
# check <- Ex_complete_database %>%
#   select(Country, Year, delta_gdp)
######################################
######################################

for(i in 1:nrow(Index_Rebuild)) {
  indicator <- Index_Rebuild$Code[i]
  normalizer <- Index_Rebuild$Normalizer[i]
  
  # Check if the normalizer is not NA 
  if (!is.na(normalizer)) {
    # Check the normalizer type and apply the corresponding transformation
    if(normalizer == "PPP") {
      Ex_complete_database <- Ex_complete_database %>%
        mutate(!!as.name(indicator) := !!as.name(indicator) * ppp_usd)
    } else if(normalizer == "%GNI PC") {
      Ex_complete_database <- Ex_complete_database %>%
        mutate(!!as.name(indicator) := !!as.name(indicator) / gni_pc)
    } else if(normalizer == "% retailing") {
      Ex_complete_database <- Ex_complete_database %>%
        mutate(!!as.name(indicator) := !!as.name(indicator) / retail)
    } else if(normalizer == "% transaction value") {
      Ex_complete_database <- Ex_complete_database %>%
        mutate(!!as.name(indicator) := !!as.name(indicator) / tvpc)
    } else if(normalizer == "% transactions") {
      Ex_complete_database <- Ex_complete_database %>%
        mutate(!!as.name(indicator) := !!as.name(indicator) / tpc)
    } else if(normalizer == "per million") {
      Ex_complete_database <- Ex_complete_database %>%
        mutate(!!as.name(indicator) := !!as.name(indicator) / (population/1000))
    } else if(normalizer == "%GDP") {
      Ex_complete_database <- Ex_complete_database %>%
        mutate(!!as.name(indicator) := !!as.name(indicator) / (gdp /1000000))
    } else if(normalizer == "*GDP") {
      Ex_complete_database <- Ex_complete_database %>%
        mutate(!!as.name(indicator) := !!as.name(indicator) * gdp)
    } else if(normalizer == "deltaGDP") {
      Ex_complete_database <- Ex_complete_database %>%
        mutate(!!as.name(indicator) := !!as.name(indicator) / delta_gdp)
    } 
  }
}

Index_Rebuild <- read_excel("DII 4.0 Evolution Rebuild 240711.xlsx")

sum(Ex_complete_database$ppp_cons == 0, na.rm = TRUE)

#HKG, QAT use estimates
# vc_ai_estimates <- read_excel("reference_vc_ai_total.xlsx", sheet = 'estimates')
# 
# Ex_complete_database <- Ex_complete_database %>%
#   left_join(vc_ai_estimates, by = c("Country", "Year"), suffix = c("", ".estimate")) %>%
#   mutate(vc_ai = ifelse(Country %in% c('HKG', 'QAT'), vc_ai_estimates, vc_ai)) %>%
#   select(-vc_ai_estimates)


# Set the very small value for the year 2023 for all countries with 0 values so the CAGR won't be negative
# Ex_complete_database <- Ex_complete_database %>%
#   mutate(vc_ai = if_else((Year == 2023 & vc_ai == 0) | vc_ai == 0, very_small_value, vc_ai))
# 
# check <- Ex_complete_database %>%
#   select(Country, Year, vc_ai)
# 

#2. Merge country reference info sheet to database, keep all rows X, only matches from Y

Ex_complete_database <- merge(Ex_complete_database, increg_wbmc, by = c("Country"), all.x = T, all.y = F)

# Relocate reference columns to front of database

Ex_complete_database <- Ex_complete_database %>%
  relocate(names(increg_wbmc), .before = "Year")

n_distinct(Ex_complete_database$CountryName)

# Index_Rebuild <- read_excel("DII 4.0 Evolution Rebuild 240321.xlsx")
# Index_Rebuild <- Index_Rebuild %>%
#   filter(Index_Rebuild$Pillar == 'Institutions')
####################################################################################################

# 3.  Create long version of database for print by "melting" our wide version

# Check for infinite values in a data frame
inf_values <- sapply(Ex_complete_database, function(x) any(is.infinite(x)))

# Print columns with infinite values
names(Ex_complete_database)[inf_values]

# Replace infinite values with NA in each column of the data frame
Ex_complete_database[] <- lapply(Ex_complete_database, function(x) ifelse(is.infinite(x), NA, x))


Ex_complete_database_long <- melt(Ex_complete_database, id = c("Country", "CountryName", "MC_Region", "WB_Region", "Income Group 2022", "Year")) %>%
  rename(Code = variable)

n_distinct(Ex_complete_database_long$Code)

# # 4. Import InidcatorInfo tab of Rebuild File
# 
# Index_Rebuild <- read_excel("DII 4.0 Evolution Rebuild 230109.xlsx")

# 5. Merge to long database bound for export

Ex_complete_database_long <- merge(Ex_complete_database_long, Index_Rebuild[c("Indicator", "Code",	"Normalizer",	"Cluster", "Component",	"Source")], by = c("Code"), all.x = F, all.y = F)

# check if all the indicators data were included
n_distinct(Ex_complete_database_long$Code)

distinct_values_big <- distinct(Ex_complete_database_long, Code) %>% pull(Code)
distinct_values_complete <- distinct(Index_Rebuild, Code) %>% pull(Code)
# 
diff_df1_not_in_df2 <- setdiff(distinct_values_complete, distinct_values_big)
diff_df1_not_in_df2

# 6. Relocate Indicator info reference columns before Year column

Ex_complete_database_long <- Ex_complete_database_long %>%
  relocate(c("Indicator", "Code",	"Normalizer",	"Cluster",	"Component", 	"Source"), .before = "Year")

# 7. Export Long Format Database with no Estimations
dir.create("Database Prints")

write_xlsx(Ex_complete_database_long, paste0("Database Prints/Index_Database_Long_no_Estimations_", Sys.Date(), ".xlsx"), format_headers = F)
n_distinct(Ex_complete_database_long$Code)


######################################### do estimates for binary indicators under Institutions before RTMI, 'natstrat', 'netneut', 'naistrat'########
###################will use binary_data df later to merge
binary_data <- Ex_complete_database %>%
  group_by(Country) %>%
  arrange(Year) %>%
  mutate_at('natstrat', zoo::na.locf, na.rm = FALSE) %>% 
  mutate_at('netneut', zoo::na.locf, na.rm = FALSE) %>% 
  mutate_at('naistrat', zoo::na.locf, na.rm = FALSE)

binary_data <- binary_data %>% 
  mutate_at(vars(natstrat, netneut, naistrat), ~ ifelse(is.na(.), 0, .)) 

check_data <- Ex_complete_database[c('Country', 'Year', 'naistrat')]

####################################################################################################

###################### 3. Apply Neccessary Transformations and Normalizations ######################

####################################################################################################

# 8. Returning to wide format database, we apply normalizations to indicators where applicable



# ####################################################################################################
# 
# ###################### 4.  Verify Database Integrity ################################################
# 
# ####################################################################################################
# 
# # This set of functions probably can be better optimized, but it does successfully create a checklist
# # which verifies whether all indicators denoted in the rebuild file have a column in the database
# 
# iv_rebuild <- Index_Rebuild %>%
#   select(Indicator, Code, Source) %>%
#   mutate(ID1 = Code) %>%
#   group_by(Code) %>%
#   mutate(ID2 = row_number()) %>%
#   ungroup()
# 
# iv_database <- as.data.frame(colnames(Ex_complete_database)) %>%
#   mutate(ID1 = `colnames(Ex_complete_database)`) %>%
#   group_by(`colnames(Ex_complete_database)`) %>%
#   mutate(ID2 = row_number()) %>%
#   ungroup() %>%
#   filter(!ID1 %in% names(increg_wbmc)) %>%
#   filter(!ID1 %in% "Year")
# 
# iv_checklist <- full_join(iv_database, iv_rebuild, by = c("ID1", "ID2")) %>%
#   #select(-starts_with("ID")) %>%
#   rename(iv_database = `colnames(Ex_complete_database)`, iv_rebuild = Code) %>%
#   #select(Indicator, Source, iv_rebuild, iv_database, order_i) %>%
#   arrange(iv_rebuild) %>%
#   select(Indicator, Source, iv_rebuild, iv_database)
# 
# view(iv_checklist)
# 
# ###### REMOVE CHECKLIST ITEMS ONCE VERIFIED #######
# 
# rm(list=ls(pattern="^iv_"))
# 
# ##########################################################################################################
# 
# #### WE NOW HAVE OUR COMPLETE, PREPPED DATABASE #####
# 
# #### GO ON TO CODE SHEET Index_RTMI_Estimations to CONTINUE TUTORIAL ###
# 
# ##########################################################################################################
# 
# 
# 
