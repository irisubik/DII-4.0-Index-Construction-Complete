#####################################################################################
# STEP 12: RTMI Round 4: LGINC, Year Group MATCHES

# 3.1: Create indicator means for each MATCH  group
# RTMI_LR: LGINC, Year MATCH

RTMI_LR <- RTMI_CORE_DATASET %>%
  # treat each set of income group-year matches as independent datasets
  group_by(LGINC, Year) %>%
  # IF variable is numeric, reduce all rows to mean for each income group-year match
  summarise_if(is.numeric, function(x) mean(x, na.rm = T)) %>%
  # Remove group restrictions for future functions
  ungroup() %>%
  # replace NaN NA type with NA numeric (weird R neccessity)
  mutate_if(is.numeric, function(x) ifelse(is.nan(x), NA, x))

# "NATURAL JOIN" (Replace NA Values of DF1 with datapoints from DF2) for income group-WB_Region-year matches
# In other words, the dataset matches our missing value dataset (DF1) with our intersect mean dataset (DF2) ALONG income group-WB_Region-year row matches.
# IF the row is a missing value, the natural join subs the NA for the intersect mean from DF2

Ex_Database_RTMI_8 <- natural_join(Ex_Database_RTMI_7, RTMI_LR, by = c("LGINC", "Year"), jointype = "FULL")

# Rearrange columns to default order, and arrange rows by listed sort hierarchy

Ex_Database_RTMI_8 <- Ex_Database_RTMI_8[DATASET_INDICATOR_ORDER] %>%
  arrange(desc(WB_Region), `Income Group 2022`, Country, Year)

##################################################################################################################################


# STEP 13: Stine Interpolation Round 5

Ex_Database_RTMI_8 <- Ex_Database_RTMI_8 %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate_at(VARIABLES_TO_BE_ESTIMATED, funs(if(sum(!is.na(.))<2) {.} else{replace(na_interpolation(., option = "stine"), is.na(na.approx(., na.rm=FALSE)), NA)})) %>%
  ungroup() %>%
  arrange(desc(WB_Region), `Income Group 2022`, Country, Year)

#####################################################################################################################################

# STEP 14: FILL (LVCF) Round 5 where applicable

Ex_Database_RTMI_8 <- Ex_Database_RTMI_8 %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  fill(VARIABLES_TO_BE_ESTIMATED, .direction = "downup") %>%
  ungroup() %>%
  arrange(desc(WB_Region), `Income Group 2022`, Country, Year)

###########################update the rural parity data for HK and SGP--to perfect parity###############
check_data <- Ex_Database_RTMI_8[Ex_Database_RTMI_8$Country == 'HKG', 
                                 c('Country', 'Year', 'dispi_rural','ibuy_rural','ipay_rural','mdply_rural','acnt_rural', 'rdply_rural')]

Ex_Database_RTMI_8 <- Ex_Database_RTMI_8 %>%
  mutate(
    dispi_rural = if_else(Country %in% c('SGP', 'HKG'), 1, dispi_rural),
    ibuy_rural = if_else(Country %in% c('SGP', 'HKG'), 1, ibuy_rural),
    ipay_rural = if_else(Country %in% c('SGP', 'HKG'), 1, ipay_rural),
    mdply_rural = if_else(Country %in% c('SGP', 'HKG'), 1, mdply_rural),
    acnt_rural = if_else(Country %in% c('SGP', 'HKG'), 1, acnt_rural),
    rdply_rural = if_else(Country %in% c('SGP', 'HKG'), 1, rdply_rural)
  )

check_data <- Ex_Database_RTMI_8[Ex_Database_RTMI_8$Country == 'HKG', 
                                   c('Country', 'Year', 'dispi_rural','ibuy_rural','ipay_rural','mdply_rural','acnt_rural', 'rdply_rural')]


#####check
check_usd_ppp_data <- Ex_Database_RTMI_8 %>%
  filter(Country %in% c('CHN', 'USA', 'IRN', 'VEN')) %>%
  select(Country, ppp_usd, Year)


#####################################################################################################################################

# CHECK FOR LEFTOVER NA VALUES

#which(is.na(Ex_Database_RTMI_6), arr.ind=TRUE)

total_na_values <- sum(is.na(Ex_Database_RTMI_8))
total_na_values

######################################### update the estimates for binary indicators under Institutions before RTMI, 'natstrat', 'netneut'########
check_before <- Ex_Database_RTMI_8[c('Country', 'Year', 'natstrat', 'netneut', 'naistrat')]

Ex_Database_RTMI_8 <- Ex_Database_RTMI_8 %>%
  left_join(binary_data %>% select(Country, Year, natstrat, netneut, naistrat), by = c("Country", "Year")) %>%
  mutate(
    natstrat = coalesce(natstrat.y, natstrat),
    netneut = coalesce(netneut.y, netneut),
    naistrat = coalesce(naistrat.y, naistrat)
  ) %>%
  select(-c(natstrat.x, netneut.x, naistrat.x, natstrat.y, netneut.y, naistrat.y))

check_after <- Ex_Database_RTMI_8[c('Country', 'Year', 'natstrat', 'netneut', 'naistrat')]

####################################################################################################

##################### 2. Prep and Export Post-RTMI Database #########################################

####################################################################################################
DII_Country <- read_xlsx("DII 4.0_ranked_country_selec_240102.xlsx", sheet = '35 Tableau')

Ex_Database_RTMI_8_125country <- Ex_Database_RTMI_8 %>%
  filter(Country %in% DII_Country$Country) 

Ex_Database_RTMI_LONG <- melt(Ex_Database_RTMI_8_125country, id = c("Country", "MC_Region", "SubRegion", "CountryName", "WB_Region", "Income Group 2022", "Income Group Num", "LGINC","Year")) %>%
  rename(Code = variable)

# # 4. Import InidcatorInfo tab of Rebuild File
# 
# Index_Rebuild <- read_excel("DII 4.0 Supply Rebuild 240115.xlsx")

# 5. Merge to long database bound for export

Ex_Database_RTMI_LONG <- merge(Ex_Database_RTMI_LONG, Index_Rebuild[c("Indicator", "Code",	"Normalizer",	"Cluster", "Component",	"Source")], by = c("Code"), all.x = T, all.y = F)

# 6. Relocate Indicator info reference columns before Year column

Ex_Database_RTMI_LONG <- Ex_Database_RTMI_LONG %>%
  relocate(c("Indicator", "Code",	"Normalizer",	"Cluster",	"Component", 	"Source"), .before = "Year")

# 7. Export Long Format Database with no Estimations
dir.create("Database Prints")

write_xlsx(Ex_Database_RTMI_LONG, paste0("Database Prints/Index_Database_Long_w_Estimations_", Sys.Date(), ".xlsx"), format_headers = F)

##########################################################################################################

#### WE NOW HAVE OUR COMPLETE, PREPPED DATABASE with RTMI Estimations #####

#### GO ON TO CODE SHEET Index_ConfidenceScores_Code to CONTINUE TUTORIAL ###

##########################################################################################################


