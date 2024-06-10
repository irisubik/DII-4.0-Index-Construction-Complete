DII_Country <- read_xlsx("DII 4.0_ranked_country_selec_240102.xlsx", sheet = '35 Tableau')

DII_Country_90 <- DII_Country %>%
  filter(is_in_prior_dii == 'in list')

Ex_Database_RTMI_8_125country <- Ex_Database_RTMI_8 %>%
  filter(Country %in% DII_Country$Country) 

total_na_values <- sum(is.na(Ex_Database_RTMI_8_125country))
total_na_values

n_distinct(Ex_Database_RTMI_8_125country$Country)

which(is.na(Ex_Database_RTMI_8_125country), arr.ind=TRUE)

write_xlsx(Ex_Database_RTMI_8_125country, paste0("Database Prints/Index_Database_w_Estimations_", Sys.Date(), ".xlsx"), format_headers = F)

check <- Ex_Database_RTMI_8_125country %>%
  select(Country, Year, vc_ai)
#####################################################################################
# STEP 15: RTMI Round 5: sub region, Year Group MATCHES

# 3.1: Create indicator means for each MATCH  group
# RTMI_LR: LGINC, Year MATCH
# 
# RTMI_SR <- RTMI_CORE_DATASET %>%
#   # treat each set of income group-year matches as independent datasets
#   group_by(SubRegion, Year) %>%
#   # IF variable is numeric, reduce all rows to mean for each income group-year match
#   summarise_if(is.numeric, function(x) mean(x, na.rm = T)) %>%
#   # Remove group restrictions for future functions
#   ungroup() %>%
#   # replace NaN NA type with NA numeric (weird R neccessity)
#   mutate_if(is.numeric, function(x) ifelse(is.nan(x), NA, x))
# 
# # "NATURAL JOIN" (Replace NA Values of DF1 with datapoints from DF2) for income group-WB_Region-year matches
# # In other words, the dataset matches our missing value dataset (DF1) with our intersect mean dataset (DF2) ALONG income group-WB_Region-year row matches. 
# # IF the row is a missing value, the natural join subs the NA for the intersect mean from DF2
# 
# Ex_Database_RTMI_9 <- natural_join(Ex_Database_RTMI_8, RTMI_SR, by = c("SubRegion", "Year"), jointype = "FULL")
# 
# # Rearrange columns to default order, and arrange rows by listed sort hierarchy
# 
# Ex_Database_RTMI_8 <- Ex_Database_RTMI_8[DATASET_INDICATOR_ORDER] %>%
#   arrange(desc(WB_Region), `Income Group 2020`, Country, Year)
# 
# ##################################################################################################################################
# 
# 
# # STEP 13: Stine Interpolation Round 5
# 
# Ex_Database_RTMI_9 <- Ex_Database_RTMI_9 %>%
#   arrange(Country, Year) %>%
#   group_by(Country) %>% 
#   mutate_at(VARIABLES_TO_BE_ESTIMATED, funs(if(sum(!is.na(.))<2) {.} else{replace(na_interpolation(., option = "stine"), is.na(na.approx(., na.rm=FALSE)), NA)})) %>%
#   ungroup() %>%
#   arrange(desc(WB_Region), `Income Group 2020`, Country, Year)
# 
# #####################################################################################################################################
# 
# # STEP 14: FILL (LVCF) Round 5 where applicable
# 
# Ex_Database_RTMI_9 <- Ex_Database_RTMI_9 %>%
#   arrange(Country, Year) %>%
#   group_by(Country) %>% 
#   fill(VARIABLES_TO_BE_ESTIMATED, .direction = "downup") %>%
#   ungroup() %>%
#   arrange(desc(WB_Region), `Income Group 2020`, Country, Year)
# 
# #####################################################################################################################################
# 
# # CHECK FOR LEFTOVER NA VALUES
# 
# #which(is.na(Ex_Database_RTMI_6), arr.ind=TRUE)
# 
# total_na_values <- sum(is.na(Ex_Database_RTMI_9))
# total_na_values
# 
# Ex_Dat