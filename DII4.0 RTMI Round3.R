# STEP 9: RTMI Round 3: LGINC, Region, Year Group MATCHES

# 3.1: Create indicator means for each income group MATCH  group
# RTMI_LGR: LGINC, Region, Year MATCH

RTMI_LGR <- RTMI_CORE_DATASET %>%
  # treat each set of income group-year matches as independent datasets
  group_by(WB_Region, LGINC, Year) %>%
  # IF variable is numeric, reduce all rows to mean for each income group-year match
  summarise_if(is.numeric, function(x) mean(x, na.rm = T)) %>%
  # Remove group restrictions for future functions
  ungroup() %>%
  # replace NaN NA type with NA numeric (weird R neccessity)
  mutate_if(is.numeric, function(x) ifelse(is.nan(x), NA, x))

# "NATURAL JOIN" (Replace NA Values of DF1 with datapoints from DF2) for income group-WB_Region-year matches
# In other words, the dataset matches our missing value dataset (DF1) with our intersect mean dataset (DF2) ALONG income group-WB_Region-year row matches. 
# IF the row is a missing value, the natural join subs the NA for the intersect mean from DF2

Ex_Database_RTMI_7 <- natural_join(Ex_Database_RTMI_6, RTMI_LGR, by = c("WB_Region", "LGINC", "Year"), jointype = "FULL")

# Rearrange columns to default order, and arrange rows by listed sort hierarchy

Ex_Database_RTMI_7 <- Ex_Database_RTMI_7[DATASET_INDICATOR_ORDER] %>%
  arrange(desc(WB_Region), `Income Group 2022`, Country, Year)

##################################################################################################################################


# STEP 10: Stine Interpolation Round 4

Ex_Database_RTMI_7 <- Ex_Database_RTMI_7 %>%
  arrange(Country, Year) %>%
  group_by(Country) %>% 
  mutate_at(VARIABLES_TO_BE_ESTIMATED, funs(if(sum(!is.na(.))<2) {.} else{replace(na_interpolation(., option = "stine"), is.na(na.approx(., na.rm=FALSE)), NA)})) %>%
  ungroup() %>%
  arrange(desc(WB_Region), `Income Group 2022`, Country, Year)

#####################################################################################################################################

# STEP 11: FILL (LVCF) Round 4 where applicable

Ex_Database_RTMI_7 <- Ex_Database_RTMI_7 %>%
  arrange(Country, Year) %>%
  group_by(Country) %>% 
  fill(VARIABLES_TO_BE_ESTIMATED, .direction = "downup") %>%
  ungroup() %>%
  arrange(desc(WB_Region), `Income Group 2022`, Country, Year)

# Check nan values
total_na_values <- sum(is.na(Ex_Database_RTMI_7))
total_na_values
