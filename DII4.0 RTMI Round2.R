#####################################################################################################################################

# STEP 6: RTMI Round 2: Income Group MATCHES

# 3.1: Create indicator means for each income group MATCH  group
# RTMI_IG: Income Group MATCH

RTMI_IG <- RTMI_CORE_DATASET %>%
  # treat each set of income group-year matches as independent datasets
  group_by(`Income Group 2022`, Year) %>%
  # IF variable is numeric, reduce all rows to mean for each income group-year match
  summarise_if(is.numeric, function(x) mean(x, na.rm = T)) %>%
  # Remove group restrictions for future functions
  ungroup() %>%
  # replace NaN NA type with NA numeric (weird R neccessity)
  mutate_if(is.numeric, function(x) ifelse(is.nan(x), NA, x))

# "NATURAL JOIN" (Replace NA Values of DF1 with datapoints from DF2) for income group-WB_Region-year matches
# In other words, the dataset matches our missing value dataset (DF1) with our intersect mean dataset (DF2) ALONG income group-WB_Region-year row matches. 
# IF the row is a missing value, the natural join subs the NA for the intersect mean from DF2

Ex_Database_RTMI_6 <- natural_join(Ex_Database_RTMI_5, RTMI_IG, by = c("Income Group 2022", "Year"), jointype = "FULL")

# Rearrange columns to default order, and arrange rows by listed sort hierarchy

Ex_Database_RTMI_6 <- Ex_Database_RTMI_6[DATASET_INDICATOR_ORDER] %>%
  arrange(desc(WB_Region), `Income Group 2022`, Country, Year)

##################################################################################################################################


# STEP 7: Stine Interpolation Round 3

Ex_Database_RTMI_6 <- Ex_Database_RTMI_6 %>%
  arrange(Country, Year) %>%
  group_by(Country) %>% 
  mutate_at(VARIABLES_TO_BE_ESTIMATED, funs(if(sum(!is.na(.))<2) {.} else{replace(na_interpolation(., option = "stine"), is.na(na.approx(., na.rm=FALSE)), NA)})) %>%
  ungroup() %>%
  arrange(desc(WB_Region), `Income Group 2022`, Country, Year)

#####################################################################################################################################

# STEP 8: FILL (LVCF) Round 3 where applicable

Ex_Database_RTMI_6 <- Ex_Database_RTMI_6 %>%
  arrange(Country, Year) %>%
  group_by(Country) %>% 
  fill(VARIABLES_TO_BE_ESTIMATED, .direction = "downup") %>%
  ungroup() %>%
  arrange(desc(WB_Region), `Income Group 2022`, Country, Year)

# Check nan values
total_na_values <- sum(is.na(Ex_Database_RTMI_6))
total_na_values
##########################################################################################################################################

# STEP 7 - 100 : Continue RTMI Rounds with looser and looser restrictions on intersect maches
# Essentially, make larger and larger groups of countries until each country with missing values has a intersect match to pull mean from
# As such, estimations get less and less accurate with each subsequent round
