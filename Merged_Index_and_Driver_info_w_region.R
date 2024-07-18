State_Wide <- State_Wide %>% 
  select(Country, CountryName, Year, `Index Zone`, `Index Score`, `Index Score Momentum`, `Index Score Rank`, `Index Score Momentum Rank`)

Merged_State_Driver <- State_Wide %>%
  left_join(Driver_Wide%>%select(-CountryName), by = c("Country", "Year"))

Merged_State_Driver <- Merged_State_Driver %>%
  # order vector first, then everything else
  select(Order_Vector, everything()) %>%
  # any columns from reference info sheet before column year
  relocate(names(increg_wbmc), .before = "Year") %>%
  # Zone columns after column year
  relocate(unname(vars_select(names(Ex_INDEX_EP_Combined), contains("Zone"))), .after = "Year") %>%
  # IsCountry? Indicator column immediately after country code
  relocate("IsCountry", .after = "Country") %>%
  #Index after Year****
  relocate(unname(vars_select(names(State_Wide), contains("Index"))), .after = "Year") %>%
  # Sort observations by following hierarchy
  arrange(desc(IsCountry), Country, Year) %>%
  #iscountry -> NA(since leftjoin) -> 0
  mutate(IsCountry = ifelse(is.na(IsCountry), 0, IsCountry))
  
dir.create("Booklet Prints")

write_xlsx(Merged_State_Driver, paste0("Booklet Prints/Booklet_w_regional_info_", Sys.Date(), ".xlsx"), format_headers = F)

###########################################################################
###########################################################################prepare file for SSU 
DEI2022_Scores_SSU_FULL_z <- Merged_State_Driver %>%
  filter(IsCountry == 1) %>%
  group_by(Year, `Index Zone`) %>%
  summarise(`Index Score` = median(`Index Score`), 
            `Demand` = median(`Demand`),
            `Supply` = median(`Supply`),
            `Institutional Environment` = median(`Institutional Environment`),
            `Innovation` = median(`Innovation`),
            `Index Score Momentum` = median(`Index Score Momentum`),
            `Demand Momentum` = median(`Demand Momentum`),
            `Supply Momentum` = median(`Supply Momentum`),
            `Institutional Environment Momentum` = median(`Institutional Environment Momentum`),
            `Innovation Momentum` = median(`Innovation Momentum`)) %>%
  ungroup() %>%
  mutate(CountryName = paste(`Index Zone`, "median", sep = " "), IsCountry = 0) %>%
  bind_rows(Merged_State_Driver, .)


DEI2022_Scores_SSU_FULL_f <- Merged_State_Driver %>%
  filter(IsCountry == 1) %>%
  group_by(Year) %>%
  summarise(`Index Score` = quantile(`Index Score`, probs = 2/3, na.rm = TRUE), 
            `Demand` = quantile(`Demand`, probs = 2/3, na.rm = TRUE),
            `Supply` = quantile(`Supply`, probs = 2/3, na.rm = TRUE), #0.35
            `Institutional Environment` = quantile(`Institutional Environment`, probs = 2/3, na.rm = TRUE),
            `Innovation` = quantile(`Innovation`, probs = 2/3, na.rm = TRUE),
            `Index Score Momentum` = median(`Index Score Momentum`),
            `Demand Momentum` = median(`Demand Momentum`),
            `Supply Momentum` = median(`Supply Momentum`),
            `Institutional Environment Momentum` = median(`Institutional Environment Momentum`),
            `Innovation Momentum` = median(`Innovation Momentum`)) %>%
  ungroup() %>%
  mutate(CountryName = "World Wide - Reference", IsCountry = 0, Country = "WWR") %>%
  bind_rows(DEI2022_Scores_SSU_FULL_z, .)


DEI2022_Scores_SSU_FULL <- DEI2022_Scores_SSU_FULL_f %>%
  # mutate(IsCountry = case_when(IsCountry %in% NA_real_ ~ 0, TRUE ~ 1)) %>%
  mutate_at(vars(contains("Rank")), funs(case_when(Country %in% NA_real_ ~ NA_real_, TRUE ~ as.numeric(.)))) %>%
  mutate(iso3c = Country) %>%
  mutate(iso2c = countrycode(iso3c, "iso3c", "iso2c", nomatch = NULL)) %>%
  # mutate(`Country Name` = countrycode(Country, "iso3c", "country.name", nomatch = NULL)) %>%
  rename(Region = MC_Region)

# DEI2022_Scores_SSU_FULL_check <- DEI2022_Scores_SSU_FULL %>%
#   filter(Year == 2023) %>%
#   select(Year, `Country Name`, `CountryName`) %>%
#   mutate(is_same = ifelse((`Country Name`) == `CountryName`, 1, 0))
#   

DEI2022_Scores_SSU_FULL <- DEI2022_Scores_SSU_FULL %>%
  select(-Country) %>%
  dplyr::rename(CountryCode = iso2c, 
                CountryCode3 = iso3c, 
                # CountryName = `Country Name`,
                IncomeGroup = `Income Group 2022`,
                
                Quadrant = `Index Zone`, 
                DEIScore = `Index Score`, 
                MOMScore = `Index Score Momentum`, 
                DEIRank = `Index Score Rank`, 
                MOMRank = `Index Score Momentum Rank`,
                
                Supply_DEIScore = `Supply`,
                Demand_DEIScore = `Demand`,
                Institutions_DEIScore = `Institutional Environment`,
                Innovation_DEIScore = `Innovation`,
                
                Supply_DEIRank = `Supply Rank`,
                Demand_DEIRank = `Demand Rank`,
                Institutions_DEIRank = `Institutional Environment Rank`,
                Innovation_DEIRank = `Innovation Rank`,
                
                Demand_Quadrant = `Demand Zone`,
                Supply_Quadrant = `Supply Zone`,
                Innovation_Quadrant = `Innovation Zone`,
                Institutions_Quadrant = `Institutional Environment Zone`,
                
                Demand_MOMScore = `Demand Momentum`,
                Supply_MOMScore = `Supply Momentum`,
                Institutions_MOMScore = `Institutional Environment Momentum`,
                Innovation_MOMScore = `Innovation Momentum`,
                
                Demand_MOMRank = `Demand Momentum Rank`,
                Supply_MOMRank = `Supply Momentum Rank`,
                Institutions_MOMRank = `Institutional Environment Momentum Rank`,
                Innovation_MOMRank = `Innovation Momentum Rank`) %>%
  #dplyr::select(Year, iso2c, iso3c, `Country Name`, Region, `Income Group 2022`, Zone, IsCountry, `DEI Score`, `DEI Momentum`, `DEI Score Rank`, `DEI Momentum Rank`, everything())
  dplyr::select(Year, CountryCode, CountryCode3, CountryName, Region, IncomeGroup, Quadrant, IsCountry, DEIScore,	MOMScore,	DEIRank, MOMRank, Supply_DEIScore, Supply_DEIRank, Supply_Quadrant, Demand_DEIScore, Demand_DEIRank, Demand_Quadrant, Institutions_DEIScore, 
                Institutions_DEIRank, Institutions_Quadrant, Innovation_DEIScore, Innovation_DEIRank, Innovation_Quadrant, Supply_MOMScore, Supply_MOMRank, Demand_MOMScore, Demand_MOMRank, Institutions_MOMScore, Institutions_MOMRank, Innovation_MOMScore, Innovation_MOMRank)

DEI2022_Scores_SSU_FULL <- DEI2022_Scores_SSU_FULL %>%
  mutate(CountryName = case_when(CountryCode3 %in% "HKG" ~ "Hong Kong", TRUE ~ as.character(CountryName))) %>%
  mutate(CountryCode3 = case_when(CountryName %in% "High income median" ~ "HM", TRUE ~ as.character(CountryCode3))) %>%
  mutate(CountryCode3 = case_when(CountryName %in% "Low income median" ~ "LM", TRUE ~ as.character(CountryCode3))) %>%
  mutate(CountryCode3 = case_when(CountryName %in% "Lower middle income median" ~ "LMM", TRUE ~ as.character(CountryCode3))) %>%
  mutate(CountryCode3 = case_when(CountryName %in% "Upper middle income median" ~ "UMM", TRUE ~ as.character(CountryCode3))) %>%
  mutate(CountryCode3 = case_when(CountryName %in% "Asia Pacific median" ~ "APM", TRUE ~ as.character(CountryCode3))) %>%
  mutate(CountryCode3 = case_when(CountryName %in% "Europe & Central Asia median" ~ "ECAM", TRUE ~ as.character(CountryCode3))) %>%
  mutate(CountryCode3 = case_when(CountryName %in% "Latin America & Caribbean median" ~ "LACM", TRUE ~ as.character(CountryCode3))) %>%
  mutate(CountryCode3 = case_when(CountryName %in% "Middle East & Africa median" ~ "MEAM", TRUE ~ as.character(CountryCode3))) %>%
  mutate(CountryCode3 = case_when(CountryName %in% "North America median" ~ "NAM", TRUE ~ as.character(CountryCode3))) %>%
  mutate(CountryCode3 = case_when(CountryName %in% "Break Out median" ~ "BOM", TRUE ~ as.character(CountryCode3))) %>%
  mutate(CountryCode3 = case_when(CountryName %in% "Stall Out median" ~ "SLOM", TRUE ~ as.character(CountryCode3))) %>%
  mutate(CountryCode3 = case_when(CountryName %in% "Stand Out median" ~ "SOM", TRUE ~ as.character(CountryCode3))) %>%
  mutate(CountryCode3 = case_when(CountryName %in% "Watch Out median" ~ "WOM", TRUE ~ as.character(CountryCode3))) %>%
  mutate(CountryCode3 = case_when(CountryName %in% "World Wide - Reference" ~ "WWR", TRUE ~ as.character(CountryCode3))) %>%
  mutate(CountryCode3 = case_when(CountryName %in% "World Wide - Median" ~ "WWM", TRUE ~ as.character(CountryCode3))) %>%
  mutate(CountryCode = case_when(CountryName %in% "High income median" ~ "HM", TRUE ~ as.character(CountryCode))) %>%
  mutate(CountryCode = case_when(CountryName %in% "Low income median" ~ "LM", TRUE ~ as.character(CountryCode))) %>%
  mutate(CountryCode = case_when(CountryName %in% "Lower middle income median" ~ "LMM", TRUE ~ as.character(CountryCode))) %>%
  mutate(CountryCode = case_when(CountryName %in% "Upper middle income median" ~ "UMM", TRUE ~ as.character(CountryCode))) %>%
  mutate(CountryCode = case_when(CountryName %in% "Asia Pacific median" ~ "APM", TRUE ~ as.character(CountryCode))) %>%
  mutate(CountryCode = case_when(CountryName %in% "Europe & Central Asia median" ~ "ECAM", TRUE ~ as.character(CountryCode))) %>%
  mutate(CountryCode = case_when(CountryName %in% "Latin America & Caribbean median" ~ "LACM", TRUE ~ as.character(CountryCode))) %>%
  mutate(CountryCode = case_when(CountryName %in% "Middle East & Africa median" ~ "MEAM", TRUE ~ as.character(CountryCode))) %>%
  mutate(CountryCode = case_when(CountryName %in% "North America median" ~ "NAM", TRUE ~ as.character(CountryCode))) %>%
  mutate(CountryCode = case_when(CountryName %in% "Break Out median" ~ "BOM", TRUE ~ as.character(CountryCode))) %>%
  mutate(CountryCode = case_when(CountryName %in% "Stall Out median" ~ "SLOM", TRUE ~ as.character(CountryCode))) %>%
  mutate(CountryCode = case_when(CountryName %in% "Stand Out median" ~ "SOM", TRUE ~ as.character(CountryCode))) %>%
  mutate(CountryCode = case_when(CountryName %in% "Watch Out median" ~ "WOM", TRUE ~ as.character(CountryCode))) %>%
  mutate(CountryCode = case_when(CountryName %in% "World Wide - Reference" ~ "WR", TRUE ~ as.character(CountryCode))) %>%
  mutate(CountryCode = case_when(CountryName %in% "World Wide - Median" ~ "WW", TRUE ~ as.character(CountryCode))) %>%
  mutate(Region = case_when(CountryName %in% "World Wide - Reference" ~ "WR", TRUE ~ as.character(Region))) %>%
  mutate(Region = case_when(CountryName %in% "World Wide - Median" ~ "WW", TRUE ~ as.character(Region)))

DEI2022_Scores_SSU_FULL <- DEI2022_Scores_SSU_FULL %>%
  mutate(Region = case_when(CountryCode3 %in% "ISR" ~ "Middle East & Africa", TRUE ~ as.character(Region))) %>%
  mutate(CountryName = ifelse(CountryName == "Macedonia", "North Macedonia", CountryName))

DEI2022_Scores_SSU_FULL <- DEI2022_Scores_SSU_FULL %>%
  mutate_if(is.numeric, function(x) round(x, digits = 2)) %>%
  filter(!grepl("mean", CountryName, ignore.case = TRUE))

dir.create("To SSU")
write_xlsx(DEI2022_Scores_SSU_FULL, format_headers = F, paste0("To SSU/DEI2022_SSU_Booklet_", Sys.Date(), ".xlsx"))