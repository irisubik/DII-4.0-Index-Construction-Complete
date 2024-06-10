Index_Booklet_FULL <- read_excel("Booklet Prints/Index_Booklet_FULL_2024-05-08.xlsx") 


Index_Booklet_FULL <- Index_Booklet_FULL %>%
  filter(Country %in% c('IND', 'IDN')) %>% 
  filter(Type %in% c('Momentum', 'Momentum Rank')) %>%
  filter(Year == 2023) %>%
  mutate(value = as.numeric(as.character(value))) %>% 
  select(-IsCountry, -`WB Region`, -`MC Region`,  -`Income Group 2022`, -Country,-Year, -variable)


# Pivot the data to a wide format
Index_Booklet_FULL_wide <- Index_Booklet_FULL %>%
  unite("Level", c(variable_parent, Type, Class, Component), sep = "_") %>%
  pivot_wider(names_from = CountryName, values_from = value)


# Splitting the 'Level' column back into 'Level1' and 'Level1_sub'
Index_Booklet_FULL_wide_sep <- Index_Booklet_FULL_wide %>%
  separate(Level, into = c('variable_parent', 'Type', 'Class', 'Component'), sep = "_")


# Calculate the differences for each level
Index_Booklet_FULL_wide_sep <- Index_Booklet_FULL_wide_sep %>%
  mutate(Diff = abs(Indonesia - India))

write_xlsx(Index_Booklet_FULL_wide_sep, paste0("QA_2023_scores/IND_QA_", Sys.Date(), ".xlsx"), format_headers = F)