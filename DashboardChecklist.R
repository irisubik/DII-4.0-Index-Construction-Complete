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

ALL_Ex_INDEX_EP_Combined_LONG <- Driver_Ex_INDEX_EP_Combined_LONG %>%
  bind_rows(Overall_Ex_INDEX_EP_Combined_LONG %>%
              filter(Class %in% c('Overall Score', 'Index', 'Overall Momentum')), .) %>%
  filter(Year == 2023, !Type == 'Zone') %>%
  rename(CountryCode3 = Country)

DEI2023_EP_Combined <- ALL_Ex_INDEX_EP_Combined_LONG

DEI2023_EP_Combined$value <- as.numeric(as.character(DEI2023_EP_Combined$value))

DEI2023_EP_top3 <- DEI2023_EP_Combined %>%
  group_by(CountryCode3, Driver) %>%
  filter(Class == "Cluster") %>%
  filter(Type == "Rank") %>%
  filter(IsCountry == 1) %>%
  # filter(variable_parent != "Postal Delivery") %>%
  slice_min(order_by = value, n = 3, with_ties = F) %>%
  ungroup() %>%
  select(CountryCode3, variable_parent) %>%
  mutate(Top3 = 1)

DEI2023_EP_bottom3 <- DEI2023_EP_Combined %>%
  group_by(CountryCode3, Driver) %>%
  filter(Class == "Cluster") %>%
  filter(Type == "Rank") %>%
  filter(IsCountry == 1) %>%
  # filter(variable_parent != "Postal Delivery") %>%
  slice_max(order_by = value, n = 3, with_ties = F) %>%
  ungroup() %>%
  select(CountryCode3, variable_parent) %>%
  mutate(Bottom3 = 1)

DEI2023_EP_Combined <- merge(DEI2023_EP_Combined, DEI2023_EP_top3, by = c("CountryCode3", "variable_parent"), all.x = T, all.y = F)

DEI2023_EP_Combined <- merge(DEI2023_EP_Combined, DEI2023_EP_bottom3, by = c("CountryCode3", "variable_parent"), all.x = T, all.y = F)

DEI2023_EP_Combined <- DEI2023_EP_Combined %>%
  select(CountryCode3, Year, CountryName, `MC Region`, `Income Group 2022`, IsCountry, variable_parent, variable, Type, value, Class, Component, Driver, Top3, Bottom3) %>%
  arrange(desc(IsCountry), CountryCode3, Class, Driver, Component, variable_parent, Type)

# Create a lookup table for country name to code mappings
country_code_lookup <- c(
  "High income median" = "HM",
  "Low income median" = "LM",
  "Lower middle income median" = "LMM",
  "Upper middle income median" = "UMM",
  "Asia Pacific median" = "APM",
  "Europe & Central Asia median" = "ECAM",
  "Latin America & Caribbean median" = "LACM",
  "Middle East & Africa median" = "MEAM",
  "North America median" = "NAM",
  "Break Out median" = "BOM",
  "Stall Out median" = "SLOM",
  "Stand Out median" = "SOM",
  "Watch Out median" = "WOM",
  "World Wide - Reference" = "WWR",
  "World Wide - Median" = "WWM"
)

DEI2023_EP_Combined <- DEI2023_EP_Combined %>%
  mutate(
    CountryName = if_else(CountryCode3 == "HKG", "Hong Kong", CountryName),
    CountryCode3 = coalesce(country_code_lookup[CountryName], CountryCode3)
  )

DEI2023_EP_Combined <- DEI2023_EP_Combined %>%
  mutate(`MC Region` = case_when(CountryCode3 %in% "ISR" ~ "Middle East & Africa", TRUE ~ as.character(`MC Region`))) %>%
  mutate(CountryName = ifelse(CountryName == "Macedonia", "North Macedonia", CountryName))

dir.create("Booklet Prints")

write_xlsx(DEI2023_EP_Combined, paste0("Booklet Prints/SSU_Booklet_FULL_", Sys.Date(), ".xlsx"), format_headers = F)

library(tidyverse)
library(dplyr)
library(writexl)
library(purrr)
library(dplyr)
library(rqdatatable)
library(countrycode)

DEI2023_EP <- DEI2023_EP_Combined

EP_Checklist <- read_xlsx("Dashboard_Checklist_ISO3_DATE.xlsx") %>%
  filter(Scorecard == 'Digital Evolution')


DEI2023_EP <- DEI2023_EP %>%
  mutate(Scorecard = "Digital Evolution") %>%
  mutate(Type = case_when(
    Type == 'Quartile' ~ "Score Quartile",
    Type == 'Momentum' ~ "Momentum Score",
    TRUE ~ Type
  ))

DEI2023 <- DEI2023_EP 

DEI2023_TOP <- DEI2023 %>%
  filter(Top3 %in% 1) %>%
  filter(Type %in% "Rank") %>%
  mutate(Type = "Top Rank") %>%
  group_by(CountryCode3, Driver) %>%
  arrange(CountryCode3, Driver, value) %>%
  mutate(Ranks = rank(as.numeric(value), ties.method = "first")) %>%
  mutate(Type = paste(Type, Ranks, sep = " ")) %>%
  ungroup() %>%
  select(CountryCode3, Type, Driver, Component, variable_parent, value) %>%
  mutate(Element = variable_parent)

DEI2023_BOTTOM <- DEI2023 %>%
  filter(Bottom3 %in% 1) %>%
  filter(Type %in% "Rank") %>%
  mutate(Type = "Bottom Rank") %>%
  group_by(CountryCode3, Driver) %>%
  arrange(CountryCode3, Driver, value) %>%
  mutate(Ranks = rank(as.numeric(value), ties.method = "first")) %>%
  mutate(Type = paste(Type, Ranks, sep = " ")) %>%
  ungroup() %>%
  select(CountryCode3, Type, Driver, variable_parent, value) %>%
  mutate(Element = variable_parent)

DEI_ZONE <- DEI2023 %>%
  filter(Year %in% 2023) %>%
  filter(Type == 'Zone', IsCountry == 1) %>%
  select(CountryCode3, value) %>%
  group_by(CountryCode3) %>%
  slice(1) %>%
  mutate(variable_parent = "Zone") %>%
  mutate(Type = "Zone") %>%
  mutate(Driver = "Zone") %>%
  mutate(Element = variable_parent)

# years_check <- unique(DEI2023$Year)
# print(years_check)
  
write_xlsx(DEI2023, paste0("Dashboard Checklist/DEI2023_check", Sys.Date(), ".xlsx"))

write_xlsx(DEI2023_TOP, paste0("Dashboard Checklist/DEI2023_TOP_", Sys.Date(), ".xlsx"))
write_xlsx(DEI2023_BOTTOM, paste0("Dashboard Checklist/DEI2023_BOTTOM_", Sys.Date(), ".xlsx"))

DES <- merge(EP_Checklist, DEI2023[, c("CountryCode3", "variable_parent", "Type", "value")], 
             by = c("variable_parent", "Type"), 
             all.x = TRUE, 
             all.y = FALSE)

# Select specified columns
DES <- select(DES, "Element", "Type", "Section", "variable_parent", "ROW", "Order", "Class", "Driver", "Scorecard", "CountryCode3", "value", "Verified (Y/N)" , "Verified by:", "Verified Date:", "PDF Version", "If N, mistake:")

# Fill NA values in CountryCode3 column
DES <- fill(DES, CountryCode3)

# Perform natural joins with DEI2023_TOP, DEI2023_BOTTOM, and DEI_ZONE
DES <- natural_join(DES, DEI2023_TOP, by = c("CountryCode3", "Driver", "Type"), jointype = "LEFT")
DES <- natural_join(DES, DEI2023_BOTTOM, by = c("CountryCode3", "Driver", "Type"), jointype = "LEFT")
DES <- natural_join(DES, DEI_ZONE, by = c("CountryCode3", "Driver", "Type", "Element"), jointype = "LEFT")

# Select specified columns again after the joins
DES <- select(DES, "Element", "Type", "Section", "variable_parent", "ROW", "Order", "Class", "Driver", "Scorecard", "CountryCode3", "value", "Verified (Y/N)" , "Verified by:", "Verified Date:", "PDF Version", "If N, mistake:")

# Mutate the Country column with country names
DES <- mutate(DES, Country = countrycode(CountryCode3, "iso3c", "country.name", nomatch = NULL))

# Arrange the rows by the ROW column
DES <- arrange(DES, ROW)

#########################################################
#########################################################
DEI2023 <- split( DEI2023, f = DEI2023$CountryCode3 )


func <- function(x,y){merge(y, x[c("CountryCode3", "variable_parent", "Type", "value")], by = c("variable_parent", "Type"), all.x = T, all.y = F)}
DES <- lapply(DEI2023, func, EP_Checklist)

DES <- lapply(DES, function(x) select(x, "Element", "Type", "Section", "variable_parent", "ROW", "Order", "Class", "Driver", "Scorecard", "Country", "CountryCode3", "value", "Verified (Y/N)" , "Verified by:", "Verified Date:", "PDF Version", "If N, mistake:"))

DES <- lapply(DES, function(x) fill(x, CountryCode3))

DES <- lapply(DES, function(x) natural_join(x, DEI2023_TOP, by = c("CountryCode3", "Driver", "Type"), jointype = "LEFT"))

DES <- lapply(DES, function(x) natural_join(x, DEI2023_BOTTOM, by = c("CountryCode3", "Driver", "Type"), jointype = "LEFT"))

DES <- lapply(DES, function(x) natural_join(x, DEI_ZONE, by = c("CountryCode3", "Driver", "Type", "Element"), jointype = "LEFT"))

DES <- lapply(DES, function(x) select(x, "Element", "Type", "Section", "variable_parent", "ROW", "Order", "Class", "Driver", "Scorecard", "Country", "CountryCode3", "value", "Verified (Y/N)" , "Verified by:", "Verified Date:", "PDF Version", "If N, mistake:"))

DES <- lapply(DES, function(x) {mutate(x, Country = countrycode(CountryCode3, "iso3c", "country.name", nomatch = NULL))})

DES <- lapply(DES, function(x){arrange(x, ROW)})


dir.create("Dashboard Checklist")

write_xlsx(DES, paste0("Dashboard Checklist/Dashboard_Checlist_ALL_", Sys.Date(), ".xlsx"))

current_directory <- getwd()

setwd("C:/Users/niu02/Desktop/DII 4.0 Index Construction 240319/DII 4.0 Index Construction 240628 - all updates - git - full - ITU2 - PARw2/Dashboard Checklist")

for (i in c(1:110)){
  write_xlsx(DES[i], paste0("Dashboard_Checklist_", print(as.character(names(DES)[i])),"_", Sys.Date(),".xlsx"))}


setwd(current_directory)