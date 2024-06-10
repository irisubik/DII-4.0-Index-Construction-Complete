new_database <- read_excel("Database Prints/Index_Database_Long_no_Estimations_2024-04-29.xlsx") %>%
  rename(new_value = value) %>% 
  filter(Year == 2019, Country == 'USA')

old_database <- read_excel("Database Prints/Index_Database_Long_no_Estimations_2024-04-12.xlsx") %>%
  rename(old_value = value) %>% 
  filter(Year == 2019, Country == 'USA')

merged_new_old <- merge(new_database[c('Country','CountryName', 'Year', 'Code', 'new_value')], 
                        old_database[c('Country', 'Year', 'Code', 'old_value')], 
                        by = c('Country', 'Year', 'Code'), all = F)

options(scipen = 999)

merged_new_old <- merged_new_old %>%
  mutate(
    new_value = as.numeric(as.character(new_value)), 
    old_value = as.numeric(as.character(old_value)),
    diff = abs(new_value - old_value)
  )

results <- merged_new_old %>%
  group_by(Code) %>%
  summarise(R_squared = summary(lm(new_value ~ old_value, data = .))$r.squared)

new_check_database <- read_excel("Database Prints/Index_Database_Long_no_Estimations_2024-04-25.xlsx") %>%
  rename(new_value = value) %>% 
  filter(Year == 2019, Code %in% c('gde_rd', 'ecom_mbl'))

old_check_database <- read_excel("Database Prints/Index_Database_Long_no_Estimations_2024-04-12.xlsx") %>%
  rename(old_value = value) %>% 
  filter(Year == 2019, Code %in% c('gde_rd', 'ecom_mbl'))

#####################check cluster level supply 

#####################check supply - ELECTRONIC PAYMENT
new_check_database <- read_excel("Database Prints/Index_Database_Long_no_Estimations_2024-04-26.xlsx") %>%
  rename(new_value = value) %>% 
  filter(Year %in% 2019:2024, Code %in% c('nopos', 'atmpc'))

old_check_database <- read_excel("Database Prints/Index_Database_Long_no_Estimations_2024-04-12.xlsx") %>%
  rename(old_value = value) %>% 
  filter(Year %in% 2019:2024, Code %in% c('nopos', 'atmpc'))

merged_new_old <- merge(new_check_database[c('Country','CountryName', 'Year', 'Code', 'new_value')], 
                        old_check_database[c('Country', 'Year', 'Code', 'old_value')], 
                        by = c('Country', 'Year', 'Code'), all = F)

options(scipen = 999)

merged_new_old <- merged_new_old %>%
  mutate(
    new_value = as.numeric(as.character(new_value)), 
    old_value = as.numeric(as.character(old_value)),
    diff = abs(new_value - old_value)
  )

#####################check supply - Chile, Panama, Uruguay
new_check_database <- read_excel("Database Prints/Index_Database_Long_no_Estimations_2024-05-01.xlsx") %>%
  rename(new_value = value) %>% 
  filter(Code %in% c('hitech'))

old_check_database <- read_excel("Database Prints/Index_Database_Long_no_Estimations_2024-04-12.xlsx") %>%
  rename(old_value = value) %>% 
  filter(Code %in% c('hitech'))

merged_new_old <- merge(new_check_database[c('Country','CountryName', 'Year', 'Code', 'new_value')], 
                        old_check_database[c('Country', 'Year', 'Code', 'old_value')], 
                        by = c('Country', 'Year', 'Code'), all = F)

options(scipen = 999)

merged_new_old <- merged_new_old %>%
  mutate(
    new_value = as.numeric(as.character(new_value)), 
    old_value = as.numeric(as.character(old_value)),
    diff = abs(new_value - old_value)
  )


#####################check driver scores/momentum 
new_check_database <- read_excel("Booklet Prints/Driver_level_Index_Booklet_WIDE_2024-04-29.xlsx") %>%
  filter(Year %in% 2019:2024, Country %in% c('CHL','PAN', 'URY','CHN'))

old_check_database <- read_excel("Booklet Prints/Driver_level_Index_Booklet_WIDE_2024-04-12.xlsx") %>%
  filter(Year %in% 2019:2024, Country %in% c('CHL','PAN', 'URY','CHN'))
#############################################################
Index_Rebuild <- read_excel("DII 4.0 Evolution Rebuild 240419.xlsx")
Index_Rebuild_Clusters <- read_excel("DII 4.0 Evolution Rebuild 240419.xlsx", sheet = "Clusters")
Index_Rebuild_Components <- read_excel("DII 4.0 Evolution Rebuild 240419.xlsx", sheet = "Components")

DII4_index_wide <- read_excel("Booklet Prints/Driver_level_Index_Booklet_WIDE_2024-05-01.xlsx")

#don't change 0313 date!!!!!!!!!!!!!!!!!!!!
DII4_driver_organized <- read_excel("Driver_level_Index_Booklet_WIDE_2024-03-13.xlsx", sheet = "Drivers")
DII4_col_driver_names <- names(DII4_driver_organized)
DII4_driver <- DII4_index_wide[, c(DII4_col_driver_names)]

DII4_cluster_organized <- read_excel("Driver_level_Index_Booklet_WIDE_2024-03-13.xlsx", sheet = "Clusters")
DII4_col_cluster_names <- names(DII4_cluster_organized)
DII4_cluster <- DII4_index_wide[, c(DII4_col_cluster_names)]

DII4_component_organized <- read_excel("Driver_level_Index_Booklet_WIDE_2024-03-13.xlsx", sheet = "Components")
DII4_col_component_names <- names(DII4_component_organized)
DII4_component <- DII4_index_wide[, c(DII4_col_component_names)]

DII4_cluster_long <- melt(DII4_cluster, id.vars = c("Country", "Year"), variable.name = "Cluster", value.name = "DII4_Cluster_Value")

DII4_component_long <- melt(DII4_component, id.vars = c("Country", "Year"), variable.name = "Component", value.name = "DII4_Component_Value")


# Melt the wide-format dataset into the desired format
DII4_driver_long <- melt(DII4_driver,
                         id.vars = c("Year", "Country", "CountryName", "WB_Region", "Income Group 2022", "LGINC", "Demand Zone", "Supply Zone", "Institutional Environment Zone", "Innovation Zone"),
                         measure.vars = c("Demand", "Supply", "Institutional Environment", "Innovation",
                                          "Demand Momentum", "Supply Momentum", "Institutional Environment Momentum", "Innovation Momentum"),
                         variable.name = "DII4_Driver",
                         value.name = "DII4_Driver_Value")

DII4_driver_long <- melt(DII4_driver_long,
                         id.vars = c("Year", "Country", "CountryName", "WB_Region", "Income Group 2022", "LGINC", "DII4_Driver", "DII4_Driver_Value"),
                         measure.vars = c("Demand Zone", "Supply Zone", "Institutional Environment Zone", "Innovation Zone"),
                         variable.name = "DII4_Quadrant",
                         value.name = "DII4_Quadrant_Zone")

# DII4_cluster_long$Cluster <- tolower(DII4_cluster_long$Cluster)



DII4_old_index_wide <- read_excel("Booklet Prints/Driver_level_Index_Booklet_WIDE_2024-04-12.xlsx")

#don't change 0313 date!!!!!!!!!!!!!!!!!!!!
DII4_old_driver_organized <- read_excel("Driver_level_Index_Booklet_WIDE_2024-03-13.xlsx", sheet = "Drivers")
DII4_col_driver_names <- names(DII4_driver_organized)
DII4_old_driver <- DII4_old_index_wide[, c(DII4_col_driver_names)]

DII4_old_cluster_organized <- read_excel("Driver_level_Index_Booklet_WIDE_2024-03-13.xlsx", sheet = "Clusters")
DII4_col_cluster_names <- names(DII4_cluster_organized)
DII4_old_cluster <- DII4_old_index_wide[, c(DII4_col_cluster_names)]

DII4_old_component_organized <- read_excel("Driver_level_Index_Booklet_WIDE_2024-03-13.xlsx", sheet = "Components")
DII4_col_component_names <- names(DII4_component_organized)
DII4_old_component <- DII4_old_index_wide[, c(DII4_col_component_names)]

DII4_old_cluster_long <- melt(DII4_old_cluster, id.vars = c("Country", "Year"), variable.name = "Cluster", value.name = "DII4_old_Cluster_Value")

DII4_old_component_long <- melt(DII4_old_component, id.vars = c("Country", "Year"), variable.name = "Component", value.name = "DII4_old_Component_Value")


# Melt the wide-format dataset into the desired format
DII4_old_driver_long <- melt(DII4_old_driver,
                         id.vars = c("Year", "Country", "CountryName", "WB_Region", "Income Group 2022", "LGINC", "Demand Zone", "Supply Zone", "Institutional Environment Zone", "Innovation Zone"),
                         measure.vars = c("Demand", "Supply", "Institutional Environment", "Innovation",
                                          "Demand Momentum", "Supply Momentum", "Institutional Environment Momentum", "Innovation Momentum"),
                         variable.name = "DII4_Driver",
                         value.name = "DII4_old_Driver_Value")

DII4_old_driver_long <- melt(DII4_old_driver_long,
                         id.vars = c("Year", "Country", "CountryName", "WB_Region", "Income Group 2022", "LGINC", "DII4_Driver", "DII4_old_Driver_Value"),
                         measure.vars = c("Demand Zone", "Supply Zone", "Institutional Environment Zone", "Innovation Zone"),
                         variable.name = "DII4_Quadrant",
                         value.name = "DII4_Quadrant_Zone")

Cluster_DII4_old_vs_DII4 <- merge(DII4_old_cluster_long, DII4_cluster_long, by = c("Country", "Year", "Cluster"))
Cluster_DII4_old_vs_DII4$Cluster_ABS_diff <- abs(Cluster_DII4_old_vs_DII4$DII4_Cluster_Value - Cluster_DII4_old_vs_DII4$DII4_old_Cluster_Value)


Component_DII4_old_vs_DII4 <- merge(DII4_old_component_long, DII4_component_long, by = c("Country", "Year", "Component"))
Component_DII4_old_vs_DII4$Component_ABS_diff <- abs(Component_DII4_old_vs_DII4$DII4_Component_Value - Component_DII4_old_vs_DII4$DII4_old_Component_Value)


#####Database combine 
DII4_database_no_estim <- read_excel("Database Prints/Index_Database_Long_no_Estimations_2024-04-29.xlsx") %>%
  rename(DII4_ind_value = value) 


DII4_old_database_no_estim <- read_excel("Database Prints/Index_Database_Long_no_Estimations_2024-04-12.xlsx") %>%
  rename(DII4_old_ind_value = value)

Database_DII4_old_vs_DII4_no_estim <- merge(DII4_database_no_estim[c("Country", "Year", "Code", "DII4_ind_value")], 
                                            DII4_old_database_no_estim, by = c("Country", "Year", "Code"), all = F)

Database_DII4_old_vs_DII4_no_estim <- Database_DII4_old_vs_DII4_no_estim %>%
  mutate(
    Ind_ABS_diff = if_else(
      is.na(DII4_old_ind_value) | is.na(DII4_ind_value),
      NA_real_,
      abs(as.numeric(DII4_old_ind_value) - as.numeric(DII4_ind_value))
    )
  )

Database_DII4_old_vs_DII4_no_estim <- Database_DII4_old_vs_DII4_no_estim %>%
  filter(Country %in% DII_Country$Country) 

dir.create("QA_2023_scores")

write_xlsx(Cluster_DII4_old_vs_DII4, paste0("QA_2023_scores/Cluster_DII4_old_vs_DII4_2023_", Sys.Date(), ".xlsx"), format_headers = F)
write_xlsx(Database_DII4_old_vs_DII4_no_estim, paste0("QA_2023_scores/Database_DII4_old_vs_DII4_no_estim", Sys.Date(), ".xlsx"), format_headers = F)
##############################################union
#############################################add level columns and rename the columns to align
Database_DII4_old_vs_DII4_no_estim_bind <- Database_DII4_old_vs_DII4_no_estim %>%
  mutate(Level = "Indicator_no_estim_dataset") %>%
  mutate(Component = tolower(Component)) %>%
  rename(DII4_old_value = DII4_old_ind_value, DII4_value = DII4_ind_value, ABS_diff = Ind_ABS_diff)


# Database_DII4_old_vs_DII4_w_estim_bind <- Database_DII4_old_vs_DII4_w_estim %>%
#   mutate(Level = "Indicator_w_estim_dataset") %>%
#   mutate(Component = tolower(Component)) %>%
#   rename(DII4_old_value = DII4_old_ind_value, DII4_value = DII4_ind_value, ABS_diff = Ind_ABS_diff)

# add driver column
Database_DII4_old_vs_DII4_no_estim_bind <- Database_DII4_old_vs_DII4_no_estim_bind %>%
  left_join(distinct(QA_Cluster_DII4_old_vs_DII4[c('Component', 'Driver')]) %>%
              mutate(Component = tolower(Component)),
            by = c("Component" = "Component"))

Database_DII4_old_vs_DII4_w_estim_bind <- Database_DII4_old_vs_DII4_w_estim_bind %>%
  left_join(distinct(QA_Cluster_DII4_old_vs_DII4[c('Component', 'Driver')]) %>%
              mutate(Component = tolower(Component)),
            by = c("Component" = "Component")) 

# add level columns and rename the columns to align
Cluster_DII4_old_vs_DII4_bind <- Cluster_DII4_old_vs_DII4 %>%
  mutate(Level = "Cluster",
         Indicator = NA,
         Indicator_Code = NA) %>%
  rename(DII4_old_value = DII4_old_Cluster_Value, DII4_value = DII4_Cluster_Value, ABS_diff = Cluster_ABS_diff)

Component_DII4_old_vs_DII4_bind <- Component_DII4_old_vs_DII4 %>%
  mutate(Level = "Component", 
         Cluster = NA,
         Indicator = NA,
         Indicator_Code = NA) %>%
  rename(DII4_old_value = DII4_old_Component_Value, DII4_value = DII4_Component_Value, ABS_diff = Component_ABS_diff)

#check columns align
# colnames1 <- colnames(Database_DII4_old_vs_DII4_w_estim_bind)
# colnames2 <- colnames(Cluster_DII4_old_vs_DII4_bind)
# 
# missing_colnames_in_colnames2 <- setdiff(colnames1, colnames2)

# complete_DII4_old_vs_DII4_QA_file_w_estim <- bind_rows(QA_Component_DII4_old_vs_DII4_bind, 
#                                                    QA_Cluster_DII4_old_vs_DII4_bind, 
#                                                    Database_DII4_old_vs_DII4_w_estim_bind)

Database_DII4_old_vs_DII4_no_estim_bind <- Database_DII4_old_vs_DII4_no_estim_bind %>%
  mutate(DII4_value = as.double(DII4_value))

complete_DII4_old_vs_DII4_file_no_estim <- bind_rows(Component_DII4_old_vs_DII4_bind, 
                                                    Cluster_DII4_old_vs_DII4_bind, 
                                                    Database_DII4_old_vs_DII4_no_estim_bind)

write_xlsx(complete_DII4_old_vs_DII4_QA_file_w_estim, paste0("complete_DII4_old_vs_DII4_QA_file_w_estim", Sys.Date(), ".xlsx"), format_headers = F)

write_xlsx(complete_DII4_old_vs_DII4_QA_file_no_estim, paste0("complete_DII4_old_vs_DII4_QA_file_no_estim", Sys.Date(), ".xlsx"), format_headers = F)
