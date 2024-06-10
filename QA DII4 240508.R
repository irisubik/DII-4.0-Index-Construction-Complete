#############################################################
Index_Rebuild <- read_excel("DII 4.0 Evolution Rebuild 240419.xlsx")
Index_Rebuild_Clusters <- read_excel("DII 4.0 Evolution Rebuild 240419.xlsx", sheet = "Clusters")
Index_Rebuild_Components <- read_excel("DII 4.0 Evolution Rebuild 240419.xlsx", sheet = "Components")

DII4_index_wide <- read_excel("Booklet Prints/Driver_level_Index_Booklet_WIDE_2024-04-29.xlsx")

#don't change 0313 date!!!!!!!!!!!!!!!!!!!!
DII4_driver_organized <- read_excel("Driver_level_Index_Booklet_WIDE_2024-03-13.xlsx", sheet = "Drivers")
DII4_col_driver_names <- names(DII4_driver_organized)
DII4_driver <- DII4_index_wide[, c(DII4_col_driver_names)]

DII4_cluster_organized <- read_excel("Driver_level_Index_Booklet_WIDE_2024-03-13.xlsx", sheet = "Clusters")
DII4_col_cluster_names <- names(DII4_cluster_organized)
# DII4_cluster <- DII4_index_wide[, c(DII4_col_cluster_names)]
# Create a pattern string to match any column name that contains any of the names in DII4_col_cluster_names
pattern <- paste(DII4_col_cluster_names, collapse = "|")
# Select columns from DII4_index_wide where their names match the pattern
DII4_cluster <- DII4_index_wide[, grep(pattern, names(DII4_index_wide))] %>%
  select(-IsCountry)


DII4_component_organized <- read_excel("Driver_level_Index_Booklet_WIDE_2024-03-13.xlsx", sheet = "Components")
DII4_col_component_names <- names(DII4_component_organized)
pattern <- paste(DII4_col_component_names, collapse = "|")
DII4_component <- DII4_index_wide[, grep(pattern, names(DII4_index_wide))] %>%
  select(-IsCountry)

DII4_cluster_long <- melt(DII4_cluster, id.vars = c("Country", "Year", "CountryName"), variable.name = "Cluster", value.name = "DII4_Cluster_Value")

DII4_component_long <- melt(DII4_component, id.vars = c("Country", "Year", "CountryName"), variable.name = "Component", value.name = "DII4_Component_Value")

Index_Rebuild_Clusters_reference <- Index_Rebuild_Clusters %>%
  select(Driver, Component, Cluster)

DII4_cluster_long_w_reference <- DII4_cluster_long %>% 
  left_join(Index_Rebuild_Clusters_reference, by = 'Cluster')

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
