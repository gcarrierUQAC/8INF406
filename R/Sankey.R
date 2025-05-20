# Install if necessary
install.packages("networkD3")
install.packages("dplyr")
install.packages("readr")

library(networkD3)
library(dplyr)
library(readr)

source("R/ETL.R")

powerplants <- etl_powerplants("data/global_power_plant_database.csv")

sankey_data <- powerplants %>%
  group_by(primary_fuel, country_long) %>%
  summarise(total_capacity = sum(capacity_mw, na.rm = TRUE)) %>%
  filter(!is.na(primary_fuel) & !is.na(country_long))

# Create unique node list
nodes <- data.frame(
  name = unique(c(sankey_data$primary_fuel, sankey_data$country_long))
)

# Map names to node indices
sankey_data$source <- match(sankey_data$primary_fuel, nodes$name) - 1
sankey_data$target <- match(sankey_data$country_long, nodes$name) - 1

# Prepare links data frame
links <- data.frame(
  source = sankey_data$source,
  target = sankey_data$target,
  value  = sankey_data$total_capacity
)

# Basic Sankey plot
sankeyNetwork(
  Links = links, 
  Nodes = nodes,
  Source = "source", 
  Target = "target",
  Value = "value", 
  NodeID = "name",
  sinksRight = FALSE,
  fontSize = 12, 
  nodeWidth = 30
)
