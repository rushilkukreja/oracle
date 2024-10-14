# Load the necessary libraries
library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggtext)
library(scales)

# Define folder paths
folder <- dirname(rstudioapi::getSourceEditorContext()$path)
visualizations = file.path(folder, 'figures')

# Load data for constellations emissions
filename = "constellation_emissions.csv"
data <- read.csv(file.path(folder, '../results', filename))

# Preprocess data for constellation emissions
data = select(
  data, 
  Constellation, 
  Launch.Event,
  Launcher.Production,
  Electronics.Production,
  Launcher.Transportation,
  Electricity.Consumption,
  Propulsion.System
)

# Convert columns to numeric
data$Launch.Event <- as.numeric(gsub(",", "", data$Launch.Event))
data$Launcher.Production <- as.numeric(gsub(",", "", data$Launcher.Production))
data$Electronics.Production <- as.numeric(gsub(",", "", data$Electronics.Production))
data$Launcher.Transportation <- as.numeric(gsub(",", "", data$Launcher.Transportation))
data$Electricity.Consumption <- as.numeric(gsub(",", "", data$Electricity.Consumption))
data$Propulsion.System <- as.numeric(gsub(",", "", data$Propulsion.System))

# Pivot data for plotting
data <- pivot_longer(
  data,
  cols = c(Launch.Event, Launcher.Production, Electronics.Production, Launcher.Transportation, Electricity.Consumption, Propulsion.System),
  names_to = "impact_category",
  values_to = "emission_value"
)

# Set factor levels for impact categories
data$impact_category = factor(
  data$impact_category,
  levels = c(
    "Launch.Event", "Launcher.Production", "Electronics.Production",
    "Launcher.Transportation", "Electricity.Consumption", "Propulsion.System"),
  labels = c(
    "Launch Event", "Launcher Production", "Electronics Production",
    "Launcher Transportation", "Electricity Consumption", "Propulsion System"))

# Set factor levels for constellations
data$Constellation = factor(
  data$Constellation,
  levels = c('Astra', 'BlueWalker', 'Cinnamon-937', 'Flock', 'Globalstar', 'Guowang', 'Hanwha', 'Honghu-3', 'HVNET', 'KLEO', 'Kuiper', 'Lacuna', 'Lightspeed', 'Lynk', 'Omni', 'OneWeb', 'Rassvet', 'Semaphore-C', 'SferaCon', 'Starlink (Gen2)', 'Swarm', 'Xingshidai', 'Xingwang', 'Yinhe'),
  labels = c('Astra', 'BlueWalker', 'Cinnamon-937', 'Flock', 'Globalstar', 'Guowang', 'Hanwha', 'Honghu-3', 'HVNET', 'KLEO', 'Kuiper', 'Lacuna', 'Lightspeed', 'Lynk', 'Omni', 'OneWeb', 'Rassvet', 'Semaphore-C', 'SferaCon', 'Starlink (Gen2)', 'Swarm', 'Xingshidai', 'Xingwang', 'Yinhe'))

# Calculate total emissions for constellations
data <- data %>%
  group_by(Constellation) %>%
  mutate(total_emissions = sum(emission_value))

# Calculate proportion of emissions
data <- data %>%
  mutate(proportion = emission_value / total_emissions)

# Plot B - GHG emissions by constellations
c_constellation_emissions <-
  ggplot(data, aes(x = Constellation, y = emission_value)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  labs(
    title = "Greenhouse Gas Emissions by Satellite Megaconstellation",
    subtitle = "Emissions across various impact categories\nfor all satellite megaconstellations with >150 satellites",
    colour = NULL,
    x = NULL,
    fill = NULL # Remove "Impact Category" from legend
  ) +
  ylab("Greenhouse Gas Emissions (kt CO2e)") + 
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0),
    limits = c(0, max_emission1 * 1.1)
  ) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 10),
    axis.line = element_line(colour = "black"),
    strip.text.x = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_markdown(margin = margin(r = 10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) +
  geom_text(data = total_emissions1, aes(x = Constellation, y = total_emission_value, label = comma(total_emission_value)), vjust = -0.5, size = 3)

# Load constellation emissions by country for graph D
filename_country = "constellation_emissions_by_country.csv"
data_country <- read.csv(file.path(folder, '../results', filename_country))

# Preprocess data for emissions by country
data_country = select(
  data_country, 
  Country, 
  Launch.Event,
  Launcher.Production,
  Electronics.Production,
  Launcher.Transportation,
  Electricity.Consumption,
  Propulsion.System
)

# Convert columns to numeric
data_country$Launch.Event <- as.numeric(gsub(",", "", data_country$Launch.Event))
data_country$Launcher.Production <- as.numeric(gsub(",", "", data_country$Launcher.Production))
data_country$Electronics.Production <- as.numeric(gsub(",", "", data_country$Electronics.Production))
data_country$Launcher.Transportation <- as.numeric(gsub(",", "", data_country$Launcher.Transportation))
data_country$Electricity.Consumption <- as.numeric(gsub(",", "", data_country$Electricity.Consumption))
data_country$Propulsion.System <- as.numeric(gsub(",", "", data_country$Propulsion.System))

# Pivot data for plotting
data_country <- pivot_longer(
  data_country,
  cols = c(Launch.Event, Launcher.Production, Electronics.Production, Launcher.Transportation, Electricity.Consumption, Propulsion.System),
  names_to = "impact_category",
  values_to = "emission_value"
)

# Set factor levels for impact categories
data_country$impact_category = factor(
  data_country$impact_category,
  levels = c(
    "Launch.Event", "Launcher.Production", "Electronics.Production",
    "Launcher.Transportation", "Electricity.Consumption", "Propulsion.System"),
  labels = c(
    "Launch Event", "Launcher Production", "Electronics Production",
    "Launcher Transportation", "Electricity Consumption", "Propulsion System"))

# Calculate total emissions by country
total_emissions_country <- data_country %>%
  group_by(Country) %>%
  summarise(total_emission_value = round(mean(emission_value)))  # Calculate average emissions

max_emission_country <- max(total_emissions_country$total_emission_value)

# Plot D - Average GHG emissions by country
d_country_emissions <-
  ggplot(data_country, aes(x = Country, y = emission_value)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  labs(
    title = "Average Greenhouse Gas Emissions by Country",
    subtitle = "Average emissions across various impact categories\nfor all satellite megaconstellations by country",
    colour = NULL,
    x = NULL,
    fill = NULL # Remove "Impact Category" from legend
  ) +
  ylab("Average Greenhouse Gas Emissions (kt CO2e)") + 
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0),
    limits = c(0, max_emission_country * 1.1)
  ) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 10),
    axis.line = element_line(colour = "black"),
    strip.text.x = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_markdown(margin = margin(r = 10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) +
  geom_text(data = total_emissions_country, aes(x = Country, y = total_emission_value, label = comma(total_emission_value)), vjust = -0.5, size = 3)

# Plot C (emissions_plot) - remove legend
emissions_plot <-
  ggplot(data, aes(x = Constellation, y = proportion)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  labs(
    title = "Greenhouse Gas Emissions by Satellite Constellation",
    subtitle = "Percentage of total emissions across various impact categories\nfor all satellite megaconstellations with >150 satellites",
    colour = NULL,
    x = NULL,
    fill = NULL # Remove "Impact Category" from legend
  ) +
  ylab("Percentage of Total Emissions") + 
  scale_y_continuous(labels = percent, expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(
    legend.position = "none", # Remove legend
    legend.spacing.x = unit(0.5, 'cm'),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.line = element_line(colour = "black"),
    strip.text.x = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_markdown(margin = margin(r = 10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 6),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

# Plot F - Remove legend similarly
e_per_launch_emissions <-
  ggplot(data3, aes(x = Constellation, y = emission_value)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  labs(
    title = "Greenhouse Gas Emissions per Launch by Satellite Megaconstellation",
    subtitle = "Emissions per launch across various impact categories\nfor all satellite megaconstellations with >150 satellites",
    colour = NULL,
    x = NULL,
    fill = NULL # Remove "Impact Category" from legend
  ) +
  ylab("Greenhouse Gas Emissions (t CO2e)") + 
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0),
    limits = c(0, max_emission3 * 1.1)
  ) +
  theme(
    legend.position = "none", # Remove legend
    axis.title = element_text(size = 10),
    axis.line = element_line(colour = "black"),
    strip.text.x = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_markdown(margin = margin(r = 10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) +
  geom_text(data = total_emissions3, aes(x = Constellation, y = total_emission_value, label = comma(total_emission_value)), vjust = -0.5, size = 3)

# Arrange the plots in a 2-column, 3-row layout with a common legend at the bottom
f_emissions <- ggarrange(
  c_constellation_emissions, 
  e_per_launch_emissions,
  emissions_plot,
  d_country_emissions,  # New graph D
  e_per_launch_emissions,
  emissions_plot,
  ncol = 2, 
  nrow = 3,
  heights = c(1, 1, 1.1),
  labels = c("A", "B", "C", "D", "E", "F"), # Labeling for each graph
  common.legend = TRUE, 
  legend = "bottom" # Common legend at the bottom
)

# Save the plot as a PNG file
path_combined = file.path(visualizations, 'g_combined_constellation_emissions.png')
dir.create(visualizations, showWarnings = FALSE)
png(
  path_combined,
  units = "in",
  width = 12,  # Adjusting the width for 2-column layout
  height = 12,
  res = 300
)
print(f_emissions)
dev.off()