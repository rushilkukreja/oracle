# Load necessary libraries
library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggtext)
library(scales)

# Define folder paths
folder <- dirname(rstudioapi::getSourceEditorContext()$path)
visualizations = file.path(folder, 'figures')

# Load data for constellation emissions
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

# Calculate total emissions by constellation before pivoting
total_emissions1 <- data %>%
  group_by(Constellation) %>%
  summarise(total_emission_value = sum(
    Launch.Event, Launcher.Production, Electronics.Production, 
    Launcher.Transportation, Electricity.Consumption, Propulsion.System, 
    na.rm = TRUE
  ))

max_emission1 <- max(total_emissions1$total_emission_value, na.rm = TRUE)

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

# Calculate proportion of emissions
data <- data %>%
  group_by(Constellation) %>%
  mutate(proportion = emission_value / sum(emission_value, na.rm = TRUE))

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
    fill = NULL
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

# Load per_launch_emissions data
filename_per_launch = "per_launch_emissions.csv"
data_per_launch <- read.csv(file.path(folder, '../results', filename_per_launch))

# Preprocess data for per_launch_emissions
data_per_launch = select(
  data_per_launch, 
  Constellation, 
  Launch.Event,
  Launcher.Production,
  Electronics.Production,
  Launcher.Transportation,
  Electricity.Consumption,
  Propulsion.System
)

# Convert columns to numeric
data_per_launch$Launch.Event <- as.numeric(gsub(",", "", data_per_launch$Launch.Event))
data_per_launch$Launcher.Production <- as.numeric(gsub(",", "", data_per_launch$Launcher.Production))
data_per_launch$Electronics.Production <- as.numeric(gsub(",", "", data_per_launch$Electronics.Production))
data_per_launch$Launcher.Transportation <- as.numeric(gsub(",", "", data_per_launch$Launcher.Transportation))
data_per_launch$Electricity.Consumption <- as.numeric(gsub(",", "", data_per_launch$Electricity.Consumption))
data_per_launch$Propulsion.System <- as.numeric(gsub(",", "", data_per_launch$Propulsion.System))

# Calculate total emissions for per launch data
total_emissions_per_launch <- data_per_launch %>%
  group_by(Constellation) %>%
  summarise(total_emission_value = sum(
    Launch.Event, Launcher.Production, Electronics.Production, 
    Launcher.Transportation, Electricity.Consumption, Propulsion.System, 
    na.rm = TRUE
  ))

max_emission_per_launch <- max(total_emissions_per_launch$total_emission_value, na.rm = TRUE)

# Pivot data for plotting
data_per_launch <- pivot_longer(
  data_per_launch,
  cols = c(Launch.Event, Launcher.Production, Electronics.Production, Launcher.Transportation, Electricity.Consumption, Propulsion.System),
  names_to = "impact_category",
  values_to = "emission_value"
)

# Set factor levels for impact categories
data_per_launch$impact_category = factor(
  data_per_launch$impact_category,
  levels = c(
    "Launch.Event", "Launcher.Production", "Electronics.Production",
    "Launcher.Transportation", "Electricity.Consumption", "Propulsion.System"),
  labels = c(
    "Launch Event", "Launcher Production", "Electronics Production",
    "Launcher Transportation", "Electricity Consumption", "Propulsion System"))

# Plot G - Per Launch Emissions
g_constellation_emissions <-
  ggplot(data_per_launch, aes(x = Constellation, y = emission_value)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  labs(
    title = "Greenhouse Gas Emissions Per Launch by Satellite Megaconstellation",
    subtitle = "Emissions across various impact categories\nper launch for satellite megaconstellations",
    colour = NULL,
    x = NULL,
    fill = NULL
  ) +
  ylab("Greenhouse Gas Emissions per Launch (kt CO2e)") + 
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0),
    limits = c(0, max_emission_per_launch * 1.1)
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
  geom_text(data = total_emissions_per_launch, aes(x = Constellation, y = total_emission_value, label = comma(total_emission_value)), vjust = -0.5, size = 3)

# Load data for per_launch_emissions
filename_per_launch = "per_launch_emissions.csv"
data_per_launch <- read.csv(file.path(folder, '../results', filename_per_launch))

# Preprocess data for per_launch_emissions
data_per_launch = select(
  data_per_launch, 
  Constellation, 
  Launch.Event,
  Launcher.Production,
  Electronics.Production,
  Launcher.Transportation,
  Electricity.Consumption,
  Propulsion.System
)

# Convert columns to numeric
data_per_launch$Launch.Event <- as.numeric(gsub(",", "", data_per_launch$Launch.Event))
data_per_launch$Launcher.Production <- as.numeric(gsub(",", "", data_per_launch$Launcher.Production))
data_per_launch$Electronics.Production <- as.numeric(gsub(",", "", data_per_launch$Electronics.Production))
data_per_launch$Launcher.Transportation <- as.numeric(gsub(",", "", data_per_launch$Launcher.Transportation))
data_per_launch$Electricity.Consumption <- as.numeric(gsub(",", "", data_per_launch$Electricity.Consumption))
data_per_launch$Propulsion.System <- as.numeric(gsub(",", "", data_per_launch$Propulsion.System))

# Calculate total emissions by constellation before pivoting
total_emissions_per_launch <- data_per_launch %>%
  group_by(Constellation) %>%
  summarise(total_emission_value = sum(
    Launch.Event, Launcher.Production, Electronics.Production, 
    Launcher.Transportation, Electricity.Consumption, Propulsion.System, 
    na.rm = TRUE
  ))

max_emission_per_launch <- max(total_emissions_per_launch$total_emission_value, na.rm = TRUE)

# Pivot data for plotting
data_per_launch <- pivot_longer(
  data_per_launch,
  cols = c(Launch.Event, Launcher.Production, Electronics.Production, Launcher.Transportation, Electricity.Consumption, Propulsion.System),
  names_to = "impact_category",
  values_to = "emission_value"
)

# Set factor levels for impact categories
data_per_launch$impact_category = factor(
  data_per_launch$impact_category,
  levels = c(
    "Launch.Event", "Launcher.Production", "Electronics.Production",
    "Launcher.Transportation", "Electricity.Consumption", "Propulsion.System"),
  labels = c(
    "Launch Event", "Launcher Production", "Electronics Production",
    "Launcher Transportation", "Electricity Consumption", "Propulsion System"))

# Calculate proportion of emissions
data_per_launch <- data_per_launch %>%
  group_by(Constellation) %>%
  mutate(proportion = emission_value / sum(emission_value, na.rm = TRUE))

# Plot G - Per Launch Emissions
g_constellation_emissions <-
  ggplot(data_per_launch, aes(x = Constellation, y = emission_value)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  labs(
    title = "Greenhouse Gas Emissions Per Launch by Satellite Megaconstellation",
    subtitle = "Emissions across various impact categories\nper launch for satellite megaconstellations",
    colour = NULL,
    x = NULL,
    fill = NULL
  ) +
  ylab("Greenhouse Gas Emissions per Launch (kt CO2e)") + 
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0),
    limits = c(0, max_emission_per_launch * 1.1)
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
  geom_text(data = total_emissions_per_launch, aes(x = Constellation, y = total_emission_value, label = comma(total_emission_value)), vjust = -0.5, size = 3)
# Define folder paths
folder <- dirname(rstudioapi::getSourceEditorContext()$path)
visualizations <- file.path(folder, 'figures')

# Load data for reusability emissions
filename_reusability <- "reusability3.csv"
data_reusability <- read.csv(file.path(folder, '../results', filename_reusability))

# Preprocess data for reusability emissions
data_reusability <- data_reusability %>%
  pivot_longer(
    cols = -Reusable, # Exclude the "Reusable" column
    names_to = "impact_category",
    values_to = "emission_value"
  )

# Set factor levels for impact categories
data_reusability$impact_category <- factor(
  data_reusability$impact_category,
  levels = c(
    "Launch.Event", "Launcher.Production", "Electronics.Production",
    "Launcher.Transportation", "Electricity.Consumption", "Propulsion.System"),
  labels = c(
    "Launch Event", "Launcher Production", "Electronics Production",
    "Launcher Transportation", "Electricity Consumption", "Propulsion System")
)

# Plot H - Emissions for Reusable vs Non-Reusable Rockets
h_reusability_emissions <-
  ggplot(data_reusability, aes(x = Reusable, y = emission_value, fill = impact_category)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  labs(
    title = "Greenhouse Gas Emissions by Reusability",
    subtitle = "Comparison of emissions for reusable vs. non-reusable rockets\nacross various impact categories",
    x = NULL,
    y = "Greenhouse Gas Emissions (kt CO2e)",
    fill = NULL
  ) +
  theme(
    legend.position = "right",
    axis.title = element_text(size = 10),
    axis.line = element_line(colour = "black"),
    strip.text.x = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_markdown(margin = margin(r = 10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 8),
    axis.line.x = element_line(size = 0.15),
    axis.line.y = element_line(size = 0.15),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

# Update the panel plot
f_emissions_updated <- ggarrange(
  c_constellation_emissions, 
  d_country_emissions,
  emissions_plot,
  f_size_vs_emissions,  
  h_reusability_emissions,
  g_constellation_emissions,
  ncol = 2, 
  nrow = 3,
  heights = c(1, 1, 1.1),
  labels = c("A", "B", "C", "D", "E", "F"),
  common.legend = TRUE, 
  legend = "bottom"
)

# Save the updated panel plot
path_combined_updated = file.path(visualizations, 'g_combined_constellation_emissions.png')
png(
  path_combined_updated,
  units = "in",
  width = 12,
  height = 12,
  res = 300
)
print(f_emissions_updated)
dev.off()