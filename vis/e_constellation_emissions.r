library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggtext)
library(scales)
library(viridis)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
visualizations = file.path(folder, 'figures')

filename = "constellation_emissions.csv"
data <- read.csv(file.path(folder, '../results', filename))

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

data$Launch.Event <- as.numeric(gsub(",", "", data$Launch.Event))
data$Launcher.Production <- as.numeric(gsub(",", "", data$Launcher.Production))
data$Electronics.Production <- as.numeric(gsub(",", "", data$Electronics.Production))
data$Launcher.Transportation <- as.numeric(gsub(",", "", data$Launcher.Transportation))
data$Electricity.Consumption <- as.numeric(gsub(",", "", data$Electricity.Consumption))
data$Propulsion.System <- as.numeric(gsub(",", "", data$Propulsion.System))

total_emissions1 <- data %>%
  group_by(Constellation) %>%
  summarise(total_emission_value = sum(
    Launch.Event, Launcher.Production, Electronics.Production, 
    Launcher.Transportation, Electricity.Consumption, Propulsion.System, 
    na.rm = TRUE
  ))

max_emission1 <- max(total_emissions1$total_emission_value, na.rm = TRUE)

data <- pivot_longer(
  data,
  cols = c(Launch.Event, Launcher.Production, Electronics.Production, Launcher.Transportation, Electricity.Consumption, Propulsion.System),
  names_to = "impact_category",
  values_to = "emission_value"
)

data$impact_category = factor(
  data$impact_category,
  levels = c(
    "Launch.Event", "Launcher.Production", "Electronics.Production",
    "Launcher.Transportation", "Electricity.Consumption", "Propulsion.System"),
  labels = c(
    "Launch Event", "Launcher Production", "Electronics Production",
    "Launcher Transportation", "Electricity Consumption", "Propulsion System"))

data <- data %>%
  group_by(Constellation) %>%
  mutate(proportion = emission_value / sum(emission_value, na.rm = TRUE))

c_constellation_emissions <-
  ggplot(data, aes(x = Constellation, y = emission_value)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_viridis_d(option = "plasma") +
  theme_minimal() +
  labs(
    title = "Greenhouse Gas Emissions by Satellite Constellation",
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
    legend.position = "bottom",
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
  geom_text(
    data = total_emissions1, 
    aes(x = Constellation, y = total_emission_value, label = comma(round(total_emission_value))),
    vjust = -0.5, 
    size = 3
  )

filename_per_launch = "per_launch_emissions.csv"
data_per_launch <- read.csv(file.path(folder, '../results', filename_per_launch))

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

data_per_launch$Launch.Event <- as.numeric(gsub(",", "", data_per_launch$Launch.Event))
data_per_launch$Launcher.Production <- as.numeric(gsub(",", "", data_per_launch$Launcher.Production))
data_per_launch$Electronics.Production <- as.numeric(gsub(",", "", data_per_launch$Electronics.Production))
data_per_launch$Launcher.Transportation <- as.numeric(gsub(",", "", data_per_launch$Launcher.Transportation))
data_per_launch$Electricity.Consumption <- as.numeric(gsub(",", "", data_per_launch$Electricity.Consumption))
data_per_launch$Propulsion.System <- as.numeric(gsub(",", "", data_per_launch$Propulsion.System))

total_emissions_per_launch <- data_per_launch %>%
  group_by(Constellation) %>%
  summarise(total_emission_value = sum(
    Launch.Event, Launcher.Production, Electronics.Production, 
    Launcher.Transportation, Electricity.Consumption, Propulsion.System, 
    na.rm = TRUE
  ))

max_emission_per_launch <- max(total_emissions_per_launch$total_emission_value, na.rm = TRUE)

data_per_launch <- pivot_longer(
  data_per_launch,
  cols = c(Launch.Event, Launcher.Production, Electronics.Production, Launcher.Transportation, Electricity.Consumption, Propulsion.System),
  names_to = "impact_category",
  values_to = "emission_value"
)

data_per_launch$impact_category = factor(
  data_per_launch$impact_category,
  levels = c(
    "Launch.Event", "Launcher.Production", "Electronics.Production",
    "Launcher.Transportation", "Electricity.Consumption", "Propulsion.System"),
  labels = c(
    "Launch Event", "Launcher Production", "Electronics Production",
    "Launcher Transportation", "Electricity Consumption", "Propulsion System"))

g_constellation_emissions <-
  ggplot(data_per_launch, aes(x = Constellation, y = emission_value)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_viridis_d(option = "plasma") +
  theme_minimal() +
  labs(
    title = "Greenhouse Gas Emissions per Launch by Satellite Constellation",
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
    legend.position = "bottom",
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
  geom_text(
    data = total_emissions_per_launch, 
    aes(x = Constellation, y = total_emission_value, label = comma(round(total_emission_value))),
    vjust = -0.5, 
    size = 3
  )