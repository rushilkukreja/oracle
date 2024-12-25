library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggtext)
library(scales)
library(cowplot)
library(viridis)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
visualizations = file.path(folder, 'figures')
filename1 = "rocket_emissions.csv"
data1 <- read.csv(file.path(folder, '../results', filename1))

data1 = select(
  data1, 
  Rocket, 
  Launch.Event,
  Launcher.Production,
  Electronics.Production,
  Launcher.Transportation,
  Electricity.Consumption
)

data1 <- pivot_longer(
  data1,
  cols = c(Launch.Event, Launcher.Production, Electronics.Production, Launcher.Transportation, Electricity.Consumption),
  names_to = "impact_category",
  values_to = "emission_value"
)

data1$emission_value <- as.numeric(gsub(",", "", data1$emission_value))

data1$impact_category = factor(
  data1$impact_category,
  levels = c(
    "Launch.Event", "Launcher.Production", "Electronics.Production",
    "Launcher.Transportation", "Electricity.Consumption"),
  labels = c(
    "Launch Event", "Launcher Production", "Electronics Production",
    "Launcher Transportation", "Electricity Consumption"))

data1$Rocket = factor(
  data1$Rocket,
  levels = c('Ariane-5', 'Ariane-62', 'Ariane-64', 'Atlas V', 'Falcon-9', 'Falcon-Heavy', 'Long March 5', 'LVM3', 'New Glenn', 'Soyuz-FG', 'Starship', 'Vulcan Centaur'),
  labels = c('Ariane-5', 'Ariane-62', 'Ariane-64', 'Atlas V', 'Falcon-9', 'Falcon-\nHeavy', 'Long\nMarch 5', 'LVM3', 'New\nGlenn', 'Soyuz-FG', 'Starship', 'Vulcan\nCentaur'))

data_summary1 <- data1 %>%
  group_by(Rocket) %>%
  summarise(total_emission = sum(emission_value))

emissions_plot1 <- ggplot(data1, aes(x = Rocket, y = emission_value / 1000)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_viridis_d(option = "plasma", name = NULL) +
  geom_text(data = data_summary1, aes(x = Rocket, y = total_emission / 1000, label = comma(total_emission / 1000)), 
            vjust = -0.5, size = 3.5) +
  theme_minimal() +
  labs(
    colour = NULL,
    x = NULL,
    fill = NULL,
    title = "Greenhouse Gas Emissions by Launch Vehicle",
  ) +
  ylab("Greenhouse Gas Emissions (tons CO2e)") + 
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0),
    limits = c(0, max(data_summary1$total_emission / 1000) * 1.1)
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
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

data2 <- read.csv(file.path(folder, '../results', "adjusted_rocket_emissions.csv"))

data2 <- select(
  data2, 
  Rocket, 
  Launch.Event,
  Launcher.Production,
  Electronics.Production,
  Launcher.Transportation,
  Electricity.Consumption
)

data2 <- pivot_longer(
  data2,
  cols = c(Launch.Event, Launcher.Production, Electronics.Production, Launcher.Transportation, Electricity.Consumption),
  names_to = "impact_category",
  values_to = "emission_value"
)

data2$emission_value <- as.numeric(gsub(",", "", data2$emission_value))

data2$impact_category = factor(
  data2$impact_category,
  levels = c(
    "Launch.Event", "Launcher.Production", "Electronics.Production",
    "Launcher.Transportation", "Electricity.Consumption"),
  labels = c(
    "Launch Event", "Launcher Production", "Electronics Production",
    "Launcher Transportation", "Electricity Consumption"))

data2$Rocket = factor(
  data2$Rocket,
  levels = c('Ariane-5', 'Ariane-62', 'Ariane-64', 'Atlas V', 'Falcon-9', 'Falcon-Heavy', 'Long March 5', 'LVM3', 'New Glenn', 'Soyuz-FG', 'Starship', 'Vulcan Centaur'),
  labels = c('Ariane-5', 'Ariane-62', 'Ariane-64', 'Atlas V', 'Falcon-9', 'Falcon-\nHeavy', 'Long\nMarch 5', 'LVM3', 'New\nGlenn', 'Soyuz-FG', 'Starship', 'Vulcan\nCentaur'))

data_summary2 <- data2 %>%
  group_by(Rocket) %>%
  summarise(total_emission = sum(emission_value))

emissions_plot2 <- ggplot(data2, aes(x = Rocket, y = emission_value / 1000)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_viridis_d(option = "plasma", name = NULL) +
  geom_text(data = data_summary2, aes(x = Rocket, y = total_emission / 1000, label = comma(total_emission / 1000)), 
            vjust = -0.5, size = 3.5) +
  theme_minimal() +
  labs(
    colour = NULL,
    x = NULL,
    fill = NULL,
    title = "Greenhouse Gas Emissions per kg Payload by Launch Vehicle",
  ) +
  ylab("Greenhouse Gas Emissions (tons CO2e)") + 
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0),
    limits = c(0, max(data_summary2$total_emission / 1000) * 1.1)
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
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

reusability_data <- read.csv(file.path(folder, '../results', 'reusability.csv'))

reusability_data <- pivot_longer(
  reusability_data,
  cols = c("Launch.Event", "Launcher.Production", "Electronics.Production", "Launcher.Transportation", "Electricity.Consumption"),
  names_to = "impact_category",
  values_to = "emission_value"
)
reusability_data$Rocket <- factor(
  reusability_data$Rocket,
  levels = c('Falcon-9 Initial', 'Falcon-9 Subsequent', 'Falcon-Heavy Initial', 'Falcon-Heavy Subsequent', 'New Glenn Initial', 'New Glenn Subsequent', 'Starship Initial', 'Starship Subsequent'),
  labels = c('Falcon-9\nInitial', 'Falcon-9\nSubsequent', 'Falcon-Heavy\nInitial', 'Falcon-Heavy\nSubsequent', 'New Glenn\nInitial', 'New Glenn\nSubsequent', 'Starship\nInitial', 'Starship\nSubsequent')
)

reusability_data$emission_value <- as.numeric(gsub(",", "", reusability_data$emission_value))

reusability_data$impact_category = factor(
  reusability_data$impact_category,
  levels = c(
    "Launch.Event", "Launcher.Production", "Electronics.Production",
    "Launcher.Transportation", "Electricity.Consumption"),
  labels = c(
    "Launch Event", "Launcher Production", "Electronics Production",
    "Launcher Transportation", "Electricity Consumption"))

data_summary_reuse <- reusability_data %>%
  group_by(Rocket) %>%
  summarise(total_emission = sum(emission_value))

reusability_plot <- ggplot(reusability_data, aes(x = Rocket, y = emission_value / 1000)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_viridis_d(option = "plasma", name = NULL) +
  geom_text(data = data_summary_reuse, aes(x = Rocket, y = total_emission / 1000, label = comma(total_emission / 1000)), 
            vjust = -0.5, size = 3.5) +
  theme_minimal() +
  labs(
    title = "Impact of Reusability on Launch Emissions",
    y = "Greenhouse Gas Emissions (tons CO2e)"
  ) +
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0),
    limits = c(0, max(data_summary_reuse$total_emission / 1000) * 1.1)
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
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.line.x = element_line(size = 0.15),
    axis.line.y = element_line(size = 0.15),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

reusability2_data <- read.csv(file.path(folder, '../results', 'reusability_rockets.csv'))

reusability2_data <- reusability2_data %>%
  pivot_longer(
    cols = c(Launch.Event, Launcher.Production, Electronics.Production, Launcher.Transportation, Electricity.Consumption),
    names_to = "impact_category",
    values_to = "emission_value"
  ) %>%
  filter(!is.na(emission_value))

reusability2_data$emission_value <- as.numeric(gsub(",", "", reusability2_data$emission_value))

reusability2_data$Rocket <- factor(reusability2_data$Rocket, 
                                   levels = c("Reusable", "Non-Reusable"))

reusability2_data$impact_category <- factor(
  reusability2_data$impact_category,
  levels = c("Launch.Event", "Launcher.Production", "Electronics.Production", "Launcher.Transportation", "Electricity.Consumption"),
  labels = c("Launch Event", "Launcher Production", "Electronics Production", "Launcher Transportation", "Electricity Consumption")
)

reusability_plot2 <- ggplot(reusability2_data, aes(x = Rocket, y = emission_value / 1000, fill = impact_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = comma(emission_value / 1000)), 
            position = position_dodge(0.9), vjust = -0.5, size = 3.5) +
  scale_fill_viridis_d(option = "plasma", name = NULL) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)), 
    limits = c(0, max(reusability2_data$emission_value / 1000) * 1.05), 
    labels = comma
  ) +
  theme_minimal() +
  labs(
    title = "Emissions for Reusable and Non-Reusable Rockets",
    x = NULL,
    y = "Greenhouse Gas Emissions (tons CO2e)"
  ) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 10),
    axis.line = element_line(colour = "black"),
    panel.border = element_blank(),
    axis.title.y = element_markdown(margin = margin(r = 10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

base_theme <- theme(
  legend.position = "none",
  axis.title = element_text(size = 10),
  axis.line = element_line(colour = "black"),
  strip.text.x = element_blank(),
  panel.border = element_blank(),
  axis.title.y = element_markdown(margin = margin(r = 10)),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(size = 10, angle = 45, hjust = 1, face = "italic"),
  axis.text.y = element_text(size = 10),
  axis.line.x = element_line(size = 0.15),
  axis.line.y = element_line(size = 0.15),
  plot.subtitle = element_text(size = 14, hjust = 0.5),
  plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
  plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
)

emissions_plot1 <- emissions_plot1 + base_theme
emissions_plot2 <- emissions_plot2 + base_theme
reusability_plot <- reusability_plot + base_theme
reusability_plot2 <- reusability_plot2 + base_theme

combined_plot <- ggarrange(
  emissions_plot1,
  emissions_plot2,
  reusability_plot,
  reusability_plot2,
  ncol = 2,
  nrow = 2,
  common.legend = TRUE,
  legend = "bottom",
  labels = c("A", "B", "C", "D")
)

path_combined <- file.path(visualizations, 'd_rocket_emissions.png')
png(path_combined,
    units = "in", 
    width = 16,
    height = 12,
    res = 300)
print(combined_plot)
dev.off()