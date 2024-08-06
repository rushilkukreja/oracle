library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggtext)
library(scales)

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

emissions_plot1 <-
  ggplot(data1, aes(x = Rocket, y = emission_value)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_brewer(palette = "Dark2") + 
  geom_text(data = data_summary1, aes(x = Rocket, y = total_emission, label = comma(total_emission)), 
            vjust = -0.5, size = 3.5) +
  theme_minimal() +
  labs(
    colour = NULL,
    x = NULL,
    fill = "Impact Category",
    title = "Greenhouse Gas Emissions by Launch Vehicle",
    subtitle = "Emissions across various impact categories for all launch vehicles"
  ) +
  ylab("Greenhouse Gas Emissions (kg CO2e)") + 
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0),
    limits = c(0, max(data_summary1$total_emission) * 1.1)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
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
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

filename2 = "adjusted_rocket_emissions.csv"
data2 <- read.csv(file.path(folder, '../results', filename2))

data2 = select(
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

emissions_plot2 <-
  ggplot(data2, aes(x = Rocket, y = emission_value)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_brewer(palette = "Dark2") + 
  geom_text(data = data_summary2, aes(x = Rocket, y = total_emission, label = comma(total_emission)), 
            vjust = -0.5, size = 3.5) +
  theme_minimal() +
  labs(
    colour = NULL,
    x = NULL,
    fill = "Impact Category",
    title = "Greenhouse Gas Emissions per kg Payload by Launch Vehicle",
    subtitle = "Emissions per kg of payload mass across various impact categories for all launch vehicles"
  ) +
  ylab("Greenhouse Gas Emissions (kg CO2e)") + 
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0),
    limits = c(0, max(data_summary2$total_emission) * 1.1)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(
    legend.position = "bottom",
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
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

combined_plot <- ggarrange(emissions_plot1, emissions_plot2, ncol = 1, nrow = 2)

combined_plot <- ggarrange(
  emissions_plot1, emissions_plot2,
  ncol = 1, nrow = 2,
  heights = c(1, 1.1),
  labels = c("A", "B")
)

path_combined = file.path(visualizations, 'd_combined_rocket_emissions.png')
dir.create(visualizations, showWarnings = FALSE)
png(
  path_combined,
  units = "in",
  width = 9,
  height = 11,
  res = 480
)
print(combined_plot)
dev.off()
