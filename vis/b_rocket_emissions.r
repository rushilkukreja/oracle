library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggtext)
library(scales)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
visualizations = file.path(folder, 'figures')
filename = "rocket_emissions.csv"
data <- read.csv(file.path(folder, '../results', filename))

data = select(
  data, 
  Rocket, 
  Launch.Event,
  Launcher.Production,
  Electronics.Production,
  Launcher.Transportation,
  Electricity.Consumption
)

data <- pivot_longer(
  data,
  cols = c(Launch.Event, Launcher.Production, Electronics.Production, Launcher.Transportation, Electricity.Consumption),
  names_to = "impact_category",
  values_to = "emission_value"
)

data$emission_value <- as.numeric(gsub(",", "", data$emission_value))

data$impact_category = factor(
  data$impact_category,
  levels = c(
    "Launch.Event", "Launcher.Production", "Electronics.Production",
    "Launcher.Transportation", "Electricity.Consumption"),
  labels = c(
    "Launch Event", "Launcher Production", "Electronics Production",
    "Launcher Transportation", "Electricity Consumption"))

data$Rocket = factor(
  data$Rocket,
  levels = c('Ariane-5', 'Ariane-62', 'Ariane-64', 'Atlas V', 'Falcon-9', 'Falcon-Heavy', 'Long March 5', 'LVM3', 'New Glenn', 'Soyuz-FG', 'Starship', 'Vulcan Centaur'),
  labels = c('Ariane-5', 'Ariane-62', 'Ariane-64', 'Atlas V', 'Falcon-9', 'Falcon-\nHeavy', 'Long\nMarch 5', 'LVM3', 'New\nGlenn', 'Soyuz-FG', 'Starship', 'Vulcan\nCentaur'))

emissions_plot <-
  ggplot(data, aes(x = Rocket, y = emission_value)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  labs(
    colour = NULL,
    x = NULL,
    fill = "Impact Category"
  ) +
  ylab("Greenhouse Gas Emissions (kg CO2e)") + 
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(
    legend.position = "bottom",
    legend.spacing.x = unit(0.5, 'cm'),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 6),
    axis.title = element_text(size = 6),
    axis.line = element_line(colour = "black"),
    strip.text.x = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_markdown(margin = margin(r = 10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    plot.subtitle = element_text(size = 8, face = "bold"),
    plot.title = element_text(size = 8, face = "bold"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

path = file.path(visualizations, 'b_rocket_emissions.png')
dir.create(visualizations, showWarnings = FALSE)
png(
  path,
  units = "in",
  width = 8.5,
  height = 5,
  res = 480
)
print(emissions_plot)
dev.off()
