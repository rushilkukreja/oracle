library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(ggtext)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
visualizations = file.path(folder, 'figures')
dir.create(visualizations, showWarnings = FALSE)

filename1 = "subscribers.csv"
data1 <- read.csv(file.path(folder, '../data/raw', filename1))
data1 = select(data1, Constellation, Subscribers)
data1$Subscribers <- as.numeric(gsub(",", "", data1$Subscribers))
data1$Constellation = factor(
  data1$Constellation,
  levels = c('Astra', 'BlueWalker', 'Cinnamon-937', 'Flock', 'Globalstar', 'Guowang', 'Hanwha', 'Honghu-3', 'HVNET', 'KLEO', 'Kuiper', 'Lacuna', 'Lightspeed', 'Lynk', 'Omni', 'OneWeb', 'Rassvet', 'Semaphore-C', 'SferaCon', 'Starlink (Gen2)', 'Swarm', 'Xingshidai', 'Xingwang', 'Yinhe'),
  labels = c('Astra', 'BlueWalker', 'Cinnamon-937', 'Flock', 'Globalstar', 'Guowang', 'Hanwha', 'Honghu-3', 'HVNET', 'KLEO', 'Kuiper', 'Lacuna', 'Lightspeed', 'Lynk', 'Omni', 'OneWeb', 'Rassvet', 'Semaphore-C', 'SferaCon', 'Starlink (Gen2)', 'Swarm', 'Xingshidai', 'Xingwang', 'Yinhe'))

emissions_plot1 <-
  ggplot(data1, aes(x = Constellation, y = Subscribers)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(
    x = NULL,
    y = "Number of Subscribers",
    title = "Number of Subscribers by Satellite Constellation",
    subtitle = "Subscribers for all satellite megaconstellations with >150 satellites"
  ) +
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0)
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.line = element_line(colour = "black"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

filename2 = "per_subscriber_emissions.csv"
data2 <- read.csv(file.path(folder, '../results', filename2))
data2 = select(
  data2, 
  Constellation, 
  Launch.Event,
  Launcher.Production,
  Electronics.Production,
  Launcher.Transportation,
  Electricity.Consumption,
  Propulsion.System
)
data2 <- pivot_longer(
  data2,
  cols = c(Launch.Event, Launcher.Production, Electronics.Production, Launcher.Transportation, Electricity.Consumption, Propulsion.System),
  names_to = "impact_category",
  values_to = "emission_value"
)
data2$emission_value <- as.numeric(gsub(",", "", data2$emission_value))
data2$impact_category = factor(
  data2$impact_category,
  levels = c(
    "Launch.Event", "Launcher.Production", "Electronics.Production",
    "Launcher.Transportation", "Electricity.Consumption", "Propulsion.System"),
  labels = c(
    "Launch Event", "Launcher Production", "Electronics Production",
    "Launcher Transportation", "Electricity Consumption", "Propulsion System"))
data2$Constellation = factor(
  data2$Constellation,
  levels = c('Astra', 'BlueWalker', 'Cinnamon-937', 'Flock', 'Globalstar', 'Guowang', 'Hanwha', 'Honghu-3', 'HVNET', 'KLEO', 'Kuiper', 'Lacuna', 'Lightspeed', 'Lynk', 'Omni', 'OneWeb', 'Rassvet', 'Semaphore-C', 'SferaCon', 'Starlink (Gen2)', 'Swarm', 'Xingshidai', 'Xingwang', 'Yinhe'),
  labels = c('Astra', 'BlueWalker', 'Cinnamon-937', 'Flock', 'Globalstar', 'Guowang', 'Hanwha', 'Honghu-3', 'HVNET', 'KLEO', 'Kuiper', 'Lacuna', 'Lightspeed', 'Lynk', 'Omni', 'OneWeb', 'Rassvet', 'Semaphore-C', 'SferaCon', 'Starlink (Gen2)', 'Swarm', 'Xingshidai', 'Xingwang', 'Yinhe'))

emissions_plot2 <-
  ggplot(data2, aes(x = Constellation, y = emission_value)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  labs(
    colour = NULL,
    x = NULL,
    fill = "Impact Category",
    title = "Greenhouse Gas Emissions per Subscriber by Satellite Constellation",
    subtitle = "Emissions per subscriber across various impact categories for all satellite megaconstellations with >150 satellites"
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
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.line = element_line(colour = "black"),
    strip.text.x = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_markdown(margin = margin(r = 10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

combined_plot <- ggarrange(
  emissions_plot1, emissions_plot2,
  ncol = 1, nrow = 2,
  labels = c("A", "B")
)

path_combined = file.path(visualizations, 'j_combined_subscriber_emissions.png')
png(
  path_combined,
  units = "in",
  width = 9,
  height = 11,
  res = 480
)
print(combined_plot)
dev.off()