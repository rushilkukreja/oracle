library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggtext)
library(scales)

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

data$Constellation = factor(
  data$Constellation,
  levels = c('Astra', 'BlueWalker', 'Cinnamon-937', 'Flock', 'Globalstar', 'Guowang', 'Hanwha', 'Honghu-3', 'HVNET', 'KLEO', 'Kuiper', 'Lacuna', 'Lightspeed', 'Lynk', 'Omni', 'OneWeb', 'Rassvet', 'Semaphore-C', 'SferaCon', 'Starlink (Gen2)', 'Swarm', 'Xingshidai', 'Xingwang', 'Yinhe'),
  labels = c('Astra', 'BlueWalker', 'Cinnamon-937', 'Flock', 'Globalstar', 'Guowang', 'Hanwha', 'Honghu-3', 'HVNET', 'KLEO', 'Kuiper', 'Lacuna', 'Lightspeed', 'Lynk', 'Omni', 'OneWeb', 'Rassvet', 'Semaphore-C', 'SferaCon', 'Starlink (Gen2)', 'Swarm', 'Xingshidai', 'Xingwang', 'Yinhe'))

data <- data %>%
  group_by(Constellation) %>%
  mutate(total_emissions = sum(emission_value))

data <- data %>%
  mutate(proportion = emission_value / total_emissions)

emissions_plot <-
  ggplot(data, aes(x = Constellation, y = proportion)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  labs(
    title = "Greenhouse Gas Emissions by Satellite Constellation",
    subtitle = "Percentage of total emissions across various impact categories for all satellite megaconstellations with >150 satellites",
    colour = NULL,
    x = NULL,
    fill = "Impact Category"
  ) +
  ylab("Percentage of Total Emissions") + 
  scale_y_continuous(labels = percent, expand = c(0, 0)) +
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
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 6),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

# Add your second and third plots here (keeping the same structure)

# Arrange plots in 3x2 grid
combined_plot <- ggarrange(
  emissions_plot, c_constellation_emissions, e_per_launch_emissions,
  ncol = 2, nrow = 3,
  labels = c("A", "B", "C", "D", "E", "F")
)

# Save combined plot
path_combined = file.path(visualizations, 'combined_constellation_emissions.png')
dir.create(visualizations, showWarnings = FALSE)
png(
  path_combined,
  units = "in",
  width = 12,
  height = 10,
  res = 300
)
print(combined_plot)
dev.off()