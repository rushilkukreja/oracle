library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggtext)
library(scales)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
visualizations = file.path(folder, 'figures')

filename1 = "constellation_emissions.csv"
data1 <- read.csv(file.path(folder, '../results', filename1))

data1 = select(
  data1, 
  Constellation, 
  Launch.Event,
  Launcher.Production,
  Electronics.Production,
  Launcher.Transportation,
  Electricity.Consumption,
  Propulsion.System
)

data1$Launch.Event <- as.numeric(gsub(",", "", data1$Launch.Event))
data1$Launcher.Production <- as.numeric(gsub(",", "", data1$Launcher.Production))
data1$Electronics.Production <- as.numeric(gsub(",", "", data1$Electronics.Production))
data1$Launcher.Transportation <- as.numeric(gsub(",", "", data1$Launcher.Transportation))
data1$Electricity.Consumption <- as.numeric(gsub(",", "", data1$Electricity.Consumption))
data1$Propulsion.System <- as.numeric(gsub(",", "", data1$Propulsion.System))

data1 <- pivot_longer(
  data1,
  cols = c(Launch.Event, Launcher.Production, Electronics.Production, Launcher.Transportation, Electricity.Consumption, Propulsion.System),
  names_to = "impact_category",
  values_to = "emission_value"
)

data1$impact_category = factor(
  data1$impact_category,
  levels = c(
    "Launch.Event", "Launcher.Production", "Electronics.Production",
    "Launcher.Transportation", "Electricity.Consumption", "Propulsion.System"),
  labels = c(
    "Launch Event", "Launcher Production", "Electronics Production",
    "Launcher Transportation", "Electricity Consumption", "Propulsion System"))

data1$Constellation = factor(
  data1$Constellation,
  levels = c('Astra', 'BlueWalker', 'Cinnamon-937', 'Flock', 'Globalstar', 'Guowang', 'Hanwha', 'Honghu-3', 'HVNET', 'KLEO', 'Kuiper', 'Lacuna', 'Lightspeed', 'Lynk', 'Omni', 'OneWeb', 'Rassvet', 'Semaphore-C', 'SferaCon', 'Starlink (Gen2)', 'Swarm', 'Xingshidai', 'Xingwang', 'Yinhe'),
  labels = c('Astra', 'BlueWalker', 'Cinnamon-937', 'Flock', 'Globalstar', 'Guowang', 'Hanwha', 'Honghu-3', 'HVNET', 'KLEO', 'Kuiper', 'Lacuna', 'Lightspeed', 'Lynk', 'Omni', 'OneWeb', 'Rassvet', 'Semaphore-C', 'SferaCon', 'Starlink (Gen2)', 'Swarm', 'Xingshidai', 'Xingwang', 'Yinhe'))

# Calculate total emissions per constellation
total_emissions1 <- data1 %>%
  group_by(Constellation) %>%
  summarise(total_emission_value = round(sum(emission_value)))

max_emission1 <- max(total_emissions1$total_emission_value)

c_constellation_emissions <-
  ggplot(data1, aes(x = Constellation, y = emission_value)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  labs(
    title = "Greenhouse Gas Emissions by Satellite Megaconstellation",
    subtitle = "Emissions across various impact categories for all satellite megaconstellations with >150 satellites",
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

filename3 = "per_launch_emissions.csv"
data3 <- read.csv(file.path(folder, '../results', filename3))

data3 = select(
  data3, 
  Constellation, 
  Launch.Event,
  Launcher.Production,
  Electronics.Production,
  Launcher.Transportation,
  Electricity.Consumption,
  Propulsion.System
)

data3$Launch.Event <- as.numeric(gsub(",", "", data3$Launch.Event))
data3$Launcher.Production <- as.numeric(gsub(",", "", data3$Launcher.Production))
data3$Electronics.Production <- as.numeric(gsub(",", "", data3$Electronics.Production))
data3$Launcher.Transportation <- as.numeric(gsub(",", "", data3$Launcher.Transportation))
data3$Electricity.Consumption <- as.numeric(gsub(",", "", data3$Electricity.Consumption))
data3$Propulsion.System <- as.numeric(gsub(",", "", data3$Propulsion.System))

data3 <- pivot_longer(
  data3,
  cols = c(Launch.Event, Launcher.Production, Electronics.Production, Launcher.Transportation, Electricity.Consumption, Propulsion.System),
  names_to = "impact_category",
  values_to = "emission_value"
)

data3$impact_category = factor(
  data3$impact_category,
  levels = c(
    "Launch.Event", "Launcher.Production", "Electronics.Production",
    "Launcher.Transportation", "Electricity.Consumption", "Propulsion.System"),
  labels = c(
    "Launch Event", "Launcher Production", "Electronics Production",
    "Launcher Transportation", "Electricity Consumption", "Propulsion System"))

data3$Constellation = factor(
  data3$Constellation,
  levels = c('Astra', 'BlueWalker', 'Cinnamon-937', 'Flock', 'Globalstar', 'Guowang', 'Hanwha', 'Honghu-3', 'HVNET', 'KLEO', 'Kuiper', 'Lacuna', 'Lightspeed', 'Lynk', 'Omni', 'OneWeb', 'Rassvet', 'Semaphore-C', 'SferaCon', 'Starlink (Gen2)', 'Swarm', 'Xingshidai', 'Xingwang', 'Yinhe'),
  labels = c('Astra', 'BlueWalker', 'Cinnamon-937', 'Flock', 'Globalstar', 'Guowang', 'Hanwha', 'Honghu-3', 'HVNET', 'KLEO', 'Kuiper', 'Lacuna', 'Lightspeed', 'Lynk', 'Omni', 'OneWeb', 'Rassvet', 'Semaphore-C', 'SferaCon', 'Starlink (Gen2)', 'Swarm', 'Xingshidai', 'Xingwang', 'Yinhe'))

# Calculate total emissions per constellation
total_emissions3 <- data3 %>%
  group_by(Constellation) %>%
  summarise(total_emission_value = round(sum(emission_value)))

max_emission3 <- max(total_emissions3$total_emission_value)

e_per_launch_emissions <-
  ggplot(data3, aes(x = Constellation, y = emission_value)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_brewer(palette = "Dark2") + 
  theme_minimal() +
  labs(
    title = "Greenhouse Gas Emissions per Launch by Satellite Megaconstellation",
    subtitle = "Emissions per launch across various impact categories for all satellite megaconstellations with >150 satellites",
    colour = NULL,
    x = NULL,
    fill = "Impact Category"
  ) +
  ylab("Greenhouse Gas Emissions (t CO2e)") + 
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0),
    limits = c(0, max_emission3 * 1.1)
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
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) +
  geom_text(data = total_emissions3, aes(x = Constellation, y = total_emission_value, label = comma(total_emission_value)), vjust = -0.5, size = 3)

f_emissions <- ggarrange(
  c_constellation_emissions, 
  e_per_launch_emissions,
  ncol = 1, 
  nrow = 2,
  heights = c(1, 1.2),
  labels = c("A", "B")
)

path_combined = file.path(visualizations, 'g_combined_constellation_emissions.png')
dir.create(visualizations, showWarnings = FALSE)
png(
  path_combined,
  units = "in",
  width = 9,
  height = 11,
  res = 300
)
print(f_emissions)
dev.off()
