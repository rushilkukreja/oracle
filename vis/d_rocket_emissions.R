library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggtext)
library(scales)

neutral_colors <- c(
  "Launch Event" = "#4F81BD",
  "Launcher Production" = "#70AD47",
  "Electronics Production" = "#C0504D",
  "Launcher Transportation" = "#6A5ACD",
  "Electricity Consumption" = "#FF8C00",
  "Initial Launch" = "#4F81BD",
  "Subsequent Launch" = "#70AD47"
)

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

# Graph A
emissions_plot1 <-
  ggplot(data1, aes(x = Rocket, y = emission_value)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_manual(values = neutral_colors, name = NULL) +
  geom_text(data = data_summary1, aes(x = Rocket, y = total_emission, label = comma(total_emission)), 
            vjust = -0.5, size = 3.5) +
  theme_minimal() +
  labs(
    colour = NULL,
    x = NULL,
    fill = NULL,
    title = "Greenhouse Gas Emissions by Launch Vehicle",
    subtitle = "Emissions across various impact categories for all launch vehicles"
  ) +
  ylab("Greenhouse Gas Emissions (kg CO2e)") + 
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0),
    limits = c(0, max(data_summary1$total_emission) * 1.1)
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

# Graph B
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

emissions_plot2 <-
  ggplot(data2, aes(x = Rocket, y = emission_value)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_manual(values = neutral_colors, name = NULL) +
  geom_text(data = data_summary2, aes(x = Rocket, y = total_emission, label = comma(total_emission)), 
            vjust = -0.5, size = 3.5) +
  theme_minimal() +
  labs(
    colour = NULL,
    x = NULL,
    fill = NULL,
    title = "Greenhouse Gas Emissions per kg Payload by Launch Vehicle",
    subtitle = "Emissions per kg of payload mass across various impact categories for all launch vehicles"
  ) +
  ylab("Greenhouse Gas Emissions (kg CO2e)") + 
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0),
    limits = c(0, max(data_summary2$total_emission) * 1.1)
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

# Graph C
data1_relative <- data1 %>%
  group_by(Rocket) %>%
  mutate(relative_emission = emission_value / sum(emission_value))

emissions_plot3 <-
  ggplot(data1_relative, aes(x = Rocket, y = relative_emission, fill = impact_category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = neutral_colors, name = NULL) +
  theme_minimal() +
  labs(
    colour = NULL,
    x = NULL,
    fill = NULL,
    title = "Relative Emissions by Launch Vehicle",
    subtitle = "Proportional Distribution of Emissions by Rocket Across Key Impact Categories"
  ) +
  ylab("Relative Emissions (Percentage)") + 
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
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

# Graph D
reusability_data <- read.csv(file.path(folder, '../results', 'reusability.csv'))

reusability_data <- reusability_data %>%
  filter(Rocket %in% c('Falcon-9', 'Falcon-Heavy', 'New Glenn', 'Starship')) %>%
  pivot_longer(cols = c("Initial.Launch", "Subsequent.Launch"),
               names_to = "Launch_Type", values_to = "emission_value")

reusability_data$emission_value <- as.numeric(gsub(",", "", reusability_data$emission_value))

reusability_data$Launch_Type <- factor(reusability_data$Launch_Type,
                                       levels = c("Initial.Launch", "Subsequent.Launch"),
                                       labels = c("Initial Launch", "Subsequent Launch"))

reusability_plot <- ggplot(reusability_data, aes(x = Rocket, y = emission_value, fill = Launch_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = comma(emission_value)), 
            position = position_dodge(0.9), vjust = -0.5, size = 3.5) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_fill_manual(values = c("Initial Launch" = "#C0504D", "Subsequent Launch" = "#70AD47")) +
  theme_minimal() +
  labs(
    title = "Impact of Reusability on Launch Emissions",
    subtitle = "Comparing emissions between initial and subsequent launches for reusable rockets",
    x = "Rocket",
    y = "Greenhouse Gas Emissions (kg CO2e)",
    fill = NULL
  ) +
  theme(
    legend.position = "bottom",
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

# Graph E
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

reusability_plot2 <- ggplot(reusability2_data, aes(x = Rocket, y = emission_value, fill = impact_category)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = comma(emission_value)), 
            position = position_dodge(0.9), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = neutral_colors) +
  theme_minimal() +
  labs(
    title = "Emissions Comparison between Reusable and Non-Reusable Rockets",
    subtitle = "Emissions across various impact categories",
    x = NULL,
    y = "Greenhouse Gas Emissions (kg CO2e)"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
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

legend <- get_legend(
  emissions_plot2 + theme(legend.position = "bottom")
)

combined_plot <- ggarrange(emissions_plot1, emissions_plot2, emissions_plot3, reusability_plot, reusability_plot2, emissions_plot2, 
                           ncol = 2, nrow = 3, 
                           labels = c("A", "B", "C", "D", "E", "F"))

# Graph F
size_emissions_data <- read.csv(file.path(folder, '../results', 'size_emissions.csv'))

size_emissions_data$Dry.Mass <- as.numeric(gsub(",", "", size_emissions_data$Dry.Mass))
size_emissions_data$Emissions <- as.numeric(gsub(",", "", size_emissions_data$Emissions))

scatter_plot_f <- ggplot(size_emissions_data, aes(x = Dry.Mass, y = Emissions, label = Rocket)) +
  geom_point(color = "blue", size = 3) +
  geom_text(aes(label = Rocket), vjust = -1, size = 3.5) +
  theme_minimal() +
  labs(
    title = "Dry Mass vs Emissions by Rocket",
    subtitle = "Scatterplot showing the relationship between Dry Mass and Emissions for each rocket",
    x = "Dry Mass (kg)",
    y = "Greenhouse Gas Emissions (kg CO2e)"
  ) +
  theme(
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

combined_plot <- ggarrange(emissions_plot1, emissions_plot2, emissions_plot3, reusability_plot, reusability_plot2, scatter_plot_f, 
                           ncol = 2, nrow = 3, 
                           labels = c("A", "B", "C", "D", "E", "F"))

legend <- get_legend(
  emissions_plot2 + 
    theme(legend.position = "bottom", legend.text = element_text(size = 14))
)

combined_plot <- ggarrange(emissions_plot1, emissions_plot2, emissions_plot3, reusability_plot, reusability_plot2, scatter_plot_f, 
                           ncol = 2, nrow = 3, 
                           labels = c("A", "B", "C", "D", "E", "F"))

final_plot <- ggarrange(combined_plot, legend, ncol = 1, heights = c(10, 1))

path_combined = file.path(visualizations, 'd_rocket_emissions.png')
dir.create(visualizations, showWarnings = FALSE)
png(
  path_combined,
  units = "in",
  width = 16,
  height = 18,
  res = 480
)
print(final_plot)
dev.off()