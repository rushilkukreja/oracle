library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggtext)
library(scales)
library(rstudioapi)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
visualizations = file.path(folder, 'figures')

filename = "satellites.csv"
satellites_data <- read.csv(file.path(folder, '../data/raw', filename))

neutral_colors <- c(
  "steelblue" = "#4F81BD",
  "forestgreen" = "#70AD47",
  "darkred" = "#C0504D",
  "slateblue" = "#6A5ACD",
  "darkgreen" = "#556B2F",
  "darkblue" = "#1F497D",
  "darkorange" = "#FF8C00"
)

satellites_plot <- ggplot(satellites_data, aes(x = Constellation, y = Satellites)) +
  geom_bar(stat = "identity", fill = neutral_colors["steelblue"]) +
  geom_text(aes(label = Satellites), vjust = -0.3, size = 3) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(
    title = "Number of Satellites by Constellation",
    y = "Number of Satellites"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),  # Increased font size here
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

rocket_data <- read.csv(file.path(folder, '../data/raw', 'rocket_data.csv'))

payload_capacity_plot <- ggplot(rocket_data, aes(x = Rocket, y = Payload.Capacity)) +
  geom_bar(stat = "identity", fill = neutral_colors["forestgreen"]) +
  geom_text(aes(label = Payload.Capacity), vjust = -0.3, size = 3) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(
    title = "Payload Capacity by Rocket",
    y = "Payload Capacity (kg)"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),  # Increased font size here
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

dry_mass_plot <- ggplot(rocket_data, aes(x = Rocket, y = Dry.Mass)) +
  geom_bar(stat = "identity", fill = neutral_colors["darkred"]) +
  geom_text(aes(label = Dry.Mass), vjust = -0.3, size = 3) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(
    title = "Dry Mass by Rocket",
    y = "Dry Mass (kg)"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),  # Increased font size here
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

propellant_usage_plot <- ggplot(rocket_data, aes(x = Rocket, y = Propellant)) +
  geom_bar(stat = "identity", fill = neutral_colors["darkorange"]) +
  geom_text(aes(label = Propellant), vjust = -0.3, size = 3) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(
    title = "Propellant Usage by Rocket",
    y = "Amount of Propellant"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),  # Increased font size here
    legend.position = "bottom",
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

data <- read.csv(file.path(folder, '../data/raw', 'rocket_data.csv'))

data <- data %>%
  select(
    Rocket, 
    Solid, Cryogenic, Kerosene, Hypergolic
  )

data <- data %>%
  pivot_longer(cols = c(Solid, Cryogenic, Kerosene, Hypergolic),
               names_to = "Propellant_Type", values_to = "emission_value")

data$Propellant_Type <- factor(
  data$Propellant_Type,
  levels = c("Solid", "Cryogenic", "Kerosene", "Hypergolic"),
  labels = c("Solid", "Cryogenic", "Kerosene", "Hypergolic"))

data$Rocket <- factor(
  data$Rocket,
  levels = c('Ariane-5', 'Ariane-62', 'Ariane-64', 'Arlas V', 'Falcon-9', 'Falcon-Heavy', 'Long March 5', 'LVM3', 'New Glenn', 'Soyuz-FG', 'Starship', 'Vulcan Centaur'))

data <- data %>%
  group_by(Rocket) %>%
  mutate(total_emissions = sum(emission_value)) %>%
  mutate(proportion = emission_value / total_emissions)

emissions_plot <- ggplot(data, aes(x = Rocket, y = proportion, fill = Propellant_Type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Solid" = "#4F81BD", "Cryogenic" = "#70AD47", 
                               "Kerosene" = "#C0504D", "Hypergolic" = "#6A5ACD"), name = NULL) + 
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(
    title = "Propellant Type by Rocket",
    x = NULL,
  ) +
  ylab("Percentage of Total Emissions") + 
  scale_y_continuous(labels = percent, expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 1)) +
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
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    plot.subtitle = element_text(size = 8, face = "bold"),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),  # Increased font size here
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

propellant_emissions_data <- read.csv(file.path(folder, '../data/raw', 'propellant_emissions_factors.csv'))

propellant_emissions_plot <- ggplot(propellant_emissions_data, aes(x = Propellant, y = Emissions.Factor)) +
  geom_bar(stat = "identity", fill = neutral_colors["slateblue"]) +
  geom_text(aes(label = Emissions.Factor), vjust = -0.3, size = 3) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(
    title = "Emissions Factors for Propellants",
    y = "Emissions Factor (kg CO2e)"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),  # Increased font size here
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

dry_mass_emissions_data <- read.csv(file.path(folder, '../data/raw', 'dry_mass_emissions_factors.csv'))

dry_mass_emissions_plot <- ggplot(dry_mass_emissions_data, aes(x = Material, y = Emissions.Factor)) +
  geom_bar(stat = "identity", fill = neutral_colors["darkgreen"]) +
  geom_text(aes(label = Emissions.Factor), vjust = -0.3, size = 3) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(
    title = "Emissions Factors for Dry Mass Materials",
    y = "Emissions Factor (kg CO2e)"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),  # Increased font size here
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

transportation_emissions_data <- read.csv(file.path(folder, '../data/raw', 'transportation_emissions_factors.csv'))

transportation_emissions_plot <- ggplot(transportation_emissions_data, aes(x = Transportation, y = Emissions.Factor)) +
  geom_bar(stat = "identity", fill = neutral_colors["darkblue"]) +
  geom_text(aes(label = Emissions.Factor), vjust = -0.3, size = 3) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(
    title = "Emissions Factors for Transportation",
    y = "Emissions Factor (kg CO2e)"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

f_emissions <- ggarrange(dry_mass_plot, payload_capacity_plot, satellites_plot,
                         propellant_emissions_plot, dry_mass_emissions_plot, transportation_emissions_plot, propellant_usage_plot, emissions_plot,
                         ncol = 2, 
                         nrow = 4,
                         labels = c("A", "B", "C", "D", "E", "F", "G", "H")
)

path_combined <- file.path(visualizations, 'c_parameters_and_constants.png')
dir.create(visualizations, showWarnings = FALSE)
png(
  path_combined,
  units = "in",
  width = 16,
  height = 16,
  res = 300
)
print(f_emissions)
dev.off()