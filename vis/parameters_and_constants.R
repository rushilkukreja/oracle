library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggtext)
library(scales)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
visualizations = file.path(folder, 'figures')

filename = "satellites.csv"
satellites_data <- read.csv(file.path(folder, '../data/raw', filename))

satellites_plot <- ggplot(satellites_data, aes(x = Constellation, y = Satellites)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Satellites), vjust = -0.3, size = 3) +
  theme_minimal() +
  labs(
    title = "Number of Satellites by Constellation",
    x = "Constellation",
    y = "Number of Satellites"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

rocket_data <- read.csv(file.path(folder, '../data/raw', 'rocket_data.csv'))

payload_capacity_plot <- ggplot(rocket_data, aes(x = Rocket, y = Payload.Capacity)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  geom_text(aes(label = Payload.Capacity), vjust = -0.3, size = 3) +
  theme_minimal() +
  labs(
    title = "Payload Capacity by Rocket",
    x = "Rocket",
    y = "Payload Capacity (kg)"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

dry_mass_plot <- ggplot(rocket_data, aes(x = Rocket, y = Dry.Mass)) +
  geom_bar(stat = "identity", fill = "darkred") +
  geom_text(aes(label = Dry.Mass), vjust = -0.3, size = 3) +
  theme_minimal() +
  labs(
    title = "Dry Mass by Rocket",
    x = "Rocket",
    y = "Dry Mass (kg)"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

# Prepare data for Graph 4 (Side-by-side bar graph for propellants)
propellant_data <- rocket_data %>%
  pivot_longer(cols = c(Solid, Cryogenic, Kerosene, Hypergolic),
               names_to = "Propellant_Type", values_to = "Amount")

propellant_side_by_side_plot <- ggplot(propellant_data, aes(x = Rocket, y = Amount, fill = Propellant_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Amount), position = position_dodge(width = 0.9), vjust = -0.3, size = 3) +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  labs(
    title = "Propellant Usage by Rocket (Side-by-Side)",
    x = "Rocket",
    y = "Amount of Propellant"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

# Prepare data for Graph 5 (Segmented bar graph for propellants)
propellant_segmented_plot <- ggplot(propellant_data, aes(x = Rocket, y = Amount, fill = Propellant_Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Amount), position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  labs(
    title = "Propellant Usage by Rocket (Segmented)",
    x = "Rocket",
    y = "Total Amount of Propellant"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

f_emissions <- ggarrange(
  satellites_plot, payload_capacity_plot, dry_mass_plot, propellant_side_by_side_plot,
  propellant_segmented_plot, emissions_plot, emissions_plot, emissions_plot,
  ncol = 2, 
  nrow = 4,
  labels = c("A", "B", "C", "D", "E", "F", "G", "H")
)

path_combined = file.path(visualizations, 'parameters_and_constants.png')
dir.create(visualizations, showWarnings = FALSE)
png(
  path_combined,
  units = "in",
  width = 14,  # Adjusted size for better spacing
  height = 16,
  res = 300
)
print(f_emissions)
dev.off()

# Graph 6: Emissions factors for propellants
propellant_emissions_data <- read.csv(file.path(folder, '../data/raw', 'propellant_emissions_factors.csv'))

propellant_emissions_plot <- ggplot(propellant_emissions_data, aes(x = Propellant, y = Emissions.Factor)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = Emissions.Factor), vjust = -0.3, size = 3) +
  theme_minimal() +
  labs(
    title = "Emissions Factors for Propellants",
    x = "Propellant",
    y = "Emissions Factor (kg CO2e)"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

# Graph 7: Emissions factors for dry mass materials
dry_mass_emissions_data <- read.csv(file.path(folder, '../data/raw', 'dry_mass_emissions_factors.csv'))

dry_mass_emissions_plot <- ggplot(dry_mass_emissions_data, aes(x = Material, y = Emissions.Factor)) +
  geom_bar(stat = "identity", fill = "green") +
  geom_text(aes(label = Emissions.Factor), vjust = -0.3, size = 3) +
  theme_minimal() +
  labs(
    title = "Emissions Factors for Dry Mass Materials",
    x = "Material",
    y = "Emissions Factor (kg CO2e)"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

# Graph 8: Emissions factors for transportation
transportation_emissions_data <- read.csv(file.path(folder, '../data/raw', 'transportation_emissions_factors.csv'))

transportation_emissions_plot <- ggplot(transportation_emissions_data, aes(x = Transportation, y = Emissions.Factor)) +
  geom_bar(stat = "identity", fill = "purple") +
  geom_text(aes(label = Emissions.Factor), vjust = -0.3, size = 3) +
  theme_minimal() +
  labs(
    title = "Emissions Factors for Transportation",
    x = "Transportation",
    y = "Emissions Factor (kg CO2e)"
  ) +
  theme(
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

# Combine all graphs
f_emissions <- ggarrange(
  satellites_plot, payload_capacity_plot, dry_mass_plot, propellant_side_by_side_plot,
  propellant_segmented_plot, propellant_emissions_plot, dry_mass_emissions_plot, transportation_emissions_plot,
  ncol = 2, 
  nrow = 4,
  labels = c("A", "B", "C", "D", "E", "F", "G", "H")
)

path_combined = file.path(visualizations, 'parameters_and_constants.png')
dir.create(visualizations, showWarnings = FALSE)
png(
  path_combined,
  units = "in",
  width = 16,  # Increased width for better spacing
  height = 16,
  res = 300
)
print(f_emissions)
dev.off()