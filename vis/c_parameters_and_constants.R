library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggtext)
library(scales)
library(rstudioapi)
library(viridis)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
visualizations = file.path(folder, 'figures')

filename = "satellites.csv"
satellites_data <- read.csv(file.path(folder, '../data/raw', filename))

shared_color_scale <- scale_fill_viridis_d(
  option = "plasma",
  direction = -1,
  name = NULL
)

plot_a <- ggplot(satellites_data, 
                 aes(x = factor(satellites_data$Constellation, 
                                levels = rev(unique(satellites_data$Constellation))), 
                     y = Satellites / 1000, fill=Lead.Region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f", Satellites / 1000)), vjust = .3, hjust=-0.5, size = 3) +
  theme_minimal() + coord_flip() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(
    title = "Number of Satellites by LEO Constellation",
    x = "",
    y = "Number of Satellites (in thousands)"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 400)) +
  shared_color_scale +
  theme(
    axis.title = element_text(size = 12),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.text.x = element_text(size = 10, angle = 0, hjust = 1),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

rocket_data <- read.csv(file.path(folder, '../data/raw', 'rocket_data.csv'))

payload_capacity_plot <- ggplot(rocket_data, 
                                aes(x = factor(Rocket, levels = rev(unique(Rocket))), y = Payload.Capacity, fill=Lead.Region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f", Payload.Capacity)), vjust = .3, hjust=-0.5, size = 3) +
  theme_minimal() + coord_flip() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(
    title = "Payload Capacity by Rocket",
    x = NULL,
    y = "Payload Capacity (kg)", 
    fill = 'Region'
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 150)) +
  shared_color_scale +
  theme(
    axis.title = element_text(size = 12),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.text.x = element_text(size = 10, angle = 0, hjust = 1),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

dry_mass_plot <- ggplot(rocket_data, 
                        aes(x = factor(Rocket, levels = rev(unique(Rocket))), y = Dry.Mass, fill=Lead.Region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::comma(Dry.Mass)), hjust = -0.3, size = 3) +
  theme_minimal() + coord_flip() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(
    title = "Dry Mass by Rocket",
    x = NULL,
    y = "Dry Mass (kg)", 
    fill = 'Region'
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 450000), labels = scales::comma) +
  shared_color_scale +
  theme(
    axis.title = element_text(size = 12),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

propellant_usage_plot <- ggplot(rocket_data, 
                                aes(x = factor(Rocket, levels = rev(unique(Rocket))), y = Propellant, fill=Lead.Region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::comma(Propellant)), hjust = -0.3, size = 3) +
  theme_minimal() + coord_flip() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(
    title = "Propellant Usage by Rocket",
    x = NULL,
    y = "Amount of Propellant", 
    fill = 'Region'
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5500000), labels = scales::comma) +
  shared_color_scale +
  theme(
    axis.title = element_text(size = 12),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
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
  levels = c("Solid", "Cryogenic", "Kerosene", "Hypergolic"))

data$Rocket <- factor(
  data$Rocket,
  levels = c('Ariane-5', 'Ariane-62', 'Ariane-64', 'Atlas V', 'Falcon-9', 'Falcon-Heavy', 'Long March 5', 'LVM3', 'New Glenn', 'Soyuz-FG', 'Starship', 'Vulcan Centaur'))

data <- data %>%
  group_by(Rocket) %>%
  mutate(total_emissions = sum(emission_value)) %>%
  mutate(proportion = emission_value / total_emissions)

emissions_plot <- ggplot(data, aes(x = Rocket, y = proportion, fill = Propellant_Type)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "plasma", name = NULL) +
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
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 13),
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
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

propellant_emissions_data <- read.csv(file.path(folder, '../data/raw', 'propellant_emissions_factors.csv'))

propellant_emissions_plot <- ggplot(propellant_emissions_data, aes(x = Propellant, y = Emissions.Factor)) +
  geom_bar(stat = "identity", fill = viridis(1)) +
  geom_text(aes(label = sprintf("%.2f", Emissions.Factor)), vjust = -0.3, size = 3) +
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
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

dry_mass_emissions_data <- read.csv(file.path(folder, '../data/raw', 'dry_mass_emissions_factors.csv'))

dry_mass_emissions_plot <- ggplot(dry_mass_emissions_data, aes(x = Material, y = Emissions.Factor)) +
  geom_bar(stat = "identity", fill = viridis(1, option = "plasma")) +
  geom_text(aes(label = sprintf("%.2f", Emissions.Factor)), vjust = -0.3, size = 3) +
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
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

transportation_emissions_data <- read.csv(file.path(folder, '../data/raw', 'transportation_emissions_factors.csv'))

transportation_emissions_plot <- ggplot(transportation_emissions_data, aes(x = Transportation, y = Emissions.Factor)) +
  geom_bar(stat = "identity", fill = viridis(1, option = "inferno")) +
  geom_text(aes(label = sprintf("%.2f", Emissions.Factor)), vjust = -0.3, size = 3) +
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

region_legend <- get_legend(
  plot_a + theme(legend.position = "bottom")
)

f_emissions <- ggarrange(
  dry_mass_plot, payload_capacity_plot, plot_a, propellant_usage_plot,
  ncol = 2, nrow = 2,
  labels = c("A", "B", "C", "D")
)

shared_color_scale <- scale_fill_viridis_d(
  option = "plasma",
  direction = -1,
  name = NULL,
  guide = guide_legend(nrow = 1)
)

dry_mass_plot <- dry_mass_plot + shared_color_scale
payload_capacity_plot <- payload_capacity_plot + shared_color_scale
propellant_usage_plot <- propellant_usage_plot + shared_color_scale

final_plot <- ggarrange(
  dry_mass_plot, payload_capacity_plot, propellant_usage_plot, plot_a,
  ncol = 2, 
  nrow = 2,
  labels = c("A", "B", "C", "D"),
  common.legend = TRUE,
  legend = "bottom"
)

path_combined <- file.path(visualizations, 'c_parameters_and_constants.png')
dir.create(visualizations, showWarnings = FALSE)
png(
  path_combined,
  units = "in",
  width = 12,
  height = 12,
  res = 300
)
print(final_plot)
dev.off()