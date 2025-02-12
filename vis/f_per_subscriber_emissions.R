library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(ggtext)
library(viridis)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
visualizations <- file.path(folder, 'figures')
dir.create(visualizations, showWarnings = FALSE)

filename2 <- "per_subscriber_emissions.csv"
data2 <- read.csv(file.path(folder, '../results', filename2))
data2 <- select(
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
data2$impact_category <- factor(
  data2$impact_category,
  levels = c(
    "Launch.Event", "Launcher.Production", "Electronics.Production",
    "Launcher.Transportation", "Electricity.Consumption", "Propulsion.System"),
  labels = c(
    "Launch Event", "Launcher Production", "Electronics Production",
    "Launcher Transportation", "Electricity Consumption", "Propulsion System"))

total_emissions2 <- data2 %>%
  group_by(Constellation) %>%
  summarise(total_emission_value = sum(emission_value, na.rm = TRUE))

max_emission2 <- max(total_emissions2$total_emission_value, na.rm = TRUE)

total_emissions2 <- total_emissions2 %>%
  mutate(
    lower = total_emission_value * 0.9,
    upper = total_emission_value * 1.1
  )

emissions_plot <- ggplot(data2, aes(x = Constellation, y = emission_value)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  coord_flip() +  # Flip coordinates
  scale_fill_viridis_d(option = "plasma") + 
  theme_minimal() +
  labs(
    title = "Greenhouse Gas Emissions per Subscriber by Satellite Constellation",
    colour = NULL,
    y = "GHG Emissions per Subscriber (kt CO2e)",
    x = NULL,
    fill = NULL
  ) +
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0),
    limits = c(0, max_emission2 * 1.2)
  ) + 
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.line = element_line(colour = "black"),
    strip.text.x = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_markdown(margin = margin(r = 10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 11),  # Increased y-axis text (former x-axis labels)
    axis.text.x = element_text(size = 10),  # Increased x-axis text
    axis.line.x = element_line(size = 0.15),
    axis.line.y = element_line(size = 0.15),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) +
  geom_text(
    data = total_emissions2, 
    aes(x = Constellation, y = upper + (upper - lower) * 0.05, label = comma(round(total_emission_value))),  
    hjust = -0.1,  # Adjusted for flipped coordinates
    size = 3.5
  ) +
  geom_errorbar(
    data = total_emissions2,
    aes(x = Constellation, ymin = lower, ymax = upper, y = total_emission_value),
    width = 0.2,
    size = 0.5,
    color = "red"
  )

filename4 <- "subscriber_emissions_by_country.csv"
data4 <- read.csv(file.path(folder, '../results', filename4))
data4 <- pivot_longer(
  data4,
  cols = c(Launch.Event, Launcher.Production, Electronics.Production, Launcher.Transportation, Electricity.Consumption, Propulsion.System),
  names_to = "impact_category",
  values_to = "emission_value"
)
data4$emission_value <- as.numeric(gsub(",", "", data4$emission_value))
data4$impact_category <- factor(
  data4$impact_category,
  levels = c(
    "Launch.Event", "Launcher.Production", "Electronics.Production",
    "Launcher.Transportation", "Electricity.Consumption", "Propulsion.System"),
  labels = c(
    "Launch Event", "Launcher Production", "Electronics Production",
    "Launcher Transportation", "Electricity Consumption", "Propulsion System"))

total_emissions4 <- data4 %>%
  group_by(Country) %>%
  summarise(total_emission_value = sum(emission_value, na.rm = TRUE))

total_emissions4 <- total_emissions4 %>%
  mutate(
    lower = total_emission_value * 0.9,  # -10%
    upper = total_emission_value * 1.1   # +10%
  )

max_emission4 <- max(total_emissions4$total_emission_value, na.rm = TRUE)

country_bar_chart <- ggplot(data4, aes(x = Country, y = emission_value)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  coord_flip() +  # Flip coordinates
  scale_fill_viridis_d(option = "plasma") +
  theme_minimal() +
  labs(
    title = "Average Emissions per Subscriber by Country",
    y = "Average GHG Emissions per Subscriber (kt CO2e)",
    x = NULL,
    fill = NULL
  ) +
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0),
    limits = c(0, max_emission4 * 1.2)
  ) +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.line = element_line(colour = "black"),
    strip.text.x = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_markdown(margin = margin(r = 10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    axis.line.x = element_line(size = 0.15),
    axis.line.y = element_line(size = 0.15),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.margin = margin(t = 20, r = 40, b = 20, l = 20)
  ) +
  geom_text(
    data = total_emissions4,
    aes(x = Country, y = upper + (upper - lower) * 0.05, label = comma(round(total_emission_value))),  
    hjust = -0.2,
    size = 3.5
  ) +
  geom_errorbar(
    data = total_emissions4,
    aes(x = Country, ymin = lower, ymax = upper, y = total_emission_value),
    width = 0.2,
    size = 0.5,
    color = "red"
  )

filename_reusability <- "reusability_per_subscriber.csv"
data_reusability <- read.csv(file.path(folder, '../results', filename_reusability))

data_reusability <- data_reusability %>%
  pivot_longer(
    cols = -Reusable,
    names_to = "impact_category",
    values_to = "emission_value"
  )

data_reusability$impact_category <- factor(
  data_reusability$impact_category,
  levels = c(
    "Launch.Event", "Launcher.Production", "Electronics.Production",
    "Launcher.Transportation", "Electricity.Consumption", "Propulsion.System"),
  labels = c(
    "Launch Event", "Launcher Production", "Electronics Production",
    "Launcher Transportation", "Electricity Consumption", "Propulsion System")
)

total_emissions_reusability <- data_reusability %>%
  group_by(Reusable) %>%
  summarise(total_emission_value = sum(emission_value, na.rm = TRUE))

max_emission_reusability <- max(total_emissions_reusability$total_emission_value, na.rm = TRUE)

data_reusability$Reusable <- factor(
  data_reusability$Reusable,
  levels = c("Non-Reusable", "Reusable")
)

f_emissions_updated <- ggarrange(
  emissions_plot + 
    geom_text(
      data = total_emissions2, 
      aes(x = Constellation, y = total_emission_value, label = comma(round(total_emission_value))),
      vjust = -0.5,
      size = 3
    ) +
    geom_errorbar(
      data = total_emissions2,
      aes(x = Constellation, ymin = lower, ymax = upper, y = total_emission_value),
      width = 0.2,
      size = 0.5,
      color = "red"
    ) +
    theme(
      axis.title = element_text(size = 10),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 8),
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      plot.margin = margin(t = 20, r = 40, b = 20, l = 20)
    ),
  country_bar_chart + 
    geom_text(
      data = total_emissions4,
      aes(x = Country, y = total_emission_value, label = comma(round(total_emission_value))),
      vjust = -0.3,
      size = 3
    ) +
    geom_errorbar(
      data = total_emissions4,
      aes(x = Country, ymin = lower, ymax = upper, y = total_emission_value),
      width = 0.2,
      size = 0.5,
      color = "red"
    ) +
    theme(
      axis.title = element_text(size = 10),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 8),
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    ),
  ncol = 2,
  nrow = 1,
  labels = c("A", "B")
)

shared_legend <- get_legend(
  emissions_plot +
    guides(fill = guide_legend(nrow = 1), byrow = TRUE) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.text = element_text(size = 10),  # Increased legend text size
      legend.title = element_text(size = 11),  # Increased legend title size
      legend.spacing.x = unit(0.2, 'cm'),
      legend.margin = margin(t = 5, r = 0, b = 0, l = 0)
    )
)

emissions_plot <- emissions_plot + theme(legend.position = "none")
country_bar_chart <- country_bar_chart + theme(legend.position = "none")

final_plot <- ggarrange(
  emissions_plot,
  country_bar_chart,
  ncol = 2,
  nrow = 1,
  labels = c("A", "B"),
  common.legend = TRUE,
  legend = "bottom",
  legend.grob = shared_legend
)

output_path <- file.path(visualizations, 'f_per_subscriber_emissions.png')
png(
  output_path,
  units = "in",
  width = 16,
  height = 8,
  res = 300
)
print(final_plot)
dev.off(