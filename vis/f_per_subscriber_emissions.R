library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(ggtext)

neutral_colors <- c(
  "Launch Event" = "#4F81BD",
  "Launcher Production" = "#70AD47",
  "Electronics Production" = "#C0504D",
  "Launcher Transportation" = "#6A5ACD",
  "Electricity Consumption" = "#FF8C00",
  "Propulsion System" = "#9B870C"
)

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

emissions_plot <- ggplot(data2, aes(x = Constellation, y = emission_value)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_manual(values = neutral_colors) + 
  theme_minimal() +
  labs(
    title = "Greenhouse Gas Emissions per Subscriber by Satellite Constellation",
    colour = NULL,
    x = NULL,
    fill = NULL
  ) +
  ylab("GHG Emissions per Subscriber (kt CO2e)") + 
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0),
    limits = c(0, max_emission2 * 1.1)
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
    axis.line.x = element_line(size = 0.15),
    axis.line.y = element_line(size = 0.15),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) +
  geom_text(
    data = total_emissions2, 
    aes(x = Constellation, y = total_emission_value, label = comma(round(total_emission_value))),
    vjust = -0.5,
    size = 3
  )

filename3 <- "subscribers_emissions.csv"
data3 <- read.csv(file.path(folder, '../results', filename3))
data3$Subscribers <- as.numeric(gsub(",", "", data3$Subscribers))
data3$Emissions <- as.numeric(gsub(",", "", data3$Emissions))

max_emission3 <- max(data3$Emissions, na.rm = TRUE)
max_subscribers <- max(data3$Subscribers, na.rm = TRUE)

scatter_plot <- ggplot(data3, aes(x = Subscribers, y = Emissions)) +
  geom_point() +
  geom_text(aes(label = Constellation), vjust = -0.8, hjust = 0.5, size = 3) +
  theme_minimal() +
  labs(
    title = "Size vs Emissions per Subscriber by Satellite Constellation",
    x = "Size (number of satellites)",
    y = "GHG Emissions per Subscriber (kt CO2e)"
  ) +
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0),
    limits = c(0, max_emission3 * 1.1)
  ) +
  scale_x_continuous(
    labels = comma,
    expand = c(0, 0),
    limits = c(0, max_subscribers * 1.07)
  ) +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 30, r = 30, b = 20, l = 20)
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

max_emission4 <- max(total_emissions4$total_emission_value, na.rm = TRUE)

country_bar_chart <- ggplot(data4, aes(x = Country, y = emission_value)) +
  geom_bar(stat = "identity", aes(fill = impact_category)) +
  scale_fill_manual(values = neutral_colors) +
  theme_minimal() +
  labs(
    title = "Average Emissions per Subscriber by Country",
    x = NULL,
    y = "Average GHG Emissions per Subscriber (kt CO2e)",
    fill = NULL
  ) +
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0),
    limits = c(0, max_emission4 * 1.1)
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
    axis.line.x = element_line(size = 0.15),
    axis.line.y = element_line(size = 0.15),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) +
  geom_text(
    data = total_emissions4,
    aes(x = Country, y = total_emission_value, label = comma(round(total_emission_value))),
    vjust = -0.3,
    size = 3
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
  levels = c("Non-Reusable", "Reusable") # Explicitly set levels
)

h_reusability_emissions <- ggplot(data_reusability, aes(x = Reusable, y = emission_value, fill = impact_category)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) + # Adjust dodge width
  scale_fill_manual(values = neutral_colors) + 
  theme_minimal() +
  labs(
    title = "Emissions per Subscriber using Reusable and Non-Reusable Rockets",
    x = NULL,
    y = "GHG Emissions per Subscriber (kt CO2e)",
    fill = NULL
  ) +
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0),
    limits = c(0, max_emission_reusability * 1.1)
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
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 8),
    axis.line.x = element_line(size = 0.15),
    axis.line.y = element_line(size = 0.15),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) +
  geom_text(
    aes(label = comma(round(emission_value))),
    position = position_dodge(width = 0.8), # Saxme width as dodge
    vjust = -0.5,
    size = 3
  )

f_emissions_updated <- ggarrange(
  emissions_plot + 
    theme(
      axis.title = element_text(size = 10),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 8),
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    ),
  country_bar_chart + 
    theme(
      axis.title = element_text(size = 10),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 8),
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    ),
  h_reusability_emissions + 
    theme(
      axis.title = element_text(size = 10),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 8),
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    ),
  scatter_plot + 
    theme(
      axis.title = element_text(size = 10),
      axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 8),
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    ),
  ncol = 2,
  nrow = 2,
  labels = c("A", "B", "C", "D")
)

legend <- get_legend(
  emissions_plot + 
    theme(legend.position = "bottom", legend.text = element_text(size = 10))
)

final_plot <- ggarrange(
  f_emissions_updated,
  legend,
  ncol = 1,
  heights = c(10, 1)
)

output_path <- file.path(visualizations, 'f_per_subscriber_emissions.png')
png(
  output_path,
  units = "in",
  width = 16,
  height = 10.67,
  res = 300
)
print(final_plot)
dev.off()