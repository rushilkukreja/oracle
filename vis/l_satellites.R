library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggtext)
library(scales)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
visualizations = file.path(folder, 'figures')
filename = "satellites.csv"
data <- read.csv(file.path(folder, '../data/raw', filename))

# Since we are focusing only on the number of satellites, we select only the relevant columns
data = select(data, Constellation, Satellites)

# Convert the Satellites column to numeric if needed
data$Satellites <- as.numeric(gsub(",", "", data$Satellites))

# Set up the Constellation factor with the updated constellation names
data$Constellation = factor(
  data$Constellation,
  levels = c(
    'Astra', 'BlueWalker', 'Cinnamon-937', 'Flock', 'Globalstar', 'Guowang', 'Hanwha', 
    'Honghu-3', 'HVNET', 'KLEO', 'Kuiper', 'Lacuna', 'Lightspeed', 'Lynk', 'Omni', 
    'OneWeb', 'Rassvet', 'Semaphore-C', 'SferaCon', 'Starlink (Gen2)', 'Swarm', 
    'Xingshidai', 'Xingwang', 'Yinhe'
  )
)

# Plotting the number of satellites with a single color
satellite_plot <-
  ggplot(data, aes(x = Constellation, y = Satellites)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # single color
  theme_minimal() +
  labs(
    colour = NULL,
    x = NULL
  ) +
  ylab("Number of Satellites") + 
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0)
  ) +
  theme(
    axis.title = element_text(size = 6),
    axis.line = element_line(colour = "black"),
    panel.border = element_blank(),
    axis.title.y = element_markdown(margin = margin(r = 10)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    axis.text.y = element_text(size = 6),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    plot.subtitle = element_text(size = 8, face = "bold"),
    plot.title = element_text(size = 8, face = "bold"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

# Saving the plot as a PNG
path = file.path(visualizations, 'satellite_count.png')
dir.create(visualizations, showWarnings = FALSE)
png(
  path,
  units = "in",
  width = 8.5,
  height = 5,
  res = 480
)
print(satellite_plot)
dev.off()