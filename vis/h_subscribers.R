library(ggpubr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
visualizations = file.path(folder, 'figures')
filename = "subscribers.csv"
data <- read.csv(file.path(folder, '../data/raw', filename))

data = select(data, Constellation, Subscribers)

data$Subscribers <- as.numeric(gsub(",", "", data$Subscribers))

data$Constellation = factor(
  data$Constellation,
  levels = c('Astra', 'BlueWalker', 'Cinnamon-937', 'Flock', 'Globalstar', 'Guowang', 'Hanwha', 'Honghu-3', 'HVNET', 'KLEO', 'Kuiper', 'Lacuna', 'Lightspeed', 'Lynk', 'Omni', 'OneWeb', 'Rassvet', 'Semaphore-C', 'SferaCon', 'Starlink (Gen2)', 'Swarm', 'Xingshidai', 'Xingwang', 'Yinhe'),
  labels = c('Astra', 'BlueWalker', 'Cinnamon-937', 'Flock', 'Globalstar', 'Guowang', 'Hanwha', 'Honghu-3', 'HVNET', 'KLEO', 'Kuiper', 'Lacuna', 'Lightspeed', 'Lynk', 'Omni', 'OneWeb', 'Rassvet', 'Semaphore-C', 'SferaCon', 'Starlink (Gen2)', 'Swarm', 'Xingshidai', 'Xingwang', 'Yinhe'))

emissions_plot <-
  ggplot(data, aes(x = Constellation, y = Subscribers)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(
    x = NULL,
    y = "Number of Subscribers"
  ) +
  scale_y_continuous(
    labels = comma,
    expand = c(0, 0)
  ) +
  theme(
    axis.title = element_text(size = 6),
    axis.line = element_line(colour = "black"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 6),
    axis.line.x  = element_line(size = 0.15),
    axis.line.y  = element_line(size = 0.15),
    plot.subtitle = element_text(size = 8, face = "bold"),
    plot.title = element_text(size = 8, face = "bold"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )

path = file.path(visualizations, 'h_subscribers.png')
dir.create(visualizations, showWarnings = FALSE)
png(
  path,
  units = "in",
  width = 12,
  height = 4,
  res = 480
)
print(emissions_plot)
dev.off()
