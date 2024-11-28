library(ggpubr)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(gridExtra)
library(grid)
library("readxl")

folder <- dirname(rstudioapi::getSourceEditorContext()$path)

rct <- read.csv(file.path(folder, '..', 'data', 'raw', "rocket_parameters.csv"),
                row.names = 1)
new_names <- c('Ariane-5', 'Ariane-62', 'Ariane-64', 'Atlas V', 'Falcon-9', 'Falcon-Heavy', 'Long March-5', 'LVM3', 'New Glenn', 'Soyuz-FG', 'Starship', 'Vulcan Centaur')
colnames(rct) <- new_names

rct_transposed <- t(rct)

rct_first_half <- rct_transposed[, 1:4]
rct_second_half <- rct_transposed[, 5:ncol(rct_transposed)]

folder_tables <- file.path(folder, 'figures', 'tables')
dir.create(folder_tables, showWarnings = FALSE, recursive = TRUE)

filename_first = 'b_rocket_parameters_table_1.png'
path_first = file.path(folder_tables, filename_first)
png(
  path_first,
  units = "in",
  width = 20,
  height = 8,
  res = 480
)
table_grob_first <- tableGrob(rct_first_half)
for (i in seq_len(ncol(rct_first_half))) {
  max_width <- max(unit.c(table_grob_first$widths[i], stringWidth(colnames(rct_first_half)[i])))
  table_grob_first$widths[i] <- max_width
}
grid.newpage()
grid.draw(table_grob_first)
dev.off()

filename_second = 'b_rocket_parameters_table_2.png'
path_second = file.path(folder_tables, filename_second)
png(
  path_second,
  units = "in",
  width = 25,
  height = 8,
  res = 480
)
table_grob_second <- tableGrob(rct_second_half)
for (i in seq_len(ncol(rct_second_half))) {
  max_width <- max(unit.c(table_grob_second$widths[i], stringWidth(colnames(rct_second_half)[i])))
  table_grob_second$widths[i] <- max_width
}
grid.newpage()
grid.draw(table_grob_second)
dev.off()