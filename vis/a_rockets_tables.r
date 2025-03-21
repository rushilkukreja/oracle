library(ggpubr)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(gridExtra)
library(grid)
library("readxl")

folder <- dirname(rstudioapi::getSourceEditorContext()$path)

rct = read.csv(file.path(folder, '..', 'data', 'raw', "rockets_table.csv"),
               row.names = 1)
new_names <- c('Ariane-5', 'Ariane-62', 'Ariane-64', 'Atlas V', 'Falcon-9', 'Falcon-Heavy')
colnames(rct) <- new_names

filename = 'a_rockets_table.png'
folder_tables = file.path(folder, 'figures', 'ab_tables')
path_out = file.path(folder_tables, filename)
dir.create(file.path(folder, 'figures'), showWarnings = FALSE)
dir.create(file.path(folder_tables), showWarnings = FALSE)

table_grob <- tableGrob(rct)
n_col <- ncol(rct)
for (i in seq_len(n_col)) {
  table_grob$widths[i] <- unit(0.13, "npc")
}
png(
  path_out,
  units = "in",
  width = 15,
  height = 3,
  res = 480
)
grid.newpage()
grid.draw(table_grob)
dev.off()

rct = read.csv(file.path(folder, '..', 'data', 'raw', "rockets_table_2.csv"),
               row.names = 1)
new_names <- c('Long March-5', 'LVM3', 'New Glenn', 'Soyuz-FG', 
               'Starship', 'Vulcan Centaur')
colnames(rct) <- new_names

filename = 'a_rockets_table_2.png'
folder_tables = file.path(folder, 'figures', 'ab_tables')
path = file.path(folder_tables, filename)
dir.create(file.path(folder, 'figures'), showWarnings = FALSE)
png(
  path,
  units = "in",
  width = 15,
  height = 3,
  res = 480
)
table_grob <- tableGrob(rct)
n_col <- ncol(rct)
for (i in seq_len(n_col)) {
  table_grob$widths[i] <- unit(0.13, "npc")
}
grid.newpage()
grid.draw(table_grob)
dev.off()
