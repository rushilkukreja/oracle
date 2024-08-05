library(ggpubr)
library(ggplot2)
library(tidyverse)
library(ggtext)
library(gridExtra)
library(grid)
library("readxl")

folder <- dirname(rstudioapi::getSourceEditorContext()$path)

set_equal_widths <- function(table_grob) {
  n_col <- ncol(table_grob)
  equal_width <- unit(0.6 / n_col, "npc")
  for (i in seq_len(n_col)) {
    table_grob$widths[i] <- equal_width
  }
  return(table_grob)
}

rct1 = read.csv(file.path(folder, '..', 'data', 'raw', "constellation_parameters.csv"), row.names = 1)
new_names1 <- c('Astra', 'BlueWalker', 'Cinnamon-937', 'Flock', 'Globalstar', 'Guowang', 'Hanwha', 'Honghu-3', 'HVNET', 'KLEO', 'Kuiper', 'Lacuna')
colnames(rct1) <- new_names1

filename1 = 'constellation_parameters_table.png'
folder_tables1 = file.path(folder, 'figures', 'tables')
path1 = file.path(folder_tables1, filename1)
dir.create(file.path(folder, 'figures'), showWarnings = FALSE)
png(path1, units = "in", width = 41, height = 2.5, res = 480)
table_grob1 <- tableGrob(rct1)
table_grob1 <- set_equal_widths(table_grob1)
grid.newpage()
grid.draw(table_grob1)
dev.off()

rct2 = read.csv(file.path(folder, '..', 'data', 'raw', "constellation_parameters_2.csv"), row.names = 1)
new_names2 <- c('Lightspeed', 'Lynk', 'Omni', 'OneWeb', 'Rassvet', 'Semaphore-C', 'SferaCon', 'Starlink (Gen 2)', 'Swarm', 'Xingshidai', 'Xingwang', 'Yinhe')
colnames(rct2) <- new_names2

filename2 = 'constellation_parameters_table_2.png'
folder_tables2 = file.path(folder, 'figures', 'tables')
path2 = file.path(folder_tables2, filename2)
dir.create(file.path(folder, 'figures'), showWarnings = FALSE)
png(path2, units = "in", width = 41, height = 2.5, res = 480)
table_grob2 <- tableGrob(rct2)
table_grob2 <- set_equal_widths(table_grob2)
grid.newpage()
grid.draw(table_grob2)
dev.off()
