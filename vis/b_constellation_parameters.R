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
  equal_width <- unit(0.9 / n_col, "npc")
  for (i in seq_len(n_col)) {
    table_grob$widths[i] <- equal_width
  }
  return(table_grob)
}

rct1 = read.csv(file.path(folder, '..', 'data', 'raw', "constellation_parameters.csv"), row.names = 1)
new_names1 <- c('Astra', 'BlueWalker', 'Cinnamon-937', 'Flock', 'Globalstar', 'Guowang', 'Hanwha', 'Honghu-3', 'HVNET', 'KLEO', 'Kuiper', 'Lacuna', 'Lightspeed', 'Lynk', 'Omni', 'OneWeb', 'Rassvet', 'Semaphore-C', 'SferaCon', 'Starlink (Gen 2)', 'Swarm', 'Xingshidai', 'Xingwang', 'Yinhe')
colnames(rct1) <- new_names1
rct1_transposed <- t(rct1)

filename1 = 'b_constellation_parameters_table.png'
folder_tables1 = file.path(folder, 'figures', 'tables')
path1 = file.path(folder_tables1, filename1)
dir.create(file.path(folder, 'figures'), showWarnings = FALSE)
png(path1, units = "in", width = 10, height = 7.5, res = 480)
table_grob1 <- tableGrob(rct1_transposed)
table_grob1 <- set_equal_widths(table_grob1)
grid.newpage()
grid.draw(table_grob1)
dev.off()