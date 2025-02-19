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
new_names <- c('Ariane-5', 'Ariane-62', 'Ariane-64', 'Atlas V', 'Falcon-9', 
               'Falcon-Heavy', 'Long March-5', 'LVM3', 'New Glenn', 'Soyuz-FG', 
               'Starship', 'Vulcan Centaur')
colnames(rct) <- new_names

rct_transposed <- t(rct)

rct_first_half <- rct_transposed[, 1:4]
rct_second_half <- rct_transposed[, 5:ncol(rct_transposed)]

folder_tables <- file.path(folder, 'figures', 'ab_tables')
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

headers <- rownames(rct)[1:4]
headers_with_breaks <- c(
  "Solid propellant\nmass (kg)",
  "Cyrogenic propellant\nmass (kg)", 
  "Kerosene propellant\nmass (kg)",
  "Hypergolic propellant\nmass (kg)"
)

rct_first_half_renamed <- rct_first_half
colnames(rct_first_half_renamed) <- headers_with_breaks

table_grob_first <- tableGrob(rct_first_half_renamed)

for (i in seq_len(ncol(rct_first_half))) {
  max_width <- max(
    unit.c(table_grob_first$widths[i], 
           stringWidth(headers_with_breaks[i])))
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

rct_second_half_modified <- rct_second_half

split_text <- function(text, delimiters = c(", "), keep_delimiter = TRUE) {
  result <- text
  for(delimiter in delimiters) {
    parts <- strsplit(result, delimiter)[[1]]
    if(length(parts) > 1) {
      first_part <- parts[1]
      rest_parts <- parts[-1]
      if(keep_delimiter) {
        result <- paste0(first_part, delimiter, "\n", paste(rest_parts, collapse=delimiter))
      } else {
        result <- paste0(first_part, "\n", paste(rest_parts, collapse=delimiter))
      }
    }
  }
  return(result)
}

rct_second_half_modified <- rct_second_half
rct_second_half_modified[, "Launcher material"] <- sapply(rct_second_half[, "Launcher material"], function(x) split_text(x, c("inum"), TRUE))
rct_second_half_modified[, "Production location"] <- sapply(rct_second_half[, "Production location"], function(x) split_text(x, c(", ", "&"), TRUE))
rct_second_half_modified[, "Launch location"] <- sapply(rct_second_half[, "Launch location"], split_text)
rct_second_half_modified[, "Reusability"] <- sapply(rct_second_half[, "Reusability"], function(x) split_text(x, c("e 1", "Yes, up to"), TRUE))
rct_second_half_modified[, "Transportation vehicle"] <- sapply(rct_second_half[, "Transportation vehicle"], function(x) split_text(x, c("friendly", ","), TRUE))

headers_second <- c(
  "Dry mass\n(kg)",
  "Launcher\nmaterial",
  "Reusability", 
  "Production\nlocation",
  "Launch\nlocation",
  "Transportation\nvehicle",
  "Payload\nmass (tonnes)"
)

colnames(rct_second_half_modified) <- headers_second
table_grob_second <- tableGrob(rct_second_half_modified)

grid.newpage()
grid.draw(table_grob_second)
dev.off()