## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(octgridtools)
library(heyexr)
library(tidyverse)
library(scales)

## ----createExamples------------------------------------------------------
# Sectoral grid
grid_file <- system.file(
  "extdata", "Grid_Circle_Macula_ETDRS.txt",
  package = "octgridtools"
)

grid_sectors <-
  generate_grid_sectors(
    read_tsv(grid_file),
    center_x = 0,
    center_y = 0
  ) %>%
  mutate(type = "sectors")

