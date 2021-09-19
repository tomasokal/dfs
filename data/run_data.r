# Necessary libraries
library(data.table)
library(rvest)
library(xml2)

# Build Projections
source("data/projections/1_projections.R")
source("data/projections/2_projections_defense.R")
source("data/projections/3_projections_save.R")

# Pull Schedule
source("data/schedule/1_nfl_schedule.R")
