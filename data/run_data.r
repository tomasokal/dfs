# Necessary libraries
library(data.table)
library(rvest)
library(xml2)

# Build Projections
source("projections/1_projections.R")
source("projections/2_projections_defense.R")

# Pull Schedule
source("schedule/1_nfl_schedule.R")
