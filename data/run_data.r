# Necessary libraries
library(data.table)
library(rvest)
library(xml2)

# Write a sys.time object to have a new file to commit every time.
data.table::fwrite(data.table::as.data.table(Sys.time()), 'data/_export/run_time.csv')

# Build Projections
source("data/projections/1_projections.R")
source("data/projections/2_projections_defense.R")
source("data/projections/3_projections_save.R")

# Pull Schedule
source("data/schedule/1_nfl_schedule.R")
