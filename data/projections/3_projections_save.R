# Load player and defense projections
# Loading this to join in new script for debugging purposes
projections_players <- data.table::fread('_export/projections_players.csv')
projections_dst <- data.table::fread('_export/projections_dst.csv')

# Merge projections
projections <- data.table::rbindlist(list(projections_players, projections_dst), fill = TRUE)

# Write to csv file
data.table::fwrite(projections, '_export/projections_full.csv')