library(dplyr)

idt <- jsonlite::fromJSON('https://api.draftkings.com/draftgroups/v1/draftgroups/71429/draftables')

idt_players <- idt$draftables
idt_games <- idt$competitions

jdt_players <- idt_players %>% 
  select(-draftableId) %>% 
  select(
    shortName, 
    playerId, playerDkId,
    position, teamAbbreviation, salary
  )

jdt_players_points <- idt_players$draftStatAttributes %>% 
  data.table::rbindlist() %>% 
  mutate(value = as.numeric(value)) %>% 
  select(value) %>% 
  as.data.frame()
jdt_players_points <- jdt_players_points[c(TRUE, FALSE), ]

jdt_players_games <- idt_players$competition %>% 
  select(name, startTime)

jdt <- cbind(jdt_players, jdt_players_points, jdt_players_games) %>% 
  distinct(playerId, .keep_all = TRUE)
