create_game_data <- function(meta, score) {
  score |> 
    mutate(
      date = meta$date,
      time = meta$time,
      team1 = meta$team1,
      team2 = meta$team2,
      n_team1 = max(
        length(meta$num_players1),
        length(meta$name_players1)
      ),
      n_team2 = max(
        length(meta$num_players2),
        length(meta$name_players2)
      )
    ) |> 
    select(
      - last_jam
    )
}
