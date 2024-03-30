# Custom functions ---
tidy_scores <- function(data, teams) {
  data |> 
    select(
      !matches("^trip_"),
      - jam_2
    ) |> 
    rename(
      jammer = jammers_number,
      jammer_2 = jammers_number_2,
      jamtotal = jam_total,
      jamtotal_2 = jam_total_2,
      gametotal = game_total,
      gametotal_2 = game_total_2
    ) |>
    tidyr::pivot_longer(
      cols = !jam,
      names_to = "what",
      values_to = "value"
    ) |> 
    tidyr::separate_wider_delim(
      what,
      delim = "_",
      names = c("what", "team"),
      too_few = "align_start"
    ) |> 
    # glimpse()
    mutate(
      team = if_else(
        is.na(team),
        teams[1],
        teams[2]
      )
    ) |> 
    arrange(
      jam,
      what,
      team
    ) |> 
    group_by(jam, what, team) |> 
    reframe(value = max(value, na.rm = TRUE)) |> 
    mutate(
      jam = as.integer(jam),
      team = as.factor(team)
    ) |> 
    filter(!is.na(jam))
}

combine_two_halves <- function(x, y) {
  rbind(
    x, y
  ) |> 
    arrange(
      half,
      jam,
      what,
      team
    ) |>
    mutate(
      last_jam = max(
        last_jam, 
        na.rm = TRUE
      )
    ) |> 
    rowwise() |> 
    mutate(
      jam = if_else(
        half == "first",
        jam,
        jam + last_jam
      )
    ) |> 
    ungroup()
}
