plot_scoreboard_worm <- function(data) {
  
  last_jam <- data |> 
    filter(half == "first") |> 
    pull(jam) |> 
    max()
  
  data |> 
    select(
      jam,
      matches("gametotal")
    ) |> 
    tidyr::pivot_longer(
      cols = matches("gametotal"),
      names_to = "team",
      values_to = "value"
    ) |> 
    mutate(
      team = if_else(
        stringr::str_detect(team, "1$"),
        data$team1[1],
        data$team2[1]
      )
    ) |> 
    ggplot(
      aes(
        x = jam, y = value, 
        color = team, fill = team
      )
    ) +
    geom_vline(
      aes(xintercept = last_jam),
      size = 1.2,
      col = "black",
      lty = 1
    ) +
    geom_line(
      size = 1.5
    ) +
    geom_point(
      size = 2,
      shape = 21,
      col = "black"
    ) +
    labs(
      x = "Jam number",
      y = "Total points",
      title = data$date[1]
    ) +
    theme_minimal(24) +
    theme(
      legend.position = "top",
      legend.justification = "right",
      plot.title = element_text(hjust = 0.5),
      # legend.text = element_text(margin = margin(t = 5)),
      legend.title = element_blank()
    ) +
    guides(
      color = guide_legend(nrow = 2),
      fill = guide_legend(nrow = 2)
    )
}