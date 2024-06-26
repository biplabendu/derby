library(dplyr)
library(ggplot2)


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

# Load stats workbook ----------------------------------------------------------
files <- list.files(
  "./data/",
  pattern = "^STATS(.*?).xlsx$",
  full.names = TRUE
)

writeLines("Let's peek inside one of the files")

teams <- purrr::map(
  files,
  function(x) {
    c(
      strsplit(x, "_|.xlsx")[[1]][2],
      strsplit(x, "_|.xlsx")[[1]][4]
    )
  }
)

scores_firsthalf <- purrr::map(
  files,
  function(x) {
    readxl::read_excel(
      x,
      sheet = 3,
      range = "A3:AK41",
      col_types = "text",
      # skip = 2,
      .name_repair = "minimal"
    ) |> 
      janitor::clean_names()
  }
)

scores_secondhalf <- purrr::map(
  files,
  function(x) {
    readxl::read_excel(
      x,
      sheet = 3,
      range = "A45:AK83",
      col_types = "text",
      # skip = 2,
      .name_repair = "minimal"
    ) |> 
      janitor::clean_names() |> 
      rename_with(
        .cols = c(18,37),
        ~ c("game_total", "game_total_2")
      )
  }
)


# Tidy scores data ---------------------------------------------------------------

tidy_scores_firsthalf <- purrr::map2(
  scores_firsthalf,
  teams,
  function(x, y) {
    tidy_scores(
      data = x,
      teams = y
    ) |> 
      mutate(
        half = "first",
        last_jam = max(jam, na.rm = TRUE)
      )
  }
)

tidy_scores_secondhalf <- purrr::map2(
  scores_secondhalf,
  teams,
  function(x, y) {
    tidy_scores(
      data = x,
      teams = y
    ) |> 
      mutate(
        half = "second",
        last_jam = NA
      )
  }
)

# Scoreboard --------------------------------------------------------------

scoreboard_plots <- purrr::map2(
  tidy_scores_firsthalf,
  tidy_scores_secondhalf,
  function(x, y) {
    p <- combine_two_halves(
      x, y
    ) |> 
      filter(what == "gametotal") |> 
      mutate(
        value = as.integer(value)
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
        y = "Total points"
        # subtitle = "vertical line denotes half-time"
      ) +
      theme_minimal(25) +
      theme(
        legend.position = "bottom",
        legend.justification = "right",
        # legend.spacing.y = unit(1.0, 'cm'),
        legend.text = element_text(margin = margin(t = 5)),
        legend.title = element_blank()
      ) +
      guides(
        color = guide_legend(nrow = 2),
        fill = guide_legend(nrow = 2)
      )
    
    p
  }
)

# Stats for each jammer ---------------------------------------------------

jammer_plots <- purrr::pmap(
  list(
    tidy_scores_firsthalf,
    tidy_scores_secondhalf,
    teams
  ),
  function(x, y, z) {
    # print(x)
    # print(z)
    p <- combine_two_halves(
      x, y
    ) |>
      filter(
        what %in% c(
          "jammer",
          "jamtotal",
          "lead",
          "lost"
        )
      ) |>
      tidyr::pivot_wider(
        names_from = what,
        values_from = value
      ) |>
      select(
        - last_jam
      ) |>
      relocate(
        half,
        .before = 1
      ) |>
      mutate(
        across(
          c(lead, lost),
          ~ if_else(
            is.na(.),
            0,
            1
          )
        )
      ) |>
      group_by(
        half,
        team,
        jammer
      ) |>
      reframe(
        n_jams = length(unique(jam)),
        n_points = sum(as.integer(jamtotal)),
        n_points_per_jam = round(n_points/n_jams, 1),
        n_lead = sum(lead)
        # n_lost = sum(lost)
      ) |>
      tidyr::pivot_longer(
        cols = starts_with("n_"),
        names_to = "what",
        values_to = "value"
      ) |>
      arrange(
        team,
        jammer,
        half
      ) |>
      ## Filter measures and teams
      filter(
        !what %in% c(
          "n_points"
        )
      ) |>
      # filter(
      #   team == "HighAltitudeRollerDerby"
      # ) |>
      mutate(
        half = factor(half, levels = c("second", "first")),
        team = if_else(
          team == "HighAltitudeRollerDerby",
          "HARD",
          "Opponent"
        )
      ) |>
      ggplot(
        aes(
          x = as.factor(jammer),
          y = value,
          col = half,
          fill = half
        )
      ) +
      geom_bar(
        stat = "identity",
        # position = "stacked",
        width = .7,
        size = 0.5,
        color = "black"
      ) +
      labs(
        x = "Jammer",
        y = "Value",
        subtitle = glue::glue(
          "{z[1]} vs.\n{z[2]}"
        )
      ) +
      facet_grid(
        team ~ what,
        scales = "free"
      ) +
      scale_y_continuous(n.breaks = 3) +
      theme_minimal(15) +
      theme(
        legend.position = "top",
        legend.justification = "right",
        # legend.spacing.y = unit(1.0, 'cm'),
        legend.text = element_text(margin = margin(t = 5)),
        legend.title = element_blank()
      ) +
      guides(
        color = guide_legend(nrow = 1),
        fill = guide_legend(nrow = 1)
      ) +
      scale_fill_manual(
        values = c("orange", "yellow")
      ) +
      coord_flip()

    p
  }
)
