get_game_score <- function(file) {
  scores_first <- readxl::read_excel(
    file,
    sheet = "Score",
    range = "A3:AK41",
    col_types = "text",
    .name_repair = "minimal"
  ) |> 
    janitor::clean_names()
  
  
  scores_second <- readxl::read_excel(
    file,
    sheet = "Score",
    range = "A45:AK83",
    col_types = "text",
    .name_repair = "minimal"
  ) |> 
    janitor::clean_names() |> 
    rename_with(
      .cols = c(18,37),
      ~ c("game_total", "game_total_2")
    )
  
  out <- bind_rows(
    scores_first |> 
      transmute(
        jam = as.integer(jam),
        jammer1 = jammers_number,
        jammer2 = jammers_number_2,
        jamtotal1 = jam_total,
        jamtotal2 = jam_total_2,
        gametotal1 = game_total,
        gametotal2 = game_total_2,
        lead = case_when(
          tolower(lead) == "x" ~ "team1",
          tolower(lead_2) == "x" ~ "team2",
          .default = NA
        )
      ) |> 
      filter(
        !is.na(jam)
      ) |> 
      mutate(
        half = "first",
        last_jam = max(jam, na.rm = TRUE)
      ),
    scores_second |> 
      transmute(
        jam = as.integer(jam),
        jammer1 = jammers_number,
        jammer2 = jammers_number_2,
        jamtotal1 = jam_total,
        jamtotal2 = jam_total_2,
        gametotal1 = game_total,
        gametotal2 = game_total_2,
        lead = case_when(
          tolower(lead) == "x" ~ "team1",
          tolower(lead_2) == "x" ~ "team2",
          .default = NA
        )
      ) |> 
      filter(
        !is.na(jam)
      ) |> 
      mutate(
        half = "second"
      )
  ) |> 
    mutate(
      last_jam = unique(na.omit(last_jam)),
      jam = if_else(
        half == "second",
        jam + last_jam,
        jam
      ),
      across(
        matches("total"),
        ~ as.integer(.x)
      )
    )
}