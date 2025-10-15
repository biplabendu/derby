get_game_metadata <- function(file) {
  
  suppressMessages({
    # GAME INFO
    date_tbl = readxl::read_excel(
      file,
      sheet = "IGRF",
      range = "B7",
      col_types = "date",
      col_names = FALSE
    )
    
    time_tbl = readxl::read_excel(
      file,
      sheet = "IGRF",
      range = "I7",
      col_names = FALSE
    )
    
    # TEAM NAMES
    team1_tbl = readxl::read_excel(
      file,
      sheet = "IGRF",
      range = "B10:B11",
      col_types = "text",
      col_names = FALSE
    )
    
    team2_tbl = readxl::read_excel(
      file,
      sheet = "IGRF",
      range = "I10:I11",
      col_types = "text",
      col_names = FALSE
    )
    
    # TEAM ROSTERS
    num_players1_tbl = readxl::read_excel(
      file,
      sheet = "IGRF",
      range = "B14:B33",
      col_types = "text",
      col_names = FALSE
    )
    
    name_players1_tbl = readxl::read_excel(
      file,
      sheet = "IGRF",
      range = "C14:C33",
      col_types = "text",
      col_names = FALSE
    )
    
    num_players2_tbl = readxl::read_excel(
      file,
      sheet = "IGRF",
      range = "I14:I33",
      col_types = "text",
      col_names = FALSE
    )
    
    name_players2_tbl = readxl::read_excel(
      file,
      sheet = "IGRF",
      range = "J14:J33",
      col_types = "text",
      col_names = FALSE
    )
  })
  
  list(
    date = date_tbl |> 
      pull(1) |> 
      lubridate::as_date(),
    time = time_tbl |> 
      pull(1) |> 
      lubridate::as_datetime(),
    team1 = team1_tbl |> 
      pull(1) |> 
      paste(collapse = "; "),
    team2 = team2_tbl |> 
      pull(1) |> 
      paste(collapse = "; "),
    num_players1 = team2_tbl |> 
      pull(1) |> 
      na.omit() |> 
      as.character(),
    num_players2 = num_players2_tbl |> 
      pull(1) |> 
      na.omit() |> 
      as.character(),
    name_players1 = name_players1_tbl |> 
      pull(1)|> 
      na.omit() |> 
      as.character(),
    name_players2 = name_players2_tbl |> 
      pull(1)|> 
      na.omit() |> 
      as.character()
  )
}