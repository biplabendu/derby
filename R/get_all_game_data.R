get_all_game_data <- function(files) {
  
  safely_gather <- function(file) {
    tryCatch({
      tbl <- create_game_data(
        meta = get_game_metadata(file),
        score = get_game_score(file)
      ) |>
        mutate(
          filename = file,
          .before = 1
        )
      return(list(success = TRUE, tbl = tbl))
    }, error = function(e) {
      return(list(success = FALSE, tbl = NULL))
    })
  }
  
  progressr::handlers(global = TRUE)  # enable default progress bar
  
  progressr::with_progress({
    p <- progressr::progressor(steps = length(files))
    results <- purrr::map(
      files,
      function(f) {
        out <- safely_gather(f)
        p()
        out
      }
    )
  })
  
  results
}