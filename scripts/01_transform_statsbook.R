rm(list = ls())

library(dplyr)
library(ggplot2)
# source(here::here("./functions/utils.R"))
for (i in list.files("R", full.names = TRUE)) source(i)

path <- here::here("dev/data")

all_files <- fs::dir_ls(
  path,
  recurse = 1
) |> 
  stringr::str_subset(
    "xlsx$"
  )

n = 100
l <- split(all_files, ceiling(seq_along(all_files)/n))

# Problematic formatting ---------------------------------------------------

# R crashes!
l[[24]] <- setdiff(
  l[[24]],
  c(
    "/Users/bidas/Documents/GitHub/derby/dev/data/2025-Q2/[WFTDA]STATS-2025-05-31_ConnecticutRollerDerby_YankeeBrutals_vs_TwinStateRollerDerby_Vixens.xlsx" 
    # something to do with the order of sheets or how the sheets are defined
  )
)
# R crashes!
l[[26]] <- setdiff(
  l[[26]],
  c(
    "/Users/bidas/Documents/GitHub/derby/dev/data/2025-Q3/[WFTDA]STATS-2025-07-26_ConnecticutRollerDerby_AllStars_vs_PennJerseyRollerDerby_Devils.xlsx" 
    # something to do with the order of sheets or how the sheets are defined
  )
)


# Run ---------------------------------------------------------------------

for (i in 1:length(l)) {
  filename <- glue::glue(
    "dev/data-transf/tmp/game-data-list-part-{i}.RDS"
  )
  
  if (!file.exists(filename)) {
    logger::log_info(
      glue::glue("Gathering {i}/{length(l)} ... \n")
    )
    out_list <- get_all_game_data(
      files = l[[i]]
    )
    # out <- purrr::map_dfr(out_list, ~ .x$tbl)
    saveRDS(out_list, filename)
  } else {
    cat("File exists; skipping read/write.\n")
  }
  
}

# Summary statistics ------------------------------------------------------

out <- purrr::map(
  1:length(l),
  function(part) {
    filename <- glue::glue(
      "dev/data-transf/tmp/game-data-list-part-{part}.RDS"
    )
    readRDS(
      filename
    ) 
  }
)

length(out[[1]][1])

successes <- purrr::map(
  out,
  function(x) {
    purrr::map_lgl(
      x,
      ~ .x[["success"]]
    )
  }
) |> 
  unlist()

cat(
  round(sum(successes) / length(all_files) * 100, 2),
  "% of", length(all_files), 
  "files successfully imported!"
)

tbl <- purrr::map(
  out,
  function(x) {
    purrr::map_dfr(
      x,
      ~ .x[["tbl"]]
    )
  }
) |> 
  bind_rows() |> 
  arrange(
    date
  )

saveRDS(
  tbl,
  "data/wftda/all_games_data-v1-15Oct25.RDS"
)


# # QC ----------------------------------------------------------------------
# 
# ff <- l[[26]]
# 
# for (i in 1:length(ff)) {
#   f <- ff[i]
#   print(i)
#   get_game_score(f)
#   # get_game_metadata(f)
# }
# 
# file <- ff[27]
# 
# readxl::read_excel(
#   file,
#   sheet = 4
#   # range = "A3:AK41",
#   # col_types = "text",
#   # .name_repair = "minimal"
# )
