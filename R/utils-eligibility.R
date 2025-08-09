compute_eligibility_status <- function(meta, practice, game) {
  
  m <- meta |> filter(game == !!game)
  
  dat_practice <- practice |>
  # practice |>
    filter(
      p_date >= m$start &
        p_date <= m$end
    ) |>
    filter(
      error == FALSE
    ) |>
    select(
      - error
    ) |>
    tidyr::unite(
      "player",
      c(off_skates, in_attendance),
      sep = ", ",
      remove = FALSE
    ) |>
    tidyr::separate_longer_delim(
      player,
      delim = ", "
    ) |>
    filter(
      player != "NA"
    ) |>
    mutate(
      player = tolower(trimws(player)),
      player = case_when(
        stringr::str_detect(player, "mini") ~ "mini fridge",
        stringr::str_detect(player, "toxic") ~ "gin n. toxic",
        stringr::str_detect(player, "beaver") ~ "eager beaver",
        .default = player
      )
    ) |>
    distinct()
  
  # foo <- dat_practice |>
  #   group_by(
  #     p_date,
  #     p_type,
  #     player
  #   ) |>
  #   mutate(
  #     n = n()
  #   ) |>
  #   ungroup() |>
  #   arrange(desc(n), player) |>
  #   # filter(n > 1) |> View()
  #   rowwise() |>
  #   mutate(
  #     off_skates = if_else(
  #       tolower(off_skates) == "na",
  #       NA,
  #       off_skates
  #     ),
  #     duplicate = if_else(
  #       stringr::str_detect(
  #         tolower(in_attendance),
  #         tolower(player)
  #       ) &
  #         # !is.na(off_skates) &
  #         n >= 2,
  #       TRUE,
  #       FALSE
  #     )
  #   ) |>
  #   filter(
  #     duplicate
  #   )
  
  foo <- dat_practice |>
  # dat_practice |>
    select(
      - in_attendance,
      - off_skates
    ) |>
    rowwise() |>
    mutate(
      across(
        matches("^duration"),
        ~ case_when(
          is.na(.) ~ 0,
          .default = readr::parse_number(.)
        ),
        .names = "new_{.col}"
      ),
      new_duration_osw = if_else(
        is.na(new_duration_osw) &
          stringr::str_detect(
            duration_osw,
            "hr|hour"
          ),
        1,
        new_duration_osw
      ),
      new_duration_pop = if_else(
        is.na(new_duration_pop) &
          stringr::str_detect(
            duration_pop,
            "hr|hour"
          ),
        1,
        new_duration_pop
      ),
    ) |>
    select(
      player,
      timestamp,
      p_type2,
      p_type,
      dur_pop = new_duration_pop,
      dur_osw = new_duration_osw
    ) |>
    mutate(
      credit_pop = 1*dur_pop,
      credit_osw = 0.5*dur_osw,
      credit_bw  = if_else(
        p_type2 == "bw",
        1,
        0
      ),
      credit_other = case_when(
        p_type2 == "lge" ~ 1,
        p_type2 == "star" ~ 1,
        p_type2 == "lm" ~ 1,
        p_type2 == "di" ~ 1,
        p_type2 == "fmc" ~ 1,
        .default = 0
      )
    )
  
  
  foo |>
    group_by(
      player
    ) |>
    reframe(
      credit_pop = sum(credit_pop, na.rm = TRUE),
      credit_osw = sum(credit_osw, na.rm = TRUE),
      credit_bw = sum(credit_bw, na.rm = TRUE),
      credit_other = sum(credit_other, na.rm = TRUE)
    ) |>
    # set limits on `bw` and `osw`
    mutate(
      credit_osw = if_else(credit_osw > 2, 2, credit_osw),
      credit_bw = if_else(credit_bw > 1, 1, credit_bw)
    ) |>
    mutate(
      credit_current = rowSums(
        across(
          matches("credit")
        )
      ),
      credit_reqd = m$practice_credit_req
    ) |>
    arrange(
      desc(credit_current)
    ) |> 
    mutate(
      game = game,
      .before = 1
    )
}

pretty_eligibility_tbl <- function(data) {
  data |> 
    mutate(
      status = if_else(
        credit_current >= credit_reqd,
        "Complete",
        paste(
          round(abs(credit_current - credit_reqd), 1),
          " reqd"
        )
      )
    ) |>
    select(
      game,
      player,
      status,
      `Credit Total` = credit_current,
      `Bout Watch` = credit_bw,
      `Off Skates` = credit_osw
    )
}

load_transformed_practice_data <- function(data, meta) {
  
  data |>
    as_tibble() |> 
    select(
      timestamp,
      p_date = practice_date,
      p_type = practice_type,
      off_skates,
      in_attendance,
      duration_pop = how_long_was_your_pop_up,
      duration_osw = how_long_did_you_work_out
    ) |>
    mutate(
      across(
        everything(),
        ~ if_else(
          .x == "",
          NA,
          .x
        )
      )
    ) |> 
    mutate(
      timestamp = lubridate::mdy_hms(timestamp) |>
        lubridate::as_date(),
      p_date = lubridate::mdy(p_date),
      p_type = tolower(p_type)
    ) |> 
    mutate(
      p_type2 = case_when(
        stringr::str_detect(p_type, "\\(bw\\)") ~ "bw",
        stringr::str_detect(p_type, "\\(di\\)") ~ "di",
        stringr::str_detect(p_type, "\\(fmc\\)") ~ "fmc",
        stringr::str_detect(p_type, "\\(lge\\)") ~ "lge",
        stringr::str_detect(p_type, "\\(lm\\)") ~ "lm",
        stringr::str_detect(p_type, "\\(osw\\)") ~ "osw",
        stringr::str_detect(p_type, "\\(po\\)") ~ "pop",
        stringr::str_detect(p_type, "\\(star\\)") ~ "star",
        .default = "Uncategorized"
      ),
      .after = p_type
    ) |> 
    mutate(
      error = if_else(
        lubridate::year(timestamp) != lubridate::year(p_date),
        TRUE,
        FALSE
      ),
      .before = 1
    )
}

load_transformed_volunteer_data <- function(data) {
  data |> 
    as_tibble() |>
    select(
      timestamp,
      player = derby_name,
      player2 = who_are_you_though,
      month = month_of_volunteer_hours,
      volunteer_hrs = amount_of_volunteer_hours,
      purchased_hrs = how_many_hours_are_you_purchasing_10_hr_will_need_to_confirm_payment_before_credit_is_given,
      rollover_hrs = how_many_hours_will_you_be_rolling_over_this_month_from_last_month_keep_in_mind_that_you_cannot_roll_over_hours_if_you_have_already_completed_the_minimum_number_of_required_hours_for_the_current_month_you_can_roll_over_up_to_4_hours_from_the_previous_month_and_are_responsible_for_keeping_track_of_your_own_surplus_hours
    ) |>
    mutate(
      across(
        everything(),
        ~ if_else(
          .x == "",
          NA,
          .x
        )
      )
    ) |>
    mutate(
      timestamp = lubridate::mdy_hms(timestamp) |> lubridate::date(),
      year = lubridate::year(timestamp),
      month2 = lubridate::mdy(month) |> lubridate::month(label = TRUE),
      .before = timestamp
    ) |>
    mutate(
      month3 = if_else(
        is.na(month2),
        month,
        month2
      ) |>
        as.character() |>
        tolower()
    ) |>
    # count(month3) |> print(n = 24)
    mutate(
      month = case_when(
        stringr::str_detect(month3, "jan") ~ "jan",
        stringr::str_detect(month3, "feb") ~ "feb",
        stringr::str_detect(month3, "mar") ~ "mar",
        stringr::str_detect(month3, "apr") ~ "apr",
        stringr::str_detect(month3, "may") ~ "may",
        stringr::str_detect(month3, "jun") ~ "jun",
        stringr::str_detect(month3, "jul") ~ "jul",
        stringr::str_detect(month3, "aug") ~ "aug",
        stringr::str_detect(month3, "sep") ~ "sep",
        stringr::str_detect(month3, "oct") ~ "oct",
        stringr::str_detect(month3, "nov") ~ "nov",
        stringr::str_detect(month3, "dec") ~ "dec",
        .default = NA
      ),
      month_year = lubridate::my(
        paste(month, year, sep = "-")
      ),
      player = if_else(
        is.na(player),
        player2,
        player
      ) |> 
        tolower()
    ) |>
    # count(as.character(month)) |> print(n = 24)
    select(
      month_year,
      year,
      month,
      player,
      matches("hrs$")
    ) |>
    mutate(
      across(
        matches("hrs$"),
        ~ readr::parse_number(.x)
      ),
      total_hrs = rowSums(
        across(
          matches("hrs$")
        ),
        na.rm = TRUE
      ),
      .before = volunteer_hrs
    ) |>
    group_by(
      month_year,
      year,
      month,
      player
    ) |>
    reframe(
      across(
        matches("hrs$"),
        ~ sum(.x, na.rm = TRUE)
      )
    )
}

compute_volunteer_status <- function(meta, volunteer, game) {
  m <- meta |> filter(game == !!game)
  
  volunteer |>
    filter(
      year == lubridate::year(m$start) &
        stringr::str_detect(
          tolower(m$volunteer_month),
          month
        )
    ) |>
    arrange(
      desc(month_year)
    ) |>
    select(
      - month,
      - month_year,
      - year
    ) |>
    mutate(
      hrs_reqd = m$volunteer_hr_req,
      status = if_else(
        total_hrs >= hrs_reqd,
        "Complete",
        paste(
          round(abs(total_hrs - hrs_reqd), 1),
          "h reqd"
        )
      ),
      .before = volunteer_hrs
    ) |>
    select(
      player,
      status,
      volunteer_hrs,
      purchased_hrs,
      rollover_hrs
    ) |> 
    mutate(
      game = game,
      .before = 1
    ) |> 
    arrange(
      desc(volunteer_hrs)
    )
}


# General -----------------------------------------------------------------

pretty_datetime <- function(datetime,
                            timezone = "America/Los_Angeles") {
  datetime |> 
    lubridate::as_datetime() |> 
    format("%a, %d %b, %Y | %H:%M %p", tz = timezone)
}

