pretty_datetime <- function(datetime,
                            timezone = "America/Phoenix") {
  datetime |> 
    lubridate::as_datetime() |> 
    format("%a, %d %b, %Y | %I:%M %p", tz = timezone)
}