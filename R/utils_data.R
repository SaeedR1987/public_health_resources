#' Safely Convert Character or POSIX Dates to Date (IPHRA Safe)
#'
#' Converts input to Date, strips time components, and raises structured IPHRA errors
#' if any value cannot be parsed. Works with Date, POSIX, numeric, or character inputs.
#'
#' @param x Vector of type character, Date, POSIXct/POSIXlt, or numeric.
#' @param origin Optional override for numeric conversion ("1970-01-01" or "1899-12-30").
#' @param origin_label Optional origin label for logging (defaults inferred).
#' @return Date vector of same length as input, with NAs preserved.
#' @export
convert_date <- function(x, origin = NULL, origin_label = NULL) {
  phr_try({
    # Already date
    if (inherits(x, "Date")) return(x)
    if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
    
    # Numeric
    if (is.numeric(x)) {
      if (is.null(origin)) {
        origin <- if (all(x > 20000 & x < 60000, na.rm = TRUE)) "1899-12-30" else "1970-01-01"
        origin_label <- origin
      }
      phr_message(phr_txt(glue::glue("Converting numeric dates using origin: {origin_label}.")))
      return(as.Date(x, origin = origin))
    }
    
    # Character numeric-like
    if (is.character(x) && all(grepl("^[0-9]+$", x[!is.na(x)]))) {
      x_num <- suppressWarnings(as.numeric(x))
      if (is.null(origin)) {
        origin <- if (all(x_num > 20000 & x_num < 60000, na.rm = TRUE)) "1899-12-30" else "1970-01-01"
      }
      phr_message(phr_txt(glue::glue("Converting numeric-like strings using origin: {origin}.")))
      return(as.Date(x_num, origin = origin))
    }
    
    # Convert to character
    x_vec <- as.character(x)
    is_na <- is.na(x_vec)
    x_to_parse <- x_vec[!is_na]
    if (length(x_to_parse) == 0) return(as.Date(rep(NA, length(x_vec))))
    
    x_clean <- trimws(x_to_parse)
    # Remove timezone suffixes
    x_clean <- gsub("\\s*(UTC|GMT|CET|CEST|EST|PST|EDT|PDT|\\+\\d{2}:\\d{2})$", "", x_clean, ignore.case = TRUE)
    # Remove ISO T timestamp fragments
    x_clean <- sub("T.*$", "", x_clean)
    
    if (any(grepl("\\d{2}:\\d{2}:\\d{2}", x_to_parse))) {
      phr_warning(phr_txt("Time components detected and will be removed during date conversion."))
    }
    
    # Parse using lubridate
    parsed <- suppressWarnings(lubridate::parse_date_time(
      x_clean,
      orders = c(
        "Y-m-d", "d/m/Y", "Y/m/d", "m-d-Y", "d-m-Y",
        "Y-m-d H:M:S", "Y/m/d H:M:S",
        "ymd", "dmy", "mdy", "Ymd HMS", "dmY HMS"
      ),,
      exact = FALSE
    ))
    
    parsed <- as.Date(parsed)
    
    # Identify unparsed values
    if (any(is.na(parsed))) {
      invalid_vals <- unique(x_to_parse[is.na(parsed)])
      phr_error(
        message = phr_txt(
          "Could not convert the following values to Date: {paste0('\"', invalid_vals, '\"', collapse = ', ')}."
        ),
        origin = "convert_date",
        hint   = "Ensure values match accepted formats: %Y-%m-%d, %d/%m/%Y, %Y/%m/%d, %m-%d-%Y, %d-%m-%Y, %Y-%m-%d %H:%M:%S, %Y/%m/%d %H:%M:%S, or %Y-%m-%dT%H:%M:%OSZ."
      )
    }
    
    # Reinsert original NAs
    result <- rep(NA, length(x_vec))
    result[!is_na] <- parsed
    class(result) <- "Date"
    
    result
    
  }, on_error = "abort", origin = "convert_date")
}
