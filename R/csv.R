#' Parse a WOD CSV file
#'
#' Parses WOD CSV files, the CSV format is documented here:
#' https://www.nodc.noaa.gov/OC5/SELECT/dbsearch/csv_info.html
#'
#' @param filename
#' @keywords csv
#' @export
wod_csv <- function(filename) {
  casts <- NULL
  cast <- list()
  con <- file(filename, "r")
  state <- NULL
  while (TRUE) {

    line <- readLines(con, n = 1)
    if (length(line) == 0) {
      break
    }

    # start of new cast

    if (startsWith(line, "#--------------------")) {
      if (length(cast) > 0) {
        casts <- append(casts, list(cast))
      }
      cast <- list()
    }

    # parse cast header

    else if (startsWith(line, "CAST")) {
      cast_meta <- parse_meta(line)
      cast$cast_number <- cast_meta$value
      state <- "cast"
    }

    else if (startsWith(line, "METADATA")) {
      state <- "metadata"
    }

    # parse cast metadata line

    else if (state == "cast") {
      cast_meta <- parse_meta(line)

      # parse cruise id

      if (cast_meta$keyword == "NODC Cruise ID") {
        cast$cruise_id = cast_meta$value
      }

      # parse coordinates

      else if (cast_meta$keyword == "Latitude") {
        cast$latitude = as.numeric(cast_meta$value)
      }
      else if (cast_meta$keyword == "Longitude") {
        cast$longitude = as.numeric(cast_meta$value)
      }

      # parse time

      else if (cast_meta$keyword == "Year") {
        cast$year = as.numeric(cast_meta$value)
      }
      else if (cast_meta$keyword == "Month") {
        cast$month = as.numeric(cast_meta$value)
      }
      else if (cast_meta$keyword == "Day") {
        cast$day = as.numeric(cast_meta$value)
      }
      else if (cast_meta$keyword == "Time") {
        cast$time = as.numeric(cast_meta$value)
      }

    }

  }
  close(con)
  return(casts)
}

parse_meta <- function(line) {
  parts <- trimws(unlist(strsplit(line, ",")))
  parts[parts == ""] <- NA
  setNames(as.list(parts), c("keyword", "variable", "value", "type", "description"))
}

