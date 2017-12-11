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
  while (TRUE) {

    line <- readLines(con, n = 1)
    if (length(line) == 0) {
      break
    }
    if (startsWith(line, "#--------------------")) {
      if (length(cast) > 0) {
        casts <- c(casts, cast)
      }
      cast <- list()
    } else if (startsWith(line, "CAST")) {
      meta_cast <- parse_line(line)
      cast$cast_number <- meta_cast$value
    }

  }
  close(con)
  return(casts)
}

parse_line <- function(line) {
  parts <- trimws(unlist(strsplit(line, ",")))
  parts[parts == ""] <- NA
  setNames(as.list(parts), c("keyword", "variable", "value", "type", "description"))
}

