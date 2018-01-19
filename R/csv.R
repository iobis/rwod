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
      state <- NULL
      if (length(cast) > 0) {
        casts <- append(casts, list(cast))
      }
      cast <- list()
    }

    # manage state

    else if (startsWith(line, "CAST")) {
      cast_meta <- parse_meta(line)
      cast$cast_number <- cast_meta$value
      state <- "cast"
    }
    else if (startsWith(line, "METADATA")) {
      state <- "metadata"
    }
    else if (startsWith(line, "BIOLOGY METADATA,")) {
      state <- "biology metadata"
    }
    else if (startsWith(line, "BIOLOGY,")) {
      cast$biology <- list()
      state <- "biology"
    }
    else if (startsWith(line, "END OF")) {
      state <- NULL
    }

    # parse cast metadata

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

    # parse metadata

    else if (state == "metadata") {
      cast_meta <- parse_meta(line)

      # country

      if (cast_meta$keyword == "Country") {
        cast$country = cast_meta$value
      }

      # identifiers

      else if (cast_meta$keyword == "Platform") {
        cast$platform = cast_meta$value
      }
      else if (cast_meta$keyword == "Institute") {
        cast$institute = cast_meta$value
      }
      else if (cast_meta$keyword == "Accession Number") {
        cast$accession_number = cast_meta$value
      }

    }

    # parse biology metadata

    else if (state == "biology metadata") {
      meta <- parse_meta(line)

      if (meta$keyword == "Mesh size") {
        cast$mesh_size <- c(meta$value, meta$type, meta$description)
      }
      else if (meta$keyword == "Type of tow") {
        cast$tow_type <- c(meta$value, meta$type, meta$description)
      }
      else if (meta$keyword == "Gear") {
        cast$gear <- c(meta$value, meta$type, meta$description)
      }
      else if (meta$keyword == "Net mouth area") {
        cast$net_mouth_area <- c(meta$value, meta$type, meta$description)
      }
      else if (meta$keyword == "Preservation method") {
        cast$preservation_method <- c(meta$value, meta$type, meta$description)
      }
      else if (meta$keyword == "Count method") {
        cast$count_method <- c(meta$value, meta$type, meta$description)
      }

    }

    # parse biology

    else if (state == "biology") {
      biology <- parse_biology(line)[]
      cast$biology <- append(cast$biology, list(biology))
    }

  }
  close(con)
  return(casts)
}

parse_meta <- function(line) {
  parts <- trimws(unlist(strsplit(line, ",")))
  parts[parts == ""] <- NA
  return(setNames(as.list(parts), c("keyword", "variable", "value", "type", "description")))
}

parse_biology <- function(line) {
  parts <- trimws(unlist(strsplit(line, ",")))
  parts <- c(parts[-1], NA)
  parts[parts == ""] <- NA
  return(setNames(as.list(parts), c("upper_z", "lower_z", "measurement_type", "original_value", "originator_flag", "original_units", "wod_cbv_value", "cbv_flag", "cbv_units", "calculation_method", "wod_pgc", "itis_tsn", "taxonomic_modifier", "sex", "lifestage", "trophic", "realm", "taxon_shape", "count_method", "min_size", "max_size", "taxon_length", "taxon_width", "taxon_radius", "sampled_volume")))
}

