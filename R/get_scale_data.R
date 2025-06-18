#' Get geological timescale data
#'
#' This function takes a name of a geological timescale and returns data for the
#' timescale. Valid names include those of built-in `data.frames` ([periods()],
#' [epochs()], [stages()], [eons()], or [eras()]), partial matches of those
#' names (e.g., "per" or "age"), and partial or exact matches to those hosted
#' by Macrostrat (see Details below). Note that the colors in the built-in
#' `data.frames` are according to the Commission for the Geological Map of the
#' World. If you would like to obtain custom Macrostrat colors that are better
#' for mapping, you should specify the full name of a timescale (e.g.,
#' "international periods") and set `true_colors` to `FALSE`. Note that these
#' colors only vary for the Precambrian.
#'
#' @details The following timescales are available from the Macrostrat API as of
#'   `r Sys.Date()`:
#'   \itemize{
#'     `r macrostrat_timescales()`
#'   }
#'
#'   The most up-to-date list can be found via the Macrostrat API [here](https://macrostrat.org/api/defs/timescales?all).
#'
#' @param name The name of the desired timescale.
#' @param true_colors Return original international time scale colors? (as
#'   opposed to custom Macrostrat plotting colors)
#' @return A `data.frame` with the following columns:
#'   \item{name}{the names of the time intervals}
#'   \item{max_age}{the oldest boundaries of the time intervals, in millions of
#'     years}
#'   \item{min_age}{the youngest boundaries of the time intervals, in millions
#'     of years}
#'   \item{abbr}{either traditional abbreviations of the names of the time
#'     intervals (if they exist) or custom abbreviations created with R}
#'   \item{color}{hex color codes associated with the time intervals (if
#'     applicable)}
#'   \item{lab_color}{default label colors for the time interals, either white
#'     or black, whichever has better contrast with the background color, based
#'     on [recommendations by the International Telecommunication Union](https://www.itu.int/rec/R-REC-BT.601-7-201103-I/en)}
#' @importFrom utils read.csv
#' @importFrom curl nslookup
#' @export
get_scale_data <- function(name, true_colors = TRUE) {
  check_required(name)
  check_bool(true_colors)
  possible_names <- c("periods", "epochs", "stages", "eras", "eons")
  name_match <- grep(name, possible_names, ignore.case = TRUE)
  if (length(name_match) > 0) {
    if (length(name_match) > 1) {
      cli::cli_abort("`name` matches multiple built-in timescales. Please be
                     more specific.")
    } else {
      name <- possible_names[name_match]
      if (name == "periods") {
        dat <- deeptime::periods
      } else if (name == "epochs") {
        dat <- deeptime::epochs
      } else if (name == "stages") {
        dat <- deeptime::stages
      } else if (name == "eras") {
        dat <- deeptime::eras
      } else if (name == "eons") {
        dat <- deeptime::eons
      }
    }
  } else {
    # try to get the timescale from macrostrat
    # check that we are online and macrostrat is online
    tryCatch(# nocov start
      {
        nslookup("macrostrat.org")
      },
      error = function(e) {
        cli::cli_abort("Macrostrat is not available. Either the site is down or
                       you are not connected to the internet.")
      }
    )# nocov end
    avail_scales <- read.csv(url(paste0("https://macrostrat.org/api/v2/defs/",
                                        "timescales?all&format=csv")),
                             stringsAsFactors = FALSE)
    # check exact matches
    name_match <- match(tolower(name), tolower(avail_scales$timescale))
    if (is.na(name_match)) {
      # check partial matches
      name_match <- grep(name, avail_scales$timescale, ignore.case = TRUE)
      if (length(name_match) == 0) {
        cli::cli_abort("`name` does not match a built-in or Macrostrat
                     timescale.")
      } else if (length(name_match) > 1) {
        cli::cli_abort("`name` matches multiple Macrostrat timescales. Please be
                     more specific.")
      }
    }
    # get full timescale name and retrieve data from Macrostrat
    name <- avail_scales$timescale[name_match]
    URL <- url(paste0("https://macrostrat.org/api/v2/defs/intervals",
                      "?format=csv&timescale=", gsub(" ", "%20", name),
                      ifelse(true_colors, "&true_colors=true", "")))
    raw_dat <- tryCatch(
      {
        read.csv(URL, header = TRUE, stringsAsFactors = FALSE)
      },
      error = function(e) {# nocov start
        cli::cli_abort("Macrostrat is not available. Either the site is down
                       or you are not connected to the internet.")
      }# nocov end
    )
    clean_dat <- raw_dat[, c("name", "b_age", "t_age", "abbrev", "color")]
    colnames(clean_dat) <- c("name", "max_age", "min_age", "abbr", "color")
    no_abbr <- (is.na(clean_dat$abbr) | clean_dat$abbr == "")
    clean_dat$abbr[no_abbr] <-
      abbreviate(clean_dat$name, minlength = 1,
                 use.classes = TRUE, named = FALSE)[no_abbr]
    dat <- clean_dat
    dat$lab_color <- white_or_black(dat$color)
  }
  dat
}

# Determine best label colors based on luminance as per
# https://stackoverflow.com/a/1855903/4660582
# values are from https://www.itu.int/rec/R-REC-BT.601-7-201103-I/en
#' @importFrom grDevices col2rgb
white_or_black <- function(colors) {
  rgbs <- col2rgb(colors)
  luminance <- apply(rgbs, 2, function(x) {
    (0.299 * x[1] + 0.587 * x[2] + 0.114 * x[3]) / 255
  })
  ifelse(luminance > .5, "black", "white")
}

# generate docs for available Macrostrat timescales
macrostrat_timescales <- function() {# nocov start
  scales <- read.csv(url(paste0("https://macrostrat.org/api/v2/defs/timescales",
                                "?all&format=csv")),
                     stringsAsFactors = FALSE)
  return(paste0("\\item ", scales$timescale, collapse = "\n"))
}# nocov end

#' Get geological timescale data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `getScaleData()` was renamed to [get_scale_data()] as of **deeptime** version
#' 1.0.0 to create a more consistent API.
#' @keywords internal
#' @export
getScaleData <- function(name) {
  lifecycle::deprecate_stop("1.0.0", "getScaleData()", "get_scale_data()")
}
