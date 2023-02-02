#' Get geological timescale data
#'
#' This function takes a name of a geological timescale and returns data for the timescale.
#' Valid names include those of built-in `data.frames` ([periods()],
#' [epochs()], [stages()], [eons()], or [eras()]),
#' partial matches of those names (e.g., "per" or "stage"),
#' and exact matches to those hosted by Macrostrat (see full list here:
#' <https://macrostrat.org/api/defs/timescales?all>).
#'
#' @param name The name of the desired timescale.
#' @return A `data.frame` with the following columns:
#'   \item{name}{the names of the time intervals.}
#'   \item{max_age}{the oldest boundaries of the time intervals, in millions of years.}
#'   \item{min_age}{the youngest boundaries of the time intervals, in millions of years.}
#'   \item{abbr}{either traditional abbreviations of the names of the time intervals (if they exist) or custom abbreviations created with R.}
#'   \item{color}{hex color codes associated with the time intervals (if applicable).}
#' @importFrom utils read.csv
#' @importFrom curl nslookup
#' @export
get_scale_data <- function(name) {
  possible_names <- c("periods", "epochs", "stages", "eras", "eons")
  name_match <- charmatch(name, possible_names)
  if (!is.na(name_match)) {
    if (name_match == 0) {
      stop("'name' matches multiple scales. Please be more specific.",
        call. = FALSE
      )
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
    tryCatch(
      {
        nslookup("macrostrat.org")
      },
      error = function(e) {
        stop("Macrostrat is not available. Either the site is down or you are not connected to the internet.",
          call. = FALSE
        )
      }
    )
    URL <- url(paste0("https://macrostrat.org/api/v2/defs/intervals?format=csv&timescale=", gsub(" ", "%20", name)))
    raw_dat <- tryCatch(
      {
        read.csv(URL, header = TRUE, stringsAsFactors = FALSE)
      },
      error = function(e) {
        stop("'name' does not match a built-in or Macrostrat timescale.",
          call. = FALSE
        )
      }
    )
    clean_dat <- raw_dat[, c("name", "b_age", "t_age", "abbrev", "color")]
    colnames(clean_dat) <- c("name", "max_age", "min_age", "abbr", "color")
    no_abbr <- (is.na(clean_dat$abbr) | clean_dat$abbr == "")
    clean_dat$abbr[no_abbr] <- abbreviate(clean_dat$name, minlength = 1, use.classes = FALSE, named = FALSE)[no_abbr]
    dat <- clean_dat
  }
  dat
}


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
  lifecycle::deprecate_warn("1.0.0", "getScaleData()", "get_scale_data()")
  get_scale_data(name)
}
