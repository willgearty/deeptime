#' Get geological timescale data
#'
#' This function takes a name of a geological timescale and returns data for the timescale.
#'
#' Valid names include those of built-in dataframes ("periods", "epochs", "stages", "eons", or "eras")
#' and those hosted by macrostrat (see list here: \url{https://macrostrat.org/api/defs/timescales?all}).
#'
#' @param name The name of the desired timescale.
#' @return A dataframe with the following columns:
#'   \item{name}{the names of the time intervals.}
#'   \item{max_age}{the oldest boundaries of the time intervals, in millions of years.}
#'   \item{min_age}{the youngest boundaries of the time intervals, in millions of years.}
#'   \item{abbr}{either traditional abbreviations of the names of the time intervals (if they exist) or custom abbreviations created with R.}
#'   \item{color}{hex color codes associated with the time intervals (if applicable).}
#' @export
getScaleData <- function(name){
  if(name == "periods"){
    dat <- deeptime::periods
  }else if(name == "epochs"){
    dat <- deeptime::epochs
  }else if(name == "stages"){
    dat <- deeptime::stages
  }else if(name == "eras"){
    dat <- deeptime::eras
  }else if(name == "eons"){
    dat <- deeptime::eons
  }else{
    #try to get the timescale from macrostrat
    URL <- url(paste0("https://macrostrat.org/api/v2/defs/intervals?format=csv&timescale=",gsub(" ", "%20", name)))
    raw_dat <- utils::read.csv(URL,header=TRUE, stringsAsFactors = FALSE)
    clean_dat <- raw_dat[,c("name", "b_age", "t_age", "abbrev", "color")]
    colnames(clean_dat) <- c("name", "max_age", "min_age", "abbr", "color")
    no_abbr <- (is.na(clean_dat$abbr) | clean_dat$abbr == "")
    clean_dat$abbr[no_abbr] <- abbreviate(clean_dat$name, minlength = 1, use.classes = FALSE, named = FALSE)[no_abbr]
    dat <- clean_dat
  }
  dat
}
