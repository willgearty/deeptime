#' Period data based on The Geologic Time Scale 2012
#'
#' A dataset containing the boundary ages, abbrevations, and colors for the periods of the Geologic Time Scale.
#' Based on The Geologic Time Scale 2012, by Gradstein and Ogg.
#'
#' @format A data frame with 15 rows and 5 variables:
#' \describe{
#'   \item{period}{period name}
#'   \item{max_age}{maximum age, in millions of years}
#'   \item{min_age}{minimum age, in millions of years}
#'   \item{abbr}{period name abbreviations}
#'   \item{color}{the colors for each period, according to the Comission for the Geological Map of the World}
#' }
#' @source \url{https://doi.org/10.1016/C2011-1-08249-8}
"periods"

#'   The \code{period} column lists the names of each time interval. These will be used as labels if no abbreviations are provided.
#'   The \code{max_age} column lists the oldest boundary of each time interval.
#'   The \code{min_age} column lists the youngest boundary of each time interval.
#'   The \code{abbr} column is optonal and lists abbreviations that may be used as labels.
#'   The \code{color} column is also optional and lists a hex color code (which can be obtained with \code{rgb()}) for each time interval.
