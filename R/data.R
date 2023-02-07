#' Eon data from the International Commission on Stratigraphy (v2022/10)
#'
#' A dataset containing the boundary ages, abbreviations, and colors for the
#' eons of the Geologic Time Scale. Based on The ICS International
#' Chronostratigraphic Chart (v2022/10), by Cohen, Finney, Gibbard, and Fan.
#'
#' @format A data frame with 3 rows and 5 variables:
#' \describe{
#'   \item{name}{eon name}
#'   \item{max_age}{maximum age, in millions of years}
#'   \item{min_age}{minimum age, in millions of years}
#'   \item{abbr}{eon name abbreviations}
#'   \item{color}{the colors for each eon, according to the Commission for the
#'     Geological Map of the World}
#' }
#' @family timescales
#' @source <https://stratigraphy.org> via <https://macrostrat.org/api/v2/defs/intervals?timescale=international%20eons>
"eons"

#' Era data from the International Commission on Stratigraphy (v2022/10)
#'
#' A dataset containing the boundary ages, abbreviations, and colors for the
#' eras of the Geologic Time Scale. Based on The ICS International
#' Chronostratigraphic Chart (v2022/10), by Cohen, Finney, Gibbard, and Fan.
#'
#' @format A data frame with 10 rows and 5 variables:
#' \describe{
#'   \item{name}{era name}
#'   \item{max_age}{maximum age, in millions of years}
#'   \item{min_age}{minimum age, in millions of years}
#'   \item{abbr}{era name abbreviations}
#'   \item{color}{the colors for each era, according to the Commission for the
#'     Geological Map of the World}
#' }
#' @family timescales
#' @source <https://stratigraphy.org> via <https://macrostrat.org/api/v2/defs/intervals?timescale=international%20eras>
"eras"

#' Period data from the International Commission on Stratigraphy (v2022/10)
#'
#' A dataset containing the boundary ages, abbreviations, and colors for the
#' periods of the Geologic Time Scale. Based on The ICS International
#' Chronostratigraphic Chart (v2022/10), by Cohen, Finney, Gibbard, and Fan.
#'
#' @format A data frame with 22 rows and 5 variables:
#' \describe{
#'   \item{name}{period name}
#'   \item{max_age}{maximum age, in millions of years}
#'   \item{min_age}{minimum age, in millions of years}
#'   \item{abbr}{period name abbreviations}
#'   \item{color}{the colors for each period, according to the Commission for
#'     the Geological Map of the World}
#' }
#' @family timescales
#' @source <https://stratigraphy.org> via <https://macrostrat.org/api/v2/defs/intervals?timescale=international%20periods>
"periods"

#' Epoch data from the International Commission on Stratigraphy (v2022/10)
#'
#' A dataset containing the boundary ages, abbreviations, and colors for the
#' epochs of the Geologic Time Scale. Based on The ICS International
#' Chronostratigraphic Chart (v2022/10), by Cohen, Finney, Gibbard, and Fan.
#'
#' @format A data frame with 34 rows and 5 variables:
#' \describe{
#'   \item{name}{epoch name}
#'   \item{max_age}{maximum age, in millions of years}
#'   \item{min_age}{minimum age, in millions of years}
#'   \item{abbr}{epoch name abbreviations}
#'   \item{color}{the colors for each epoch, according to the Commission for the
#'     Geological Map of the World}
#' }
#' @family timescales
#' @source <https://stratigraphy.org> via <https://macrostrat.org/api/v2/defs/intervals?timescale=international%20epochs>
"epochs"

#' Stage data from the International Commission on Stratigraphy (v2022/10)
#'
#' A dataset containing the boundary ages, abbreviations, and colors for the
#' stages of the Geologic Time Scale. Based on The ICS International
#' Chronostratigraphic Chart (v2022/10), by Cohen, Finney, Gibbard, and Fan.
#'
#' @format A data frame with 102 rows and 5 variables:
#' \describe{
#'   \item{name}{stage name}
#'   \item{max_age}{maximum age, in millions of years}
#'   \item{min_age}{minimum age, in millions of years}
#'   \item{abbr}{stage name abbreviations}
#'   \item{color}{the colors for each stage, according to the Commission for the
#'     Geological Map of the World}
#' }
#' @family timescales
#' @source <https://stratigraphy.org> via <https://macrostrat.org/api/v2/defs/intervals?timescale=international%20ages>
"stages"
