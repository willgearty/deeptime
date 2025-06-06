% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{periods}
\alias{periods}
\title{Period data from the International Commission on Stratigraphy (v2024/12)}
\format{
A data frame with 22 rows and 5 variables:
\describe{
\item{name}{period name}
\item{max_age}{maximum age, in millions of years}
\item{min_age}{minimum age, in millions of years}
\item{abbr}{period name abbreviations}
\item{color}{the colors for each period, according to the Commission for
the Geological Map of the World}
\item{lab_color}{the label colors for each period, either white or black,
whichever has better contrast with the background color, based on
\href{https://www.itu.int/rec/R-REC-BT.601-7-201103-I/en}{recommendations by the International Telecommunication Union}}
}
}
\source{
\url{https://stratigraphy.org} via \url{https://macrostrat.org/api/v2/defs/intervals?timescale=international\%20periods}
}
\usage{
periods
}
\description{
A dataset containing the boundary ages, abbreviations, and colors for the
periods of the Geologic Time Scale. Based on The ICS International
Chronostratigraphic Chart (v2024/12), by Cohen, Finney, Gibbard, and Fan.
}
\seealso{
Other built-in timescales: 
\code{\link{eons}},
\code{\link{epochs}},
\code{\link{eras}},
\code{\link{stages}}
}
\concept{built-in timescales}
\keyword{datasets}
