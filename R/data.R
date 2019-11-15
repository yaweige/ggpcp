#' NASA - Data Expo 2006
#'
#' The data are geographic and atmospheric measures on a very coarse
#' 24 by 24 grid covering Central America. This data was provided by
#' the NASA Langley Research Center Atmospheric Sciences Data Center
#' as part of the ASA Data Expo in 2006. Monthly averages of a set of
#' atmospheric measurements are provided for Jan 1995 to Dec 2000.
#' A subset of this data is available from the `GGally` package.
#'
#' @format A data frame with 41472 (= 24 x 24 x 72) rows and 15 variables:
#' @section Structural variables:
#' \describe{
#'     \item{time}{time index for each month from 1 (= Jan 1995) to 72 (= Dec 2000)}
#'     \item{id}{identifier for each grid point 1-1 to 24-24}
#'     \item{lat, long}{geographic latitude and longitude}
#'     \item{elevation}{altitude of the location in meters above sea level}
#'     \item{month, year, date}{year/month of each measurement}
#'   }
#' @section Measured variables:
#' \describe{
#'   \item{cloudlow, cloudmid, cloudhigh}{Cloud cover (in percent) at low, middle, and high levels.}
#'   \item{ozone}{mean ozone abundance (in dobson)}
#'   \item{pressure}{mean surface pressure (in millibars)}
#'   \item{surftemp, temperature}{mean surface/near surface air temperature (in Kelvin)}
#' }
#'
#' @source \url{http://stat-computing.org/dataexpo/2006/}
#' @docType data
#' @name nasa
#' @usage nasa
#' @examples
#' data(nasa)
#' nasa %>%
#'   filter(id == "1-10") %>%
#'   ggplot(aes(vars = vars(starts_with("cloud"),
#'                          ozone, temperature))) +
#'    geom_pcp(aes(colour=month))
"nasa"
