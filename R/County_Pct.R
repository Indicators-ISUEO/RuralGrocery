#' Calculates the Percentage of Market Area Inside of the Given County
#'
#' @description Calculates the percentage of the market size in relation to the county.
#' Market area is divided by the county area if the market size area is smaller than county area.
#'
#' @param county_area total square mileage of the county.
#' @param market_total_area total square mileage of the market area.
#'
#' @importFrom R.oo throw
#'
#' @author Harun Celik
#' @export

County_Pct <- function(county_area, market_total_area) {


    # Calculate the pct of the market in the county area
  county_market_pct <- round((market_total_area/county_area), digits = 3)

  if (market_total_area >= county_area) {
    throw("market area is larger than county area: ", market_total_area)
  }


  return(county_market_pct)
}
