#' Returns Calculated Populations for a Determined Market Area
#'
#' @description This function returns a list of populations used for determining
#' the primary, secondary, and rare shoppers in a calculated market area.
#'
#' @param county_pct percentage of market area inside of a county.
#' Value calculated by @seealso [County_Pct()].
#' @param metro_pop population of town in which the grocery store is in.
#' @param county_pop total population of county in which the grocery store is in.
#' @param county_cities a list of populations for cities within the county of the grocery store.
#' @param non_county_cities a list of populations for cities within the market area but
#' outside the county of the grocery store.
#'
#' @author Harun Celik
#' @export

Market_Populations <-  function(county_pct, metro_pop, county_pop, county_cities, non_county_cities) {

  # Sum the list of county and non-county cities #

  ## Take the sum of all the county cities
  county_cities_pop <- Reduce("+", county_cities)
  ## Take the sum of all non-county cities
  non_county_cities_pop <- Reduce("+", non_county_cities)


  # Calculating the rural population for the market. #

  ## Calculate total rural population
  total_town_pop <- (metro_pop + county_cities_pop + non_county_cities_pop)
  total_rural_pop <- (county_pop - total_town_pop)
  ## Multiply the county percentage with the total rural population
  market_rural_pop <- floor(total_rural_pop * county_pct)

  # Calculating the town population (metro + market rural pop + non county cities)
  total_market_pop <- (metro_pop + market_rural_pop + non_county_cities_pop)

  # Return List
  Populations_List <- list(metro_pop = metro_pop,
                           rural_market_pop = market_rural_pop,
                           non_county_cities_pop = non_county_cities_pop,
                           total_market_pop = total_market_pop
                           total_town_pop = total_town_pop)

  return(Populations_List)
}
