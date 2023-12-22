#' Calculates Primary, Secondary, and Rare Shopper Counts
#'
#' @description calculates the population of primary, secondary and rare shoppers for the metro
#' town, and rural populations of the calculated market area.
#'
#' @param populations_list list of populations calculated by @seealso [Market_Populations()].
#' @param metro_primary_pct percentage value for calculating shoppers of metro populations.
#' @param metro_secondary_pct percentage value for calculating shoppers of metro populations.
#' @param metro_rare_pct percentage value for calculating shoppers of metro populations.
#' @param town_primary_pct percentage value for calculating shoppers of town populations.
#' @param town_secondary_pct percentage value for calculating shoppers of town populations.
#' @param town_rare_pct percentage value for calculating shoppers of town populations.
#' @param rural_primary_pct percentage value for calculating shoppers of rural populations.
#' @param rural_secondary_pct percentage value for calculating shoppers of rural populations.
#' @param rural_rare_pct percentage value for calculating shoppers of rural populations.
#'
#' @author Harun Celik
#' @export

Shopper_Populations <- function(populations_list,
                                metro_primary_pct, metro_secondary_pct, metro_rare_pct,
                                town_primary_pct, town_secondary_pct, town_rare_pct,
                                rural_primary_pct, rural_secondary_pct, rural_rare_pct) {


  ## Calculating metro population values
  primary_metro <- (populations_list$metro_pop * metro_primary_pct)/100
  secondary_metro <- (populations_list$metro_pop * metro_secondary_pct)/100
  rare_metro <- (populations_list$metro_pop * metro_rare_pct)/100

  ## Calculating rural population values
  primary_rural <- (populations_list$rural_market_pop * rural_primary_pct)/100
  secondary_rural <- (populations_list$rural_market_pop * rural_secondary_pct)/100
  rare_rural <- (populations_list$rural_market_pop * rural_rare_pct)/100

  ## Calculating town populations
  primary_town <- (populations_list$market_cities_pop * town_primary_pct)/100
  secondary_town <- (populations_list$market_cities_pop * town_secondary_pct)/100
  rare_town <- (populations_list$market_cities_pop * town_rare_pct)/100

  ## Calculating primary, secondary, rare totals
  primary_total <- (primary_metro + primary_town + primary_rural)
  secondary_total <- (secondary_metro + secondary_town + secondary_rural)
  rare_total <- (rare_metro + rare_town + rare_rural)

  # Return Shoppers Count List

  Shoppers_List <- list(primary_metro = round(primary_metro),
                        primary_rural = round(primary_rural),
                        primary_town = round(primary_town),

                        secondary_metro = round(secondary_metro),
                        secondary_rural= round(secondary_rural),
                        secondary_town = round(secondary_town),

                        rare_metro = round(rare_metro),
                        rare_rural = round(rare_rural),
                        rare_town = round(rare_town),

                        primary_total = round(primary_total),
                        secondary_total = round(secondary_total),
                        rare_total = round(rare_total))

  return(Shoppers_List)
}
