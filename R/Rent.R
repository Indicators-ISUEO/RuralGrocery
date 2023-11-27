#' Returns Annual Costs for Rent
#'
#' @description
#' This function returns a data frame of annual rent values. All rents are considered on
#' 12 month periods.
#'
#' @param rent_table a data frame of the monthly rent values for calculating annual rent costs.
#'
#' @author Harun Celik
#' @export

Rent <- function(rent_table) {

  rent_table$annual_cost <- rent_table$rent * 12

  total_rent <- sum(rent_table$annual_cost, na.rm = T)

  Rent_List <- list(rent_table = rent_table,
                    total_rent = total_rent)
}
