#' Returns Annual Costs for Depreciating Assets
#'
#' @description
#' This function returns a data frame of depreciation values for costs related to different scenarios of costs.
#' The returned values include annual depreciation values
#' for Building or Remodeling, Leasehold Improvements, Parking Lot Improvements,
#' Shelving and Check Out Counters, Computer Equipment and POS
#' Vehicles, Display Cases (frozen or refrigerated), Refrigeration, Freezers, Meat Cutting Equipment,
#' and the Amortization of Start-Up Costs.
#'
#' @param scenario scenario choices to determine which depreciation costs to calculate.
#' Input is a character of  `one`, `two`, or `three`.
#' @param depreciation_table a data frame of the depreciation values containing the cost and life
#' use values used for calculating annual depreciation.
#'
#' @author Harun Celik
#' @export

Depreciation <- function(scenario, depreciation_table) {

  # Scenario One: Building is owned by business that is operating the store.
  if (scenario == "one") {
    filtered_depreciation <- depreciation_table[!(depreciation$id %in% "leasehold_improvements"), ]

    filtered_depreciation$annual_cost <- (filtered_depreciation$cost / filtered_depreciation$life)

    total_depreciation <- sum(filtered_depreciation$annual_cost, na.rm=T)

    Depreciation_List <- list(depreciation_table = filtered_depreciation,
                              total_depreciation = total_depreciation)

    return(Depreciation_List)
  }

  # Scenario Two: Building owned by a third party and periodic rent is paid.
  if (scenario == "two") {
    filtered_depreciation <- depreciation_table[!(depreciation$id %in% c("building_remodeling", "parking_lot_improvements")), ]

    filtered_depreciation$annual_cost <- (filtered_depreciation$cost / filtered_depreciation$life)

    total_depreciation <- sum(filtered_depreciation$annual_cost, na.rm=T)

    Depreciation_List <- list(depreciation_table = filtered_depreciation,
                              total_depreciation = total_depreciation)

    return(Depreciation_List)
  }
}

