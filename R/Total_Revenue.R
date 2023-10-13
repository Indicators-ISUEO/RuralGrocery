#' Calculates Spending Based on Number of Shoppers and Percentage Spent on Groceries
#'
#' @description calculates the amount of grocery spending for primary, secondary, and rare shoppers
#' through a provided percentage and aggregates the spending for a total estimated revenue value.
#'
#' @param Adjusted_Per_Capita dollar value of annual spending on groceries per capita
#' adjusted by CPI and state inflation.
#' @param Primary_Pct percentage of income spent by primary shoppers on groceries. Default set to 60 percent.
#' @param Secondary_Pct percentage of income spent by secondary shoppers on groceries. Default set to 25 percent.
#' @param Rare_Pct percentage of income spent by rare shoppers on groceries. Default set to 5 percent.
#' @param Primary_Shoppers total number of primary shoppers calculated in @seealso [Shopper_Populations()].
#' @param Secondary_Shoppers total number of secondary shoppers calculated in @seealso [Shopper_Populations()].
#' @param Rare_Shoppers total number of rare shoppers calculated in @seealso [Shopper_Populations()].
#'
#'
#' @author Harun Celik
#' @export


Total_Revenue <- function(Adjusted_Per_Capita,
                          Primary_Pct,
                          Secondary_Pct,
                          Rare_Pct,
                          Primary_Shoppers,
                          Secondary_Shoppers,
                          Rare_Shoppers) {

  # Amount Spent Per Capita by Each Shopper Category (annual spending of individuals)
  Primary_Spending_Capita <- floor(Adjusted_Per_Capita * (Primary_Pct/100))
  Secondary_Spending_Capita <- floor(Adjusted_Per_Capita * (Secondary_Pct/100))
  Rare_Spending_Capita <- floor(Adjusted_Per_Capita * (Rare_Pct/100))

  # Total Amounts by Shopper Category
  Primary_Spending_Total <- Primary_Spending_Capita * Primary_Shoppers
  Secondary_Spending_Total <- Secondary_Spending_Capita * Secondary_Shoppers
  Rare_Spending_Total <- Rare_Spending_Capita * Rare_Shoppers

  # Total Revenue
  Total_Estimated_Revenue <- Primary_Spending_Total + Secondary_Spending_Total + Rare_Spending_Total

  # Return Items in a List
  Total_Revenue_List <- list(Primary_Spending_Capita = Primary_Spending_Capita,
                             Secondary_Spending_Capita = Secondary_Spending_Capita,
                             Rare_Spending_Capita = Rare_Spending_Capita,
                             Primary_Spending_Total = Primary_Spending_Total,
                             Secondary_Spending_Total = Secondary_Spending_Total,
                             Rare_Spending_Total = Rare_Spending_Total,
                             Total_Estimated_Revenue = Total_Estimated_Revenue)

  return(Total_Revenue_List)

}
