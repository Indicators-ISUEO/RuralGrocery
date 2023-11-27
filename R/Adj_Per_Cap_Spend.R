#' Calculates the Adjusted per Capita Spending Value
#'
#' @description
#' Calculates the adjusted per capita grocery spending based on estimated cumulative price
#' increases nationally and for a given state.
#'
#' @param Total_Grocery_Sales The total volume of grocery sales in the US. The default
#' is set to 811,541,000,000 USD taken from IBIS World.
#' @param Total_Pop Total US population. The default is set to 334,233,854 for the
#' year of 2022.
#' @param Households Total number of US households. The default is at 131,200,000 based
#' based on statista.com.
#' @param Weekly_Household_Spending Average weekly spending per household. The default
#' is set to 120 USD based on Vertical IQ
#' @param Cumulative_Price_Inc Estimated cumulative price increase. Value should be entered
#' as an integer.
#' @param State_Index Pricing index for the state. Value should be entered as an
#' integer.
#'
#' @author Harun Celik
#' @export


Adj_Per_Cap_Spend <- function(Total_Grocery_Sales = 811541000000,
                              Total_Pop = 334233854,
                              Households = 131200000,
                              Weekly_Household_Spending = 120,
                              Cumulative_Price_Inc = NULL,
                              State_Index = 100) {

  # Calculate Average Grocery Per Capita
  Avg_Grocery_Per_Capita <- round(Total_Grocery_Sales / Total_Pop, digits = 2)

  # If a CPI is indicated, then calculated adjusted price accordingly.
  if (!is.null(Cumulative_Price_Inc)) {
    Adjusted_CPI_US <- round(((1 + (Cumulative_Price_Inc/100)) *
                                Avg_Grocery_Per_Capita), digits = 2)
  } else {
    Adjusted_CPI_US <- Avg_Grocery_Per_Capita
  }

  # Adjusted value by State Index
  if (State_Index != 100) {
    Adjusted_CPI_State <- round(State_Index *
                                  (Adjusted_CPI_US/100), digits = 2)
  } else {
    Adjusted_CPI_State <- Adjusted_CPI_US
  }

  Adjusted_Grocery_Spend_List <- list(Avg_Grocery_Per_Capita = Avg_Grocery_Per_Capita,
                                      Adjusted_CPI_US = Adjusted_CPI_US,
                                      Adjusted_CPI_State = Adjusted_CPI_State,
                                      Total_Grocery_Sales = Total_Grocery_Sales,
                                      Total_Pop = Total_Pop
                                      Cumulative_Price_Inc = Cumulative_Price_Inc
                                      State_Index = State_Index)
}
