#' Returns Expense Calculations for Estimating Total Costs
#'
#' @description
#' Calculates the expense values for the Cost of Goods Sold, Gross Margin,
#' Officer Compensation, Salaries, Rent, Other Operating Expense, Depreciation,
#' Total Expense, Operating Income/(Loss), Interest Expense, Other Income,
#' Pre-Tax Net Profit/(Loss)
#'
#' @param total_estimated_revenue the total estimated revenue calculated in the revenue estimation
#' @param expense_table a data frame containing percentages to be used to calculate the cost values for expenses
#'
#' @author Harun Celik
#' @export

Expense <- function(total_estimated_revenue, expense_table) {

  expense_table$pct <- (expense_table$pct / 100)

  expense_table$pct_value <- (total_estimated_revenue * expense_table$pct)

  total_expense <- sum(expense_table$pct_value, na.rm = T)

  Expense_List <- list(expense_table = expense_table,
                       total_expense = total_expense)


}
