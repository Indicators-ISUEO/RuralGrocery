#' Returns Expense Calculations for Estimating Total Costs
#'
#' @description
#' Calculates the expense values for the Cost of Goods Sold, Gross Margin,
#' Officer Compensation, Salaries, Rent, Other Operating Expense, Depreciation,
#' Total Expense, Operating Income/(Loss), Interest Expense, Other Income,
#' Pre-Tax Net Profit/(Loss).
#'
#' @param total_estimated_revenue the total estimated revenue calculated in the revenue estimation
#' @param expense_table a data frame containing percentages to be used to calculate the cost values for expenses
#' @param total_depreciation total depreciation value calculated in [Depreciation]
#' @param total_rent total rent value calculated in [Rent]
#' @param total_interest total interest value calculated in [Interest]
#'
#' @author Harun Celik
#' @export

Expense <- function(total_estimated_revenue,
                    expense_table,
                    total_depreciation,
                    total_rent,
                    total_interest) {

  browser()
  # Only operate on rows which take percentage values for calculation
  filtered_expense <- expense_table[(expense_table$id %in% c("gross_margin",
                                                             "salaries",
                                                             "other_operating_expense",
                                                             "other_income")), ]

  # Calculate the value of these rows
  filtered_expense$pct <- (filtered_expense$pct / 100)

  filtered_expense$value <- (total_estimated_revenue * filtered_expense$pct)

  # Transferring the values of the calculations to expense_table
  expense_table$value[1:4] <- filtered_expense$value[1:4]


  # Calculate remaining expenses from values in filtered_expense
  ## Officer Compensation
  index <- which(expense_table$id == "officer_compensation")
  expense_table$pct[index] <- ((expense_table$value[index] / total_estimated_revenue) * 100)

  ## Cost of Goods
  index <- which(expense_table$id == "cost_of_goods")
  expense_table$value[index] <- total_estimated_revenue - expense_table$value[which(expense_table$id == "gross_margin")]
  expense_table$pct[index] <- ((expense_table$value[index] / total_estimated_revenue) * 100)

  ## Rent
  index <- which(expense_table$id == "rent")
  expense_table$value[index] <- total_rent
  expense_table$pct[index] <- ((expense_table$value[index] / total_estimated_revenue) * 100)

  ## Depreciation
  index <- which(expense_table$id == "depreciation")
  expense_table$value[index] <- total_depreciation
  expense_table$pct[index] <- ((expense_table$value[index] / total_estimated_revenue) * 100)

  ## Interest_Expense
  index <- which(expense_table$id == "interest_expense")
  expense_table$value[index] <- total_interest
  expense_table$pct[index] <- ((expense_table$value[index] / total_estimated_revenue) * 100)

  ## Total Expense
  index <- which(expense_table$id == "total_expense")
  expense_table$value[index] <- (expense_table$value[which(expense_table$id == "officer_compensation")] +
                                   expense_table$value[which(expense_table$id == "salaries")] +
                                   expense_table$value[which(expense_table$id == "rent")] +
                                   expense_table$value[which(expense_table$id == "other_operating_expense")] +
                                   expense_table$value[which(expense_table$id == "depreciation")])
  expense_table$pct[index] <- ((expense_table$value[index] / total_estimated_revenue) * 100)

  ## Operating Income Loss
  index <- which(expense_table$id == "operating_income_loss")
  expense_table$value[index] <- (expense_table$value[which(expense_table$id == "gross_margin")] -
                                   expense_table$value[which(expense_table$id == "total_expense")])
  expense_table$pct[index] <- ((expense_table$value[index] / total_estimated_revenue) * 100)

  ## Pre-tax Profit
  index <- which(expense_table$id == "pre_tax_profit")
  expense_table$value[index] <- (expense_table$value[which(expense_table$id == "operating_income_loss")] -
                                   expense_table$value[which(expense_table$id == "interest_expense")] +
                                   expense_table$value[which(expense_table$id == "other_income")])
  expense_table$pct[index] <- ((expense_table$value[index] / total_estimated_revenue) * 100)


  # Round pct and values to two decimal points
  expense_table$pct <- round(expense_table$pct, 2)
  expense_table$value <- round(expense_table$value, 2)

  return(expense_table)

}
