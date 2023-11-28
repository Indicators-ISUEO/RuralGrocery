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

  # Only operate on rows which take percentage values for calculation
  filtered_expense <- expense_table[(expense_table$id %in% c("gross_margin",
                                                             "officer_compensation",
                                                             "salaries",
                                                             "other_operating_expense",
                                                             "other_income")), ]

  # Calculate the value of these rows
  filtered_expense$pct <- (filtered_expense$pct / 100)

  filtered_expense$value <- (total_estimated_revenue * filtered_expense$pct)

  # Transferring the values of the calculations to expense_table
  expense_table$value[1:5] <- filtered_expense$value[1:5]


  # Calculate remaining expenses from values in filtered_expense

  ## Cost of Goods
  index <- which(expenses$id == "cost_of_goods")
  expense_table$value[index] <- total_estimated_revenue - expense_table$value[which(expense_table$id == "gross_margin")]
  expense_table$pct[index] <- ((expense_table$value[index] / total_estimated_revenue) * 100)

  ## Rent
  index <- which(expenses$id == "rent")
  expense_table$value[index] <- total_rent
  expense_table$pct[index] <- ((expense_table$value[index] / total_estimated_revenue) * 100)

  ## Depreciation
  index <- which(expenses$id == "depreciation")
  expense_table$value[index] <- total_depreciation
  expense_table$pct[index] <- ((expense_table$value[index] / total_estimated_revenue) * 100)

  ## Interest_Expense
  index <- which(expenses$id == "interest_expense")
  expense_table$value[index] <- total_interest
  expense_table$pct[index] <- ((expense_table$value[index] / total_estimated_revenue) * 100)

  ## Total Expense
  index <- which(expenses$id == "total_expense")
  expense_table$value[index] <- (expense_table$value[which(expense_table$id == "officer_compensation")] +
                                   expense_table$value[which(expense_table$id == "salaries")] +
                                   expense_table$value[which(expense_table$id == "rent")] +
                                   expense_table$value[which(expense_table$id == "other_operating_expense")] +
                                   expense_table$value[which(expense_table$id == "depreciation")])
  expense_table$pct[index] <- ((expense_table$value[index] / total_estimated_revenue) * 100)

  ## Operating Income Loss
  index <- which(expenses$id == "operating_income_loss")
  expense_table$value[index] <- (expense_table$value[which(expense_table$id == "gross_margin")] -
                                   expense_table$value[which(expense_table$id == "total_expense")])
  expense_table$pct[index] <- ((expense_table$value[index] / total_estimated_revenue) * 100)

  ## Pre-tax Profit
  index <- which(expenses$id == "pre_tax_profit")
  expense_table$value[index] <- (expense_table$value[which(expense_table$id == "operating_income_loss")] -
                                   expense_table$value[which(expense_table$id == "interest_expense")] +
                                   expense_table$value[which(expense_table$id == "other_income")])
  expense_table$pct[index] <- ((expense_table$value[index] / total_estimated_revenue) * 100)

  return(expense_table)

}
