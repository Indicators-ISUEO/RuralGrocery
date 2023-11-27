#' Returns Interest Calculations for Loans
#'
#' @description
#' This function returns a data frame of loans with calculated interest values.
#'
#' @param interest_table a data frame of the loan and interest values for calculating interest amount on loans.
#'
#' @author Harun Celik
#' @export

Interest <- function(interest_table) {

  interest_table$interest <- (interest_table$interest / 100)

  interest_table$calculated_interest <- (interest_table$loan * interest_table$interest)

  total_interest <- sum(interest_table$calculated_interest, na.rm = T)

  Interest_List <- list(interest_table = interest_table,
                        total_interest = total_interest)

}
