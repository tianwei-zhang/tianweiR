#' Compute mode
#' @param x a list of numbers
#' @export


  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
