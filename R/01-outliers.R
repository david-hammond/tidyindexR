#' Calculate IQR for outliers
#'
#' This calculates IQR range for indicators
#'
#' @param x vector of values
#' @return Returns lower and upper IQR values
#' @keywords summarise
#' @author David Hammond

outliers <- function(x) {
  qnt <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  H <- 1.5 * stats::IQR(x, na.rm = TRUE)
  x1 <- qnt[1] - H
  x2 <- qnt[2] + H
  y <- c(x1, x2)
  z <- c(min(x), max(x))
  if (y[1] < z[1])
    y[1] <- z[1]
  if (y[2] > z[2])
    y[2] <- z[2]
  if (diff(y) == 0) {
    y <- z
  }
  y <- c(y, 0)
  if (y[1] > z[1] | y[2] < z[2])
    y[3] <- 1
  y
}
