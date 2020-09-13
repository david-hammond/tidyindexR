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
  z <- c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
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
  return(y)
}

#' Summarise Raw Data
#'
#' This analyses raw index data and summarises the data
#'
#' @param df dataframe in tidyindexR format
#'
#' @return Returns a summary of the raw data.
#'
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @keywords summarise
#' @export
#' @author David Hammond

index_data_summarise = function(df){
  df = df %>% dplyr::group_by(.data$variablename) %>%
    dplyr::summarise(num_geos = length(unique(.data$geocode)),
                     earliest = min(.data$year),
                     latest = max(.data$year),
                     lowest_score = paste(unique(.data$geocode[.data$value == min(.data$value)])[1], round(min(.data$value),2)),
                     highest_score = paste(unique(.data$geocode[.data$value == max(.data$value)])[1], round(max(.data$value), 2)),
                     min_value =  min(.data$value, na.rm = T),
                     max_value = max(.data$value, na.rm = T),
                     lower_iqr = outliers(.data$value)[1],
                     upper_iqr = outliers(.data$value)[2]) %>% dplyr::ungroup()

  return(df)
}
