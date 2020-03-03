#' Summarise Raw Data
#'
#' This analyses raw index data and summarises the data
#'
#' @param df dataframe in tidyindexR format
#'
#' @return Returns a summary of the raw data.
#'
#' @examples
#' data(povstats)
#' df <- index_data_summarise(povstats)
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @keywords summarise
#' @author David Hammond
#' @export
index_data_summarise = function(df){
  num_variablenames = length(unique(df$variablename))
  tmp = df %>% dplyr::group_by(.data$variablename) %>% dplyr::summarise(num_geos = length(unique(.data$geocode)),
                                                    earliest = min(.data$year),
                                                    latest = max(.data$year),
                                                    min_value = min(.data$value),
                                                    max_value = max(.data$value),
                                                    lower_iqr = outliers(.data$value)[1],
                                                    upper_iqr = outliers(.data$value)[2])
  message(paste("Your data has:", num_variablenames, "variables"))
  print(tmp)
  return(tmp)
}
