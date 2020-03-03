#' Prepares raw data
#'
#' This prepares the raw data for use in the index by
#'
#' @param df dataframe in tidyindexR format
#'
#' @return Returns preprocessed data.
#'
#' @examples
#' data(povstats)
#' df <- index_data_preprocess(povstats)
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @keywords summarise
#' @author David Hammond
#' @export
index_data_preprocess = function(df){
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
