#' Pad and Interpolate
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
#' @author David Hammond
#' @export
index_data_pad = function(df){
  df = df %>% dplyr::mutate(date = as.Date(paste0(.data$year, "-01-01"), format("%Y-%m-%d")))
  df = df %>%
    dplyr::group_by(.data$geocode, .data$variablename) %>% dplyr::arrange(.data$year) %>%
    padr::pad(interval = "year", start_val = min(df$date), end_val = max(df$date)) %>%
    dplyr::mutate(n = sum(!is.na(.data$value))) %>% dplyr::ungroup()
  pos = df$n == 1
  df1 = df[pos,] %>%
    dplyr::group_by(.data$geocode, .data$variablename) %>% dplyr::mutate(imputed = .data$value[!is.na(.data$value)][1]) %>%
    dplyr::ungroup()
  df2 = df[!pos,] %>%
    dplyr::group_by(.data$geocode, .data$variablename) %>% dplyr::mutate(imputed = imputeTS::na_interpolation(.data$value)) %>%
    dplyr::ungroup()
  df = rbind(df1, df2)
  df$year = lubridate::year(df$date)
  df$imputation_type = "Interpolated"
  df$imputation_type[df$value == df$imputed] = "Original Data"
  df = df %>% dplyr::select(.data$geocode, .data$year, .data$variablename, .data$value, .data$imputed, .data$imputation_type)
  return(df)
}

