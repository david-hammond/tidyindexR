#' Calculates Index
#'
#' This prepares the raw data for use in the index by
#'
#' @param df dataframe in tidyindexR format
#'
#' @return Returns preprocessed data.
#'
#' @examples
#' #NOT RUN BECAUSE NEED USER GENERATED META DATA FILE
#' # data(povstats)
#' # index_create_meta(df)
#' # df <- index_data_preprocess(povstats)
#' # df <- index_calculate(df)
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @keywords summarise
#' @author David Hammond
#' @export
index_calculate = function(df){
  df = df %>%
    dplyr::mutate(banded = (.data$imputed - .data$lower_iqr)/(.data$upper_iqr - .data$lower_iqr)) %>%
    dplyr::mutate(banded = ifelse(.data$banded < 0, 0, .data$banded)) %>%
    dplyr::mutate(banded = ifelse(.data$banded > 1, 1, .data$banded)) %>%
    dplyr::mutate(indicator_score = .data$banded*.data$weight)
  domains = df %>% dplyr::group_by(.data$geocode, .data$year, .data$index_domain) %>%
    dplyr::summarise(value = sum(.data$indicator_score)/sum(.data$weight)) %>%
    dplyr::rename(variablename = .data$index_domain) %>% dplyr::ungroup()
  scores = df %>% dplyr::group_by(.data$geocode, .data$year, .data$index_name) %>%
    dplyr::summarise(value = sum(.data$indicator_score)/sum(.data$weight)) %>%
    dplyr::rename(variablename = .data$index_name) %>% dplyr::ungroup()
  scores = rbind(scores, domains)
  scores = scores %>% dplyr::group_by(.data$variablename, .data$year) %>%
    dplyr::mutate(rank = rank(-.data$value), value = round(.data$value,2)) %>%
    dplyr::ungroup()
  countryinfo = wbstats::wb_cachelist$countries %>% dplyr::rename(geocode = .data$iso3c) %>%
    dplyr::select(.data$geocode, .data$country,.data$region, .data$income)
  df = list(backend = df, scores = scores, countryinfo = countryinfo)
  return(df)
}
