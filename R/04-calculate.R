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
    dplyr::mutate(indicator_score = .data$banded*.data$weight) %>%
    dplyr::group_by(.data$geocode, .data$year, .data$index_domain) %>%
    dplyr::mutate(domain_score = sum(.data$indicator_score)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$geocode, .data$year, .data$index_name) %>%
    dplyr::mutate(index_score = sum(.data$indicator_score))
  return(df)
}
