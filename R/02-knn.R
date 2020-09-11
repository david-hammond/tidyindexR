#' KNN Interpolate
#'
#' This analyses raw index data and summarises the data
#'
#' @param df dataframe in tidyindexR format
#'
#' @return Returns a summary of the raw data.
#'
#' @examples
#' data(povstats)
#' index_create_meta(povstats)
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @keywords summarise
#' @author David Hammond
#' @export
index_data_knn = function(df){
  x = df %>% dplyr::select(.data$geocode, .data$year, .data$variablename, .data$imputed) %>%
    tidyr::spread(.data$variablename, .data$imputed) %>% as.data.frame()
  preObj <- caret::preProcess(x[, -c(1, 2)], method = "knnImpute", k = 5)
  tmp <- stats::predict(preObj, x[, -c(1, 2)])
  tmp <- sweep(tmp, MARGIN = 2, preObj$std, `*`)
  tmp <- sweep(tmp, MARGIN = 2, preObj$mean, `+`)
  tmp <- cbind(geocode = x$geocode, year = x$year, tmp)
  tmp <- tmp %>% tidyr::gather("variablename", "imputed", -c(.data$geocode, .data$year)) %>%
    dplyr::mutate(geocode = as.character(.data$geocode))
  tmp <- tmp %>% dplyr::anti_join(df %>% select(geocode, year, variablename)) %>% dplyr::mutate(imputation_type = "knn") %>%
    dplyr::mutate(value = NA)
  tmp = rbind(df, tmp[,names(df)])
  return(tmp)
}

