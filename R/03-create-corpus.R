#' Prepares raw data
#'
#' This prepares the raw data for use in the index by
#'
#' @param df dataframe in tidyindexR format
#' @param index_meta_data_path where the meta data is stored
#'
#' @return Returns preprocessed data.
#'
#' @examples
#' #NOT RUN BECAUSE NEED USER GENERATED META DATA FILE
#' # data(povstats)
#' # df <- index_data_preprocess(povstats)
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @keywords summarise
#' @author David Hammond
#' @export
index_create_data_corpus = function(df){
    tmp = index_data_pad(df)
    #put in a regional average option
    tmp = index_data_knn(tmp)
    tmp = tmp %>% dplyr::left_join(index_data_summarise(tmp))

    tmp = tmp %>% dplyr::group_by(.data$geocode) %>%
      dplyr::mutate(knn_pc = sum(.data$imputation_type == "knn")/dplyr::n()) %>%
      dplyr::ungroup()
    my_ecdf = stats::ecdf(tmp$knn_pc)
    my_ecdf <- data.frame(
      knn_threshold = tmp$knn_pc,
      number_of_countries = floor(length(unique(tmp$geocode))*my_ecdf(tmp$knn_pc))
    )
    my_ecdf  = my_ecdf [order(my_ecdf$knn_threshold),]
    my_ecdf = my_ecdf %>% dplyr::distinct()
    p = plotly::ggplotly(ggplot2::ggplot(my_ecdf , ggplot2::aes(.data$knn_threshold, .data$number_of_countries)) + ggplot2::geom_line() +
                       ggplot2::labs(title = "Number of knn imputations vs number of countries in index", x = "knn Threshold",
                            y = "Number of Countries with less than knn threshold") +
                       ggplot2::theme_minimal())
    print(p)
    return(tmp)
}
