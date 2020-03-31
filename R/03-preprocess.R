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
index_data_preprocess = function(df, index_meta_data_path = "index_meta_data/index_meta_data.csv"){
  index_meta_data <- readr::read_csv(index_meta_data_path)
  if(sum(is.na(index_meta_data))+sum(grepl("User", index_meta_data))>0){
    message(paste("Malformed index_meta_data file at", index_meta_data_path))
    message("Please ensure all columns and cells are filled in")
  }else{
    tmp = index_data_pad(df)
    #put in a regional average option
    tmp = index_data_knn(tmp)
    tmp = tmp %>% dplyr::left_join(index_meta_data)
    tmp = tmp %>% dplyr::left_join(index_data_summarise(tmp))

    #set banding limits

    tmp$lower_cutoff_band = tmp$min_value
    tmp$upper_cutoff_band = tmp$max_value
    pos = tmp$banding_using_minmax == 0
    tmp$lower_cutoff_band = tmp$lower_iqr
    tmp$upper_cutoff_band = tmp$upper_iqr

    #Switch polarity

    pos = !as.logical(tmp$is_more_better)

    tmp$imputed[pos] = -tmp$imputed[pos]
    tmp$value[pos] = -tmp$value[pos]
    low = tmp$lower_cutoff_band[pos]
    high = tmp$upper_cutoff_band[pos]
    tmp$lower_cutoff_band[pos] = - high
    tmp$upper_cutoff_band[pos] = - low


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

}
