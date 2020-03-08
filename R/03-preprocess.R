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
index_data_preprocess = function(df, index_meta_data_path = "index_meta_data/index_meta_data.xlsx"){
  if(sum(is.na(index_meta_data))+sum(grepl("User", index_meta_data))>0){
    message(paste("Malformed index_meta_data file at", index_meta_data_path))
    message("Please ensure all columns and cells are filled in")
  }else{
    tmp = index_data_pad(df)
    #put in a regional average option
    tmp = rbind(tmp, index_data_knn(tmp))
    index_meta_data <- readxl::read_excel(index_meta_data_path)
    tmp = tmp %>% dplyr::left_join(index_meta_data)
    tmp$imputed[!as.logical(tmp$is_more_better)] = -tmp$imputed[!as.logical(tmp$is_more_better)]
    #this is a hack but it will do for now
    tmp$value[!as.logical(tmp$is_more_better)] = -tmp$value[!as.logical(tmp$is_more_better)]
    tmp = tmp %>% dplyr::left_join(index_data_summarise(tmp))
    return(tmp)
  }

}
