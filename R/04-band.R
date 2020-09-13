#' Prepares bands data between 0 and 1
#'
#' This returns a 0 to 1 banding based on user inputs of weights and indicator direction
#'
#' @param values values to band
#' @param best_value value to be scaled to one. Everything 'higher' than this will also be given a score of one.
#' @param worst_value value that will be zero. Everything 'lower' than this will also be given a score of zero.
#'
#' @return Returns banded data.
#'
#' @examples
#'
#' @author David Hammond
#' @export
index_data_band = function(values, worst_value = min(values), best_value = max(values)){
  hist(values)
  abline(v = worst_value, col = "red", cex = 2)
  abline(v = best_value, col = "green", cex = 2)
  text(x = best_value, y = 0.1, "BEST", col = "green")
  text(x = worst_value, y = 0.1, "WORST", col = "red")
  banded = (values - worst_value)/(best_value-worst_value)
  banded[banded > 1] = 1
  banded[banded < 0] = 0
  return(banded)
}
