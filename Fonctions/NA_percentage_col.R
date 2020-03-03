#' Calculate the percentage of missing data in each column.
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
NA_percentage_col <- function(vect) {
  100*(vect %>% is.na() %>% sum()) / (vect %>% length())
}
