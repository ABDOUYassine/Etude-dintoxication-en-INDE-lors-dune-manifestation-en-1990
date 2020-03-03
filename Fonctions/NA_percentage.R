#' Calculate the percentage of missing data.
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
NA_percentage <- function(df) {
  
  100*(df %>% 
    is.na() %>% 
    sum())/(length(df)*nrow(df))
  
}