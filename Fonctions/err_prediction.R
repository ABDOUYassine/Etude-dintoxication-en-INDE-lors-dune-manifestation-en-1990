#' Mean Square Error
#'
#' @param LM_fit le modèle
#' @param df la table
#'
#' @return erreur_train
#' @export
#'
#' @examples
err_prediction <- function(Dat_knn_imput,dat_imput_diff_metho) {
  
  
  data.frame(
    Variables = c("age", "eclair"),
    MSE = c(mean((Dat_knn_imput$age - dat_imput_diff_metho$age)^2),
             mean((Dat_knn_imput$eclair - dat_imput_diff_metho$eclair)^2))
  )
  
}

