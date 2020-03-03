#' Precision of each method
#'
#' @param BDD
#'
#' @return
#' @export
#'
#' @examples
Precision <- function(BDD, BDD_knn_imputee, BDD_meth_impute, data_gener_with_NA) {
  
  g1 <- BDD[, colSums(!is.na(BDD)) != nrow(BDD)] %>%
    dplyr::select(-c("age", "eclair")) %>%
    colnames() # nom des colonne contenant des NA
  
  
  g2 <- BDD_knn_imputee %>%
    dplyr::select(g1) # Table  imputee avant de generer les NA ,
  # avec seulement les variables contenant des NA
  
  g3 <- BDD_meth_impute %>%
    dplyr::select(g1)
  
  g4 <- (g3 %>%
           nrow()) - (data_gener_with_NA %>%
                        dplyr::select(-c("age", "eclair")) %>%
                        is.na() %>%
                        colSums())
  
  prc <- lapply(1:length(g1), 
                function(x) {(g2[, x] == g3[, x]) %>% sum()}
                ) %>% 
    unlist()
  
  Data_per_impute <-
    data.frame(Variables = g1,
               rf = ((prc - g4) / (data_gener_with_NA %>%
                                           dplyr::select(-c("age", "eclair")) %>%
                                           is.na() %>%
                                           colSums()
                                         ) 
                     )
               )
  
  return(Data_per_impute)
  
}
