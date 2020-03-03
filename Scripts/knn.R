
KNN <-
  lapply(1:20, function(x)
    VIM::kNN(DF, k = x, imp_var = FALSE))

knn <- lapply(1:20, function(x)
  Precision(KNN[[x]]))

erreur <- lapply(1:20, function(x)
  mean(knn[[x]]$rf))

erreur_df_knn <- erreur %>% as.data.frame() %>% t()
rownames(erreur_df_knn) <- NULL