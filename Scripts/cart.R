CART <-
  lapply(1:15, function(x)
    mice(
      DF,
      m = 1,
      maxit = x,
      meth = "cart",
      seed = 500
    ))

completedData_mice_cart <-
  lapply(1:15, function(x)
    mice::complete(CART[[x]], 1))

cart <- lapply(1:15, function(x)
  err_prediction(dat.kNN,completedData_mice_cart[[x]]))

erreur <- lapply(1:15, function(x)
  mean(cart[[x]]$MSE))

erreur_df_cart <- erreur %>% as.data.frame() %>% t()
rownames(erreur_df_cart) <- NULL
