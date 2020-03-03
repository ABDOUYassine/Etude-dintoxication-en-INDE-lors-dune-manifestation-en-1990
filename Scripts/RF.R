RF <-
  lapply(1:15, function(x)
    mice(
      DF,
      m = 1,
      maxit = x,
      meth = "rf",
      seed = 500
    ))

completedData_mice_rf <-
  lapply(1:15, function(x)
    mice::complete(RF[[x]], 1))

rf <- lapply(1:15, function(x)
  err_prediction(dat.kNN, completedData_mice_rf[[x]]))


erreur <- lapply(1:15, function(x)
  mean(rf[[x]]$MSE))

erreur_df <- erreur %>% as.data.frame() %>% t()
rownames(erreur_df) <- NULL
