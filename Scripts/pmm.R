PMM <-
  lapply(1:15, function(x)
    mice(
      DF,
      m = 1,
      maxit = x,
      meth = "pmm",
      seed = 500
    ))

completedData_mice_pmm <-
  lapply(1:15, function(x)
    mice::complete(PMM[[x]], 1))

pmm <- lapply(1:15, function(x)
  err_prediction(dat.kNN,completedData_mice_pmm[[x]]))

erreur <- lapply(1:15, function(x)
  mean(pmm[[x]]$MSE))

erreur_df_pmm <- erreur %>% as.data.frame() %>% t()
rownames(erreur_df_pmm) <- NULL
