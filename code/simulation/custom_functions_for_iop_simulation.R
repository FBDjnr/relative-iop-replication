# compute_iop(y, C, w) == iop_rel(x, stratum = NULL, cluster = NULL, weight = NULL,  
#                     circumstances, data = NULL, variance = FALSE,
#                     var.decompose = FALSE,
#                     distribution = c("smoothed", "standardized")
#                   )
# T = total_iop, AbsIOP = abs_iop, RelIOP = rel_iop, y_smooth = 

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Jacknife Standard Error for iop_rel
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

jackknife_se_iop2 <- function(y, C, w, strata, clusters) {
  
  est_full <- iop_rel(x = y, circumstances = C, weight = w)$rel_iop
  
  all_cls  <- unique(clusters)
  n_cls    <- length(all_cls)

  # Leave-one-cluster-out estimates
  loo_est2 <- sapply(all_cls, function(cl) {
    keep <- clusters != cl
    if (sum(keep) < 10) return(NA_real_)
    tryCatch(
      iop_rel(x = y[keep], circumstances = C[keep, , drop = FALSE], weight = w[keep])$rel_iop,
      error = function(e) NA_real_
    )
  })

  tibble(g = all_cls) |> 
    mutate(loo_est2 = map(
      g,
      ~ {
        keep <- g != .x
        if (sum(keep) < 10) return(NA_real_)
        tryCatch(
          iop_rel(x = y[keep], circumstances = C[keep, , drop = FALSE], weight = w[keep])$rel_iop,
          error = function(e) NA_real_
        )
      }
    ))
  data.frame(y, C, w, strata, clusters) |> 


  # Stratified jackknife variance
  jk_var <- data.frame(strata, clusters) |> 
    distinct() |> 
    mutate(loo_est2 = loo_est2) |>
    drop_na() |> 
    group_by(strata) |>
    summarise(n_s = count_unique(clusters),
              var_s = var(loo_est2)
              ) |>
    mutate(jk_var_s = (n_s - 1)^2 / n_s * var_s) |>
    ungroup() |> 
    summarise(jk_var = sum(jk_var_s)) |>
    pull(jk_var)

  ans <- sqrt(max(jk_var, 0))
  return(ans)

}


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Bootstrap Standard Error for iop_rel
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

boot_se_iop <- function(y, C, w, strata, clusters, B_boot = 300L) {

  dt1 <- cbind(y, C, w, strata, clusters) |> 
    group_by(strata) |>
    mutate(w_tot_stratum = sum(w)) |>
    ungroup()

  boot_est <- replicate(B_boot, {
  dt1_boot <- dt1 |> 
    distinct(strata, clusters) |> 
    group_by(strata) |>
    sample_n(size = n(), replace = TRUE) |>
    ungroup() |>
    left_join(dt1, by = join_by(strata, clusters), relationship = "many-to-many") |>
    group_by(strata) |>
    mutate(w_boot = w * w_tot_stratum / sum(w)) |> 
    ungroup()
    
    out <- iop_rel(x = y, circumstances = names(C), weight = w_boot, data = dt1_boot)$rel_iop

    out
  })
  
  ans <- sd(boot_est, na.rm = TRUE) 

  return(ans)
  
}
