#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Count Unique values function ####
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
count_unique <- function(x) {
  length(unique(x))
}

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Create column numbers for latex output of tables ####
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
colnumbering <- function(n) {
  x <- paste0("\\multicolumn{1}{c}{(", 1:n, ")}")
  x <- paste(x, collapse = " & ")
  x <- paste(x, "\\\\")
  return(x)
}

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Compute Delta Method variance of ratio (num_est/denom_est) ####
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
compute_delta_variance <- function(num_est, denom_est, num_var, denom_var, cov_both) {
  
  # num_est: estimate of the numerator
  # denom_est: estimate of the denominator
  # num_var: variance of the numerator
  # denom_var: variance of the denominator
  # cov_both: covariance of the numerator and denominator
  
  if (denom_est == 0) {
    stop("Mean of denominator (denom_est) cannot be zero.")
  }
  
  var_ratio <- (num_est/denom_est)^2 * (num_var/num_est^2 + denom_var/denom_est^2 - 2 * cov_both/(num_est*denom_est))
  return(var_ratio)
}

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Compute weighted ....... ####
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

my_revcumsum <- function(x, weight = NULL){
  
  if(is.null(weight)) {
    weight <- rep(1, length(x))
  }
  
  dt <- data.frame(x, weight)
  
  dt_Sx <- dt |> 
    group_by(x) |>
    summarise(w= sum(weight), .groups = "drop") |>
    arrange(desc(x)) |>
    mutate(Sx = cumsum(w))
  
  dt_full <- dt %>% 
    left_join(dt_Sx, by = "x")
  
  return(dt_full$Sx)
}

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Bootstrap Sampling for Complex Survey ####
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
bootstrap_sample <- function(stratum, cluster, data) {
  
  # sample clusters (with replacement) within each stratum
  boot_sel <- data |> 
    dplyr::select({{stratum}}, {{cluster}}) |> 
    dplyr::group_by({{stratum}}) |>
    dplyr::mutate(Hs = length(unique({{cluster}}))) |>  # number of clusters for each stratum
    dplyr::group_by({{stratum}}, Hs)  |>
    tidyr::nest() |> 
    dplyr::ungroup() |>
    dplyr::mutate(samp = purrr::map2(data, Hs, ~dplyr::slice_sample(.x, n = .y, replace = TRUE))) |>
    dplyr::select(-data, -Hs) |>
    tidyr::unnest(samp)
  
  # extract selected clusters from original data set
  boot_sample <- boot_sel |> 
    dplyr::left_join(data, 
                     by = dplyr::join_by({{stratum}}, {{cluster}}), 
                     relationship = "many-to-many")
  
  return(boot_sample)
}


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Data Preparation: Numbering Strata, clusters and hh_ids Appropriately ####
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Attach Cluster size, No of Hseholds, and HoseHold Sizes to Data##

prep_data <- function(x, stratum = NULL, cluster = NULL, hh_id = NULL,
                      hh_size = NULL, circumstances = NULL,
                      weight = NULL,  data = NULL){
  
  # x: observations
  # stratum: corresponding stratum IDs for x
  # cluster: corresponding cluster IDs for x
  # hh_id: corresponding household IDs for x
  # hh_size: number of household members
  # circumstances:
  # weight: corresponding weights for x
  # data: name of dataset
  
  if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
  
  # Extracting columns from the dataset
  var.name <- deparse(substitute(x))
  if (grepl("\\$", var.name)) {
    var.name <- strsplit(var.name, "\\$")[[1]][2]
    }
  
  if(!is.null(data)){
    x <- eval(substitute(x), data, parent.frame())
    stratum <- eval(substitute(stratum), data, parent.frame())
    cluster <- eval(substitute(cluster), data, parent.frame())
    hh_id <- eval(substitute(hh_id), data, parent.frame())
    hh_size <- eval(substitute(hh_size), data, parent.frame())
    circumstances <- eval(substitute(circumstances), data, parent.frame())
    weight <- eval(substitute(weight), data, parent.frame())
  }
  
  
  # Filling in missing columns
  n <- length(x)
  ones <- rep(1, n)
  if (is.null(stratum)) {stratum <- ones}
  if (is.null(cluster)) {cluster <- ones}
  if (is.null(hh_id)) {hh_id <- 1:n}
  if (is.null(hh_size)) {hh_size <- ones}
  if (is.null(weight)) {weight <- hh_size}
  
  dta <- data.frame(stratum, cluster, hh_id, hh_size, circumstances, weight, x)
  
  # combine duplicate households
  dta <- dta %>%
    dplyr::group_by(stratum, cluster, hh_id) %>%
    dplyr::summarise(x = sum(x, na.rm = TRUE),
                     hh_size = mean(hh_size, na.rm = TRUE),
                     circumstances = min(circumstances, na.rm = TRUE),
                     weight = mean(weight, na.rm = TRUE),
                     .groups = "keep") %>%
    dplyr::ungroup()
  
  # number of clusters per strata (H_s)
  dta <- dta %>%
    dplyr::group_by(stratum) %>%
    dplyr::mutate(H_s = count.unique(cluster)) %>%
    dplyr::ungroup()
  
  # number of households per cluster per stratum (M_scs)
  dta <- dta %>%
    dplyr::group_by(stratum, cluster) %>%
    dplyr::mutate(M_scs = count.unique(hh_id)) %>%
    dplyr::ungroup()
  
  # Final data
  dta <- dta %>%
    dplyr::select(stratum, cluster, hh_id, hh_size, circumstances, weight, x,
                  H_s, M_scs) %>%
    dplyr::arrange(stratum, cluster, hh_id, circumstances) %>%
    dplyr::rename_with(~ gsub("^x$", var.name, .x))
  
  return(dta)
}


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Gini index and variance for Complex Survey ####
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

## Gini Index ###
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
gini_ineq <- function(x, weight = NULL){
  
  if(is.null(weight)) {
    
    # No weights
    x <- sort(x)
    n <- length(x)
    i <- seq.int(1, n)
    
    g_num <- 2*sum(i*x)/sum(x) - (n + 1L)
    g <- g_num/n
    
  }else{
    
    # # using Bhattacharya (2007)
    # w <- weight/sum(weight) # normalize the weight
    # mu <- sum(x*w) # calculating the weighted mean
    # 
    # w <- w[order(x)]
    # Fx <- cumsum(w) # CDF
    # x <- sort(x)
    # # Fx <- {ggdist::weighted_ecdf(x, w)}(x)
    # g <- 2/mu * sum(w*x*Fx) - 1 # Gini index
    
    g <- reldist::gini(x = x, weights = weight)
  }
  
  return(g)
}

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Variance of Gini Index ####

gini_var <- function(x, stratum = NULL, cluster = NULL, weight = NULL, data = NULL){
  
  if(!is.null(data)){
    
    if(is.symbol(substitute(x))){
      x <- eval(substitute(x), data, parent.frame())
    }else if(!is.null(x)){
      x <- data[[x]]
    }
    if(is.symbol(substitute(weight))){
      weight <- eval(substitute(weight), data, parent.frame())
    }else if(!is.null(weight)){
      weight <- data[[weight]]
    }
    if(is.symbol(substitute(stratum))){
      stratum <- eval(substitute(stratum), data, parent.frame())
    }else if(!is.null(stratum)){
      stratum <- data[[stratum]]
    }
    if(is.symbol(substitute(cluster))){
      cluster <- eval(substitute(cluster), data, parent.frame())
    }else if(!is.null(cluster)){
      cluster <- data[[cluster]]
    }
    
  }
  
  n <- length(x)
  ones <- rep(1, n)
  if (is.null(stratum)){stratum <- ones}
  if (is.null(cluster)){cluster <- ones}
  if (is.null(weight)){weight <- ones}
  
  # Gini index
  gini_hat <- gini_ineq(x, weight)
  
  # normalizing the weight
  w <- weight/sum(weight) 
  
  # weighted mean
  mu <- sum(w*x)
  
  dt <- data.frame(stratum, cluster, x, w)|>
    # Empirical CDF
    dplyr::mutate(Fx = {ggdist::weighted_ecdf(x, w)}(x)) |> 
    # dplyr::arrange(desc(Fx)) |>
    # dplyr::mutate(B = cumsum(w*x)) |>
    dplyr::mutate(B = my_revcumsum(x, w*x)) |>
    # influence function (psi)
    dplyr::mutate(u_sch = w*(2/mu) * (x*Fx + B - 0.5*(mu + x)*(gini_hat + 1)))
  
  gini_variance <- dt |>
    dplyr::group_by(stratum, cluster) |>
    dplyr::summarise(u_sc = sum(u_sch), .groups = "drop") |>  # compute u_sc
    dplyr::group_by(stratum) |>
    dplyr::summarise(n_s = dplyr::n(),
                     # nvar = dplyr::n() * var(u_sc),
                     nvar = sum((u_sc - mean(u_sc))^2),
                     .groups = "drop") |> # compute n*group_variance
    dplyr::summarise(est = sum(nvar)) |> # estimate of the variance
    dplyr::pull()
  
  ans <- list(est = gini_hat, var = gini_variance)
  return(ans)
}

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Variance of Gini Index (decomposed using Bhattacharya) ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

gini_var_bhatt <- function(x, stratum = NULL, cluster = NULL, weight = NULL, data){
  
  if(!is.null(data)){
    
    if(is.symbol(substitute(x))){
      x <- eval(substitute(x), data, parent.frame())
    }else if(!is.null(x)){
      x <- data[[x]]
    }
    if(is.symbol(substitute(weight))){
      weight <- eval(substitute(weight), data, parent.frame())
    }else if(!is.null(weight)){
      weight <- data[[weight]]
    }
    if(is.symbol(substitute(stratum))){
      stratum <- eval(substitute(stratum), data, parent.frame())
    }else if(!is.null(stratum)){
      stratum <- data[[stratum]]
    }
    if(is.symbol(substitute(cluster))){
      cluster <- eval(substitute(cluster), data, parent.frame())
    }else if(!is.null(cluster)){
      cluster <- data[[cluster]]
    }
    
  }
  
  n <- length(x)
  ones <- rep(1, n)
  if (is.null(stratum)){stratum <- ones}
  if (is.null(cluster)){cluster <- ones}
  if (is.null(weight)){weight <- ones}
  
  # normalizing the weight
  w <- weight/sum(weight) 
  
  # Estimate of Gini index
  gini_hat <- gini_ineq(x, weight)
  
  # Weighted mean
  mu <- sum(x*w)
  
  # Combine survey data with structure
  dt <- data.frame(x, stratum, cluster, w)
  
  dt1 <- dt |>
    dplyr::arrange(stratum, cluster, x) |>
    dplyr::mutate(Fx = {ggdist::weighted_ecdf(x, w)}(x), 
                  q11 = Fx - 0.5*(gini_hat + 1)#, 
                  # q12 = my_revcumsum(x, w*x)
                  ) |> 
    dplyr::arrange(desc(Fx)) |>
    dplyr::mutate(q12 = cumsum(w*x)) |>
    dplyr::mutate(mtemp = (2/mu) * (q11 * x + q12 - (0.5 * mu * (gini_hat + 1)))) |> 
    dplyr::mutate(ztemp = w * mtemp) # psi
  
  
# naive variance
  dt_naive <- dt1 |> 
    summarise(naive = sum(ztemp^2)) 
  
  # cluster effect
  dt_cluster <- dt1 |> 
    group_by(stratum, cluster) |> 
    summarise(t1 = sum(ztemp)^2 - sum(ztemp^2), .groups = "drop") |> 
    ungroup() |> 
    summarise(cluster = sum(t1))
  
  # stratum effect
  dt_SUMWS <-  dt1 |> 
    group_by(stratum) |> 
    summarise(n_s = count_unique(cluster),
              sums = sum(ztemp),
              .groups = "drop") |> 
    ungroup() |> 
    summarise(sumws = sum(sums^2/n_s))
  
  s1 <- dt_naive$naive
  s2 <- dt_cluster$cluster
  s3 <- -dt_SUMWS$sumws
  
  gini_variance <- s1 + s2 + s3
  
  ans <- list(est = gini_hat, 
              var = gini_variance, 
              var.decompose = c(naive = s1, cluster = s2, stratum = s3))
  
  return(ans)
}

# Gini Bootstrap ####
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::.

gini_bstr <- function(x, stratum = NULL, cluster = NULL, weight = NULL,
                      data = NULL, nboot = 1000, 
                      parallel = FALSE, no_cores = NULL){
  
  # x: observations
  # stratum: corresponding stratum IDs for x
  # cluster: corresponding cluster IDs for x
  # weight: corresponding weights for x
  # data: name of data set
  # nboot: The number of bootstrap samples desired.
  
  # Extracting columns from the data set
  if(!is.null(data)){
    
    if(is.symbol(substitute(x))){
      x <- eval(substitute(x), data, parent.frame())
    }else if(!is.null(x)){
      x <- data[[x]]
    }
    if(is.symbol(substitute(weight))){
      weight <- eval(substitute(weight), data, parent.frame())
    }else if(!is.null(weight)){
      weight <- data[[weight]]
    }
    if(is.symbol(substitute(stratum))){
      stratum <- eval(substitute(stratum), data, parent.frame())
    }else if(!is.null(stratum)){
      stratum <- data[[stratum]]
    }
    if(is.symbol(substitute(cluster))){
      cluster <- eval(substitute(cluster), data, parent.frame())
    }else if(!is.null(cluster)){
      cluster <- data[[cluster]]
    }
    
  }
  
  n <- length(x) # number of observations
  ones <- rep(1, n)
  if (is.null(stratum)){stratum <- ones}
  if (is.null(cluster)){cluster <- ones}
  if (is.null(weight)){weight <- ones}
  
  # Find missing values from the data
  na_indices <- (is.na(x) | is.na(stratum) | is.na(cluster) | is.na(weight))
  
  # Remove observations that have missing values
  x <- x[!na_indices]
  stratum <- stratum[!na_indices]
  cluster <- cluster[!na_indices]
  weight <- weight[!na_indices]
  
  # Create unique IDs for the clusters
  orig_data <- data.frame(stratum, cluster, x, weight)
  
  # original gini index
  theta <- gini_ineq(orig_data$x, orig_data$weight)
  
  if(parallel){
    # Parallel
    all_cores <- future::availableCores()
    if(is.null(no_cores)){
      no_cores <-all_cores
    }
    use_cores <- min(no_cores, max(all_cores - 2, 1))
    cl <- parallel::makeCluster(use_cores)
    # parallel::clusterExport(cl, c(lsf.str()))
    doParallel::registerDoParallel(cl)
  }else{
    # Sequential
    foreach::registerDoSEQ()
  }
  
  thetastar <- foreach::foreach(i = 1:nboot, .combine = "c", 
                                .export =c("gini_ineq", "bootstrap_sample")) %dopar%{
                                  
                                  boot_sample <- bootstrap_sample(stratum, cluster, orig_data)
                                  g <- gini_ineq(boot_sample$x, boot_sample$weight)
                                  
                                  return(g)
                                }
  
  if(parallel){
    # End and release all clusters
    parallel::stopCluster(cl)
  }
  
  theta.var <- sum((thetastar - theta)^2)/nboot
  
  ans <- list(est = theta, var = theta.var, thetastar = thetastar)
  
  return(ans)
  
}


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Gini Index and its Variance ####
gini_index <- function(x, stratum = NULL, cluster = NULL, weight = NULL,
                 data = NULL, variance = TRUE, var.decompose = FALSE,...) {

  # x: observations
  # stratum: corresponding stratum IDs for x
  # cluster: corresponding cluster IDs for x
  # weight: corresponding weights for x
  # data: name of data set
  # variance: if TRUE, the variance will be computed

  # Extracting columns from the data set
  if (!is.null(data)) {
    
    if(is.symbol(substitute(x))){
      x <- eval(substitute(x), data, parent.frame())
    }else if(!is.null(x)){
      x <- data[[x]]
    }
    if(is.symbol(substitute(stratum))){
      stratum <- eval(substitute(stratum), data, parent.frame())
    }else if(!is.null(stratum)){
      stratum <- data[[stratum]]
    }
    if(is.symbol(substitute(cluster))){
      cluster <- eval(substitute(cluster), data, parent.frame())
    }else if(!is.null(cluster)){
      cluster <- data[[cluster]]
    }
    if(is.symbol(substitute(weight))){
      weight <- eval(substitute(weight), data, parent.frame())
    }else if(!is.null(weight)){
      weight <- data[[weight]]
    }
  }

  n <- length(x) # number of observations
  ones <- rep(1, n)
  if (is.null(stratum)){stratum <- ones}
  if (is.null(cluster)){cluster <- ones}
  if (is.null(weight)){weight <- ones}
  
  # Find missing values from the data
  na_indices <- (is.na(x) | is.na(stratum) | is.na(cluster) | is.na(weight))
  
  # Remove observations that have missing values
  x <- x[!na_indices]
  stratum <- stratum[!na_indices]
  cluster <- cluster[!na_indices]
  weight <- weight[!na_indices]
  
  # ox <- order(x) # rank of x
  # x_g <- sort(x) # sorted observations
  # w <- weight/sum(weight) # normalizing the weight
  # w_g <- w[ox] # weights corresponding to order x
  # wx_g <- w_g*x_g # weighted observations
  # 
  # Fx_g <- cumsum(w_g) # Empirical CDF
  # 
  # # Weighted mean
  # mu <- sum(wx_g)
  #  
  # # Estimate of Gini index
  # gini_hat <- 2/mu * sum(wx_g*Fx_g) - 1

  # Computation of variance if needed
  if (!variance) {
    
    ans <- gini_ineq(x, weight)
    
  }else if(!var.decompose){

    ans <- gini_var(x, stratum, cluster, weight, data = NULL)
  
  }else{
   
    ans <- gini_var_bhatt(x, stratum, cluster, weight, data = NULL) 
    
  }

  return(ans)
}

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Variance of Relative Inequality of Opportunity ####
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

iop_var <-  function(x, y, stratum = NULL, cluster = NULL, weight = NULL,
                     distribution = c("smoothed", "standardized")){
  
  # x: observations
  # y: standardized or smoothed version of x due to circumstances
  # stratum: corresponding stratum IDs for x
  # cluster: corresponding cluster IDs for x
  # weight: corresponding weights for x
  # distribution: method to be used for the parametric estimate of the distribution
  
  
  distribution <- match.arg(distribution)
  
  # Gini index
  gini_x <- gini_ineq(x, weight)
  gini_y <- gini_ineq(y, weight)
  
  # normalizing the weight
  w <- weight/sum(weight)
  
  # weighted mean
  mu_x <- sum(w*x)
  mu_y <- sum(w*y)
  
  # Compute values of influence functions
  # dt <- data.frame(stratum, cluster, w, x, y) |>
  #   dplyr::mutate(Fx = {ggdist::weighted_ecdf(x, w)}(x),
  #                 Fy = {ggdist::weighted_ecdf(y, w)}(y),
  #                 Bx = my_revcumsum(x, w*x),
  #                 By = my_revcumsum(y, w*y)
  #   ) |>
  #   dplyr::mutate(u_sch_x = w*(2/mu_x) * (x*Fx + Bx - 0.5*(mu_x + x)*(gini_x + 1)),
  #                 u_sch_y = w*(2/mu_y) * (y*Fy + By - 0.5*(mu_y + y)*(gini_y + 1))
  #   ) 
  
  dt <- data.frame(stratum, cluster, x, w, y) |>
    # Empirical CDF
    dplyr::arrange(stratum, cluster, x) |>
    dplyr::mutate(Fx = {ggdist::weighted_ecdf(x, w)}(x),
                  Fy = {ggdist::weighted_ecdf(y, w)}(y)) |> 
    dplyr::arrange(desc(Fx)) |>
    # influence function (psi)
    dplyr::mutate(u_sch_x = w*(2/mu_x) * (x*Fx + cumsum(w*x) - 0.5*(mu_x + x)*(gini_x + 1))) |>
    dplyr::arrange(stratum, cluster, y) |>
    dplyr::arrange(desc(Fy)) |>
    # influence function (psi)
    dplyr::mutate(u_sch_y = w*(2/mu_y) * (y*Fy + cumsum(w*y) - 0.5*(mu_y + y)*(gini_y + 1)))
    
  
  # Compute variances and covariance
  varcov <- dt |>
    dplyr::group_by(stratum, cluster) |>
    dplyr::summarise(u_sc_x = sum(u_sch_x),
                     u_sc_y = sum(u_sch_y), .groups = "drop") |>  # compute u_sc
    dplyr::group_by(stratum) |>
    dplyr::summarise(n_s = dplyr::n(),
                     # nvar = dplyr::n() * var(u_sc),
                     nvar_x = sum((u_sc_x - mean(u_sc_x))^2),
                     nvar_y = sum((u_sc_y - mean(u_sc_y))^2),
                     nvar_xy = sum((u_sc_x - mean(u_sc_x))*(u_sc_y - mean(u_sc_y))),
                     .groups = "drop") |> # compute n*group_variance
    dplyr::summarise(est_x = sum(nvar_x),
                     est_y = sum(nvar_y),
                     est_xy = sum(nvar_xy))  # estimate of the variance-covariance
  
  # Total IOP
  total_iop <- gini_x
  
  # Absolute IOP due to circumstance(s)
  if(distribution == "standardized"){
    abs_iop <- total_iop - gini_y
  }else{
    abs_iop <- gini_y
  }
  
  # Relative IOP due to circumstance(s)
  rel_iop <- abs_iop/total_iop
  
  # Compute variance using delta method
  var_ratio <- compute_delta_variance(gini_y, gini_x, 
                                      varcov$est_y, varcov$est_x,
                                      varcov$est_xy)
  
  ans <- data.frame(est = c(total_iop, abs_iop, rel_iop),
                    var = c(varcov$est_x, varcov$est_y , var_ratio))
  
  rownames(ans) = c("total_iop", "abs_iop", "rel_iop")
  
  return(ans)
}

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Variance of Relative Inequality of Opportunity (decomposed using Bhattacharya) ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

iop_var_decompose <-  function(x, y, stratum = NULL, cluster = NULL, 
                               weight = NULL, 
                               distribution = c("smoothed", "standardized")){
  
  # x: observations
  # y: standardized or smoothed version of x due to circumstances
  # stratum: corresponding stratum IDs for x
  # cluster: corresponding cluster IDs for x
  # weight: corresponding weights for x
  # distribution: method to be used for the parametric estimate of the distribution
  
  
  distribution <- match.arg(distribution)
  
  # Gini index
  gini_x <- gini_ineq(x, weight)
  gini_y <- gini_ineq(y, weight)
  
  # normalizing the weight
  w <- weight/sum(weight)
  
  # weighted mean
  mu_x <- sum(w*x)
  mu_y <- sum(w*y)
  
  # dt <- data.frame(stratum, cluster, x, w, y) |>
  #   dplyr::mutate(Fx = {ggdist::weighted_ecdf(x, w)}(x), 
  #                 Fy = {ggdist::weighted_ecdf(y, w)}(y) 
  #   ) |> 
  #   dplyr::mutate(q11x = Fx - 0.5*(gini_x + 1),
  #                 q11y = Fy - 0.5*(gini_y + 1),
  #                 q12x = my_revcumsum(x, w*x),
  #                 q12y = my_revcumsum(y, w*y)
  #   ) |> 
  #   dplyr::mutate(mtempx = (2/mu_x) * (q11x * x + q12x - (0.5 * mu_x * (gini_x + 1))),
  #                 mtempy = (2/mu_y) * (q11y * y + q12y - (0.5 * mu_y * (gini_y + 1)))
  #   ) |>
  #   dplyr::mutate(ztempx = w * mtempx,
  #                 ztempy = w * mtempy)
  
  dt <- data.frame(stratum, cluster, x, w, y) |>
    # Empirical CDF
    dplyr::arrange(stratum, cluster, x) |>
    dplyr::mutate(Fx = {ggdist::weighted_ecdf(x, w)}(x), 
                  q11x = Fx - 0.5*(gini_x + 1)) |> 
    dplyr::mutate(Fy = {ggdist::weighted_ecdf(y, w)}(y), 
                  q11y = Fy - 0.5*(gini_y + 1)) |> 
    dplyr::arrange(desc(Fx)) |>
    dplyr::mutate(q12x = cumsum(w*x))|>
    dplyr::arrange(stratum, cluster, y) |>
    dplyr::arrange(desc(Fy)) |>
    dplyr::mutate(q12y = cumsum(w*y))|>
    dplyr::mutate(mtempx = (2/mu_x) * (q11x * x + q12x - (0.5 * mu_x * (gini_x + 1)))) |>
    dplyr::mutate(mtempy = (2/mu_y) * (q11y * y + q12y - (0.5 * mu_y * (gini_y + 1)))) |> 
    dplyr::mutate(ztempx = w * mtempx,
                  ztempy = w * mtempy)
  
  # naive variance
  dt_naive <- dt |> 
    dplyr::summarise(naive_x = sum(ztempx^2), 
                     naive_y = sum(ztempy^2),
                     naive_xy = sum(ztempx * ztempy)) 
  
  # cluster effect
  dt_cluster <- dt |> 
    dplyr::group_by(stratum, cluster) |> 
    dplyr::summarise(t1x = sum(ztempx)^2 - sum(ztempx^2),
                     t1y = sum(ztempy)^2 - sum(ztempy^2),
                     t1xy = sum(ztempx)*sum(ztempy) - sum(ztempx*ztempy), 
                     .groups = "drop") |> 
    dplyr::ungroup() |> 
    dplyr::summarise(cluster_x = sum(t1x),
                     cluster_y = sum(t1y),
                     cluster_xy = sum(t1xy),
                     .groups = "drop")
  
  # stratum effect
  dt_SUMWS <-  dt |> 
    dplyr::group_by(stratum) |> 
    dplyr::summarise(n_s = count_unique(cluster),
                     sumsx = sum(ztempx),
                     sumsy = sum(ztempy),
                     .groups = "drop") |> 
    dplyr::ungroup() |> 
    dplyr:: summarise(sumws_x = sum(sumsx^2/n_s),
                      sumws_y = sum(sumsy^2/n_s),
                      sumws_xy = sum(sumsx*sumsy/n_s),
                      .groups = "drop")
  
  varcov <- dt_naive + dt_cluster - dt_SUMWS
  colnames(varcov) <- c("est_x", "est_y", "est_xy")
  
  # Total IOP
  total_iop <- gini_x
  
  # Absolute IOP due to circumstance(s)
  if(distribution == "standardized"){
    abs_iop <- total_iop - gini_y
  }else{
    abs_iop <- gini_y
  }
  
  # Relative IOP due to circumstance(s)
  rel_iop <- abs_iop/total_iop
  
  # compute variance of relative IOP using delta method
  var_ratio <- compute_delta_variance(gini_y, gini_x, 
                                      varcov$est_y, varcov$est_x,
                                      varcov$est_xy) 
  
  # decompose variance of relative IOP
  var_ratio_naive <- compute_delta_variance(gini_y, gini_x, 
                                            dt_naive$naive_y, dt_naive$naive_x,
                                            dt_naive$naive_xy)
  var_ratio_stratum <- compute_delta_variance(gini_y, gini_x, 
                                              -dt_SUMWS$sumws_y, -dt_SUMWS$sumws_x,
                                              -dt_SUMWS$sumws_xy)
  var_ratio_cluster <- compute_delta_variance(gini_y, gini_x, 
                                              dt_cluster$cluster_y, dt_cluster$cluster_x,
                                              dt_cluster$cluster_xy)
 
  
  ans <- data.frame(est = c(total_iop, abs_iop, rel_iop),
                    var = c(varcov$est_x, varcov$est_y, var_ratio),
                    var.naive = c(dt_naive$naive_x, dt_naive$naive_y, var_ratio_naive),
                    var.stratum = c(-dt_SUMWS$sumws_x, -dt_SUMWS$sumws_y, var_ratio_stratum), 
                    var.cluster = c(dt_cluster$cluster_x, dt_cluster$cluster_y, var_ratio_cluster)
  )
  
  rownames(ans) = c("total_iop", "abs_iop", "rel_iop")
  
  return(ans)
  
}

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Relative Inequality of Opportunity ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
iop_rel <- function(x, stratum = NULL, cluster = NULL, weight = NULL,  
                    circumstances, data = NULL, variance = FALSE,
                    var.decompose = FALSE,
                    distribution = c("smoothed", "standardized")
                  ){
  
  # x: observations
  # stratum: corresponding stratum IDs for x
  # cluster: corresponding cluster IDs for x
  # weight: corresponding weights for x
  # circumstances: a vector 
  # data: name of data set
  # variance: if TRUE, the variance will be computed
  # distribution: method to be used for the parametric estimate of the distribution


  distribution <- match.arg(distribution)

  if(!is.null(data)){

    # Extract required columns from the data
    if(is.symbol(substitute(x))){
      x <- eval(substitute(x), data, parent.frame())
    }else if(!is.null(x)){
      x <- data[[x]]
    }
    if(is.symbol(substitute(stratum))){
      stratum <- eval(substitute(stratum), data, parent.frame())
    }else if(!is.null(stratum)){
      stratum <- data[[stratum]]
    }
    if(is.symbol(substitute(cluster))){
      cluster <- eval(substitute(cluster), data, parent.frame())
    }else if(!is.null(cluster)){
      cluster <- data[[cluster]]
    }
    if(is.symbol(substitute(weight))){
      weight <- eval(substitute(weight), data, parent.frame())
    }else if(!is.null(weight)){
      weight <- data[[weight]]
    }
    
    if(is.null(circumstances)){
      stop("circumstances has to be specified as a vector of column names")
    }

  }else{

    if(is.null(circumstances)){
      stop("circumstances has to be specified as a vector of the same length as x 
           or a dataframe with the same number of rows as the length of x")
    }

  }

  n <- length(x)
  ones <- rep(1, n)
  if (is.null(stratum)){stratum <- ones}
  if (is.null(cluster)){cluster <- ones}
  if (is.null(weight)){weight <- ones}

  # Convert circumstances data into a data frame
  if(is.vector(circumstances) | is.factor(circumstances)){
    if(length(x) == length(circumstances)){
      circumstances <- as.data.frame(circumstances)
    }else{
      circumstances <- dplyr::select(data, all_of(circumstances))
    }
  }else{
    circumstances <- as.data.frame(circumstances)
  }


  # find missing values from the data
  na_indices <- (is.na(x) | is.na(stratum) | is.na(cluster) | is.na(weight) |
                        apply(is.na(circumstances), 1, any))

  # Remove observations that have missing values
  x <- x[!na_indices]
  stratum <- stratum[!na_indices]
  cluster <- cluster[!na_indices]
  weight <- weight[!na_indices]
  circumstances <- subset(circumstances, !na_indices)
  
  # drop any circumstance that is a constant
  circumstances <- circumstances %>% 
    janitor::remove_constant(na.rm = TRUE)
  
  # Log-linear relationship between the outcome and the circumstance variables
  lm_dt <- as.data.frame(cbind(x, circumstances))
  lm_mod <- lm(log(x) ~ ., lm_dt)

  # Estimating the hypothesized distribution that eliminates any differences
  # in individual circumstances
  
  if(distribution == "standardized"){
    
    e_hat <- lm_mod$residuals
    
    # Find the mean of circumstances(s)
    circumstances_mean <- circumstances %>%
      summarise(
        # For factors, find the value with the highest proportion
        across(where(is.factor), \(x) names(sort(table(x), decreasing = TRUE))[1]),
        # For numeric values, find the mean
        across(where(is.numeric), \(x) mean(x, na.rm = TRUE))
      ) %>%
      data.frame()
    
    # hypothesized standardized distribution (estimated fair outcome)
    y <- exp(predict(lm_mod, newdata = circumstances_mean) + e_hat)
    
  }else{
    
    # hypothesized smoothed distribution (estimated fair outcome)
    y <- exp(lm_mod$fitted.values)
    
  }


  # Relative IOP
  if(!variance) {
    
    # Total IOP
    total_iop <- gini_ineq(x, weight)
    
    # Absolute IOP due to circumstance(s)
    if(distribution == "standardized"){
      abs_iop <- total_iop - gini_ineq(y, weight)
    }else{
      abs_iop <- gini_ineq(y, weight)
    }
    
    rel_iop <- abs_iop/total_iop
    ans <- list(total_iop = total_iop, abs_iop = abs_iop, rel_iop = rel_iop)
    
  }else{
    
    if(!var.decompose){
      ans <- iop_var(x, y, stratum, cluster, weight, distribution)
    }else{
      ans <- iop_var_decompose(x, y, stratum, cluster, weight, distribution)
    }
    
  }

  return(ans)

}


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Estimate of Inequality of Opportunity (internal function) ####
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

iop_rel_est <- function(x, weight, circumstances,
                        distribution = c("smoothed", "standardized")
){
  
  # x: observations
  # stratum: corresponding stratum IDs for x
  # cluster: corresponding cluster IDs for x
  # weight: corresponding weights for x
  # circumstances: a vector 
  # data: name of data set
  # variance: if TRUE, the variance will be computed
  # distribution: method to be used for the parametric estimate of the distribution
  
  distribution <- match.arg(distribution)
  circumstances <- as.data.frame(circumstances)
  
  # Total IOP
  total_iop <- gini_ineq(x, weight = weight)
  
  # Log-linear relationship between the outcome and the circumstance variables
  lm_dt <- as.data.frame(cbind(x, circumstances))
  lm_mod <- lm(log(x) ~ ., lm_dt)
  
  # Estimating the hypothesized distribution that eliminates any differences
  # in individual circumstances
  
  if(distribution == "standardized"){
    
    e_hat <- lm_mod$residuals
    
    # Find the mean of circumstances(s)
    circumstances_mean <- circumstances |>
      summarise(
        # For factors, find the value with the highest proportion
        across(where(is.factor), \(x) names(sort(table(x), decreasing = TRUE))[1]),
        # For numeric values, find the mean
        across(where(is.numeric), \(x) mean(x, na.rm = TRUE))
      ) |>
      data.frame()
    
    # hypothesized standardized distribution (estimated fair outcome)
    x_hat <- exp(predict(lm_mod, newdata = circumstances_mean) + e_hat)
    
    # associated IOP with fair outcome
    iop_xhat <- gini_ineq(x_hat, weight = weight)
    
    # relative IOP
    abs_iop <- total_iop - iop_xhat
    
  }else{
    
    # hypothesized smoothed distribution (estimated fair outcome)
    mu_hat <- exp(lm_mod$fitted.values)
    
    # associated IOP with fair outcome
    abs_iop <- gini_ineq(mu_hat, weight = weight)
    
  }
  
  
  # relative IOP
  rel_iop <- abs_iop/total_iop
  
  ans <- list(total_iop = total_iop, abs_iop = abs_iop, rel_iop = rel_iop)
  
  return(ans)
}

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Bootstrap Variance of Inequality of Opportunity ####
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
iop_rel_var_bstr <- function(x, stratum = NULL, cluster = NULL, weight = NULL,
                             circumstances, data = NULL, 
                             distribution = c("smoothed", "standardized"),
                             nboot = 1000, parallel = FALSE, no_cores = NULL){
  
  # x: observations
  # stratum: corresponding stratum IDs for x
  # cluster: corresponding cluster IDs for x
  # weight: corresponding weights for x
  # data: name of data set
  # nboot: The number of bootstrap samples desired.
  
  require(foreach)
  
  # Extracting columns from the data set
  if(!is.null(data)){
    
    if(is.symbol(substitute(x))){
      x <- eval(substitute(x), data, parent.frame())
    }else if(!is.null(x)){
      x <- data[[x]]
    }
    if(is.symbol(substitute(weight))){
      weight <- eval(substitute(weight), data, parent.frame())
    }else if(!is.null(weight)){
      weight <- data[[weight]]
    }
    if(is.symbol(substitute(stratum))){
      stratum <- eval(substitute(stratum), data, parent.frame())
    }else if(!is.null(stratum)){
      stratum <- data[[stratum]]
    }
    if(is.symbol(substitute(cluster))){
      cluster <- eval(substitute(cluster), data, parent.frame())
    }else if(!is.null(cluster)){
      cluster <- data[[cluster]]
    }
    
  }
  
  n <- length(x) # number of observations
  ones <- rep(1, n)
  if (is.null(stratum)){stratum <- ones}
  if (is.null(cluster)){cluster <- ones}
  if (is.null(weight)){weight <- ones}
  
  # Convert circumstances data into a data frame
  if(is.vector(circumstances) | is.factor(circumstances)){
    if(length(x) == length(circumstances)){
      circumstances <- as.data.frame(circumstances)
    }else{
      circumstances <- dplyr::select(data, all_of(circumstances))
    }
  }else{
    circumstances <- as.data.frame(circumstances)
  }
  
  # Find missing values from the data
  na_indices <- (is.na(x) | is.na(stratum) | is.na(cluster) | is.na(weight))
  
  # Remove observations that have missing values
  x <- x[!na_indices]
  stratum <- stratum[!na_indices]
  cluster <- cluster[!na_indices]
  weight <- weight[!na_indices]
  
  # original gini index
  iop_total <- gini_ineq(x, weight)
  
  # Log-linear relationship between the outcome and the circumstance variables
  lm_dt <- as.data.frame(cbind(x, circumstances))
  lm_mod <- lm(log(x) ~ ., lm_dt)
  
  # Estimating the hypothesized distribution that eliminates any differences
  # in individual circumstances
  
  if(distribution == "standardized"){
    
    e_hat <- lm_mod$residuals
    
    # Find the mean of circumstances(s)
    circumstances_mean <- circumstances |>
      summarise(
        # For factors, find the value with the highest proportion
        across(where(is.factor), \(x) names(sort(table(x), decreasing = TRUE))[1]),
        # For numeric values, find the mean
        across(where(is.numeric), \(x) mean(x, na.rm = TRUE))
      ) |>
      data.frame()
    
    # hypothesized standardized distribution (estimated fair outcome)
    x_hat <- exp(predict(lm_mod, newdata = circumstances_mean) + e_hat)
    
    # associated IOP with fair outcome
    iop_xhat <- gini_ineq(x_hat, weight = weight)
    
    # relative IOP
    iop_r <- 1 - iop_xhat/iop_total
    
    theta <- list(iop_xhat = iop_xhat, 
                  iop_total = iop_total,
                  iop_r = iop_r)
    
    # # Create unique IDs for the clusters
    orig_data <- data.frame(stratum, cluster, x, weight, circumstances, iop_xhat)
    
  }else{
    
    # hypothesized smoothed distribution (estimated fair outcome)
    mu_hat <- lm_mod$fitted.values
    
    # associated IOP with fair outcome
    iop_muhat <- gini_ineq(mu_hat, weight = weight)
    
    # relative IOP
    iop_r <- iop_muhat/iop_total
    
    theta <- list(iop_muhat = iop_muhat, 
                  iop_total = iop_total,
                  iop_r = iop_r)
    
    # # Create unique IDs for the clusters
    orig_data <- data.frame(stratum, cluster, x, weight, circumstances, mu_hat)
  }
  
  if(parallel){
    # Parallel
    all_cores <- future::availableCores()
    if(is.null(no_cores)){
      no_cores <-all_cores
    }
    use_cores <- min(no_cores, max(all_cores - 2, 1))
    cl <- parallel::makeCluster(use_cores)
    # parallel::clusterExport(cl, c(lsf.str()))
    doParallel::registerDoParallel(cl)
  }else{
    # Sequential
    foreach::registerDoSEQ()
  }
  
  # thetastar <- foreach::foreach(i = 1:nboot, .combine = "c", 
  #                               .export =c("gini_ineq", "bootstrap_sample", "iop_rel_est")) %dopar%{
  #               
  #     # sample clusters (with replacement) within each stratum
  #     boot_sample <- bootstrap_sample(stratum, cluster, orig_data)
  #     
  #     circumstances <- boot_sample |>
  #       dplyr::select(-x, -stratum, -cluster, -weight)
  #     
  #     iop <- iop_rel_est(boot_sample$x, boot_sample$weight, 
  #                        circumstances, distribution)
  #     
  #     return(iop)
  #   }

  thetastar <- foreach::foreach(i = 1:nboot, .combine = "bind_rows", .export = c("gini_ineq", "iop_rel_est")) %dopar%{
    # sample clusters (with replacement) within each stratum
    boot_sel <- orig_data |>
      dplyr::select(stratum, cluster) |>
      dplyr::group_by(stratum) |>
      dplyr::mutate(Hs = length(unique(cluster))) |>  # number of clusters for each stratum
      dplyr::group_by(stratum, Hs)  |>
      tidyr::nest() |>
      dplyr::ungroup() |>
      dplyr::mutate(samp = purrr::map2(data, Hs, ~dplyr::slice_sample(.x, n = .y, replace = TRUE))) |>
      dplyr::select(-data, -Hs) |>
      tidyr::unnest(samp)

    # extract selected clusters from original data set
    boot_sample <- boot_sel |>
      dplyr::left_join(orig_data,
                       by = dplyr::join_by(stratum, cluster),
                       relationship = "many-to-many")

    # circumstances <- boot_sample |>
    #   dplyr::select(-x, -stratum, -cluster, -weight)
    # 
    # iop <- iop_rel_est(boot_sample$x, boot_sample$weight,
    #                    circumstances, distribution) 
    
    iop_total <- with(boot_sample, gini_ineq(x, weight))
    
    if(distribution == "standardized"){
      
      e_hat <- lm_mod$residuals
      
      # Find the mean of circumstances(s)
      circumstances_mean <- circumstances |>
        summarise(
          # For factors, find the value with the highest proportion
          across(where(is.factor), \(x) names(sort(table(x), decreasing = TRUE))[1]),
          # For numeric values, find the mean
          across(where(is.numeric), \(x) mean(x, na.rm = TRUE))
        ) |>
        data.frame()
      
      # hypothesized standardized distribution (estimated fair outcome)
      x_hat <- exp(predict(lm_mod, newdata = circumstances_mean) + e_hat)
      
      # associated IOP with fair outcome
      iop_xhat <- with(boot_sample, gini_ineq(x_hat, weight))
      
      # relative IOP
      iop_r <- 1 - iop_xhat/iop_total
      
      iop <- list(iop_xhat = iop_xhat, 
                  iop_total = iop_total,
                  iop_r = iop_r)
      
    }else{
      
      # hypothesized smoothed distribution (estimated fair outcome)
      mu_hat <- lm_mod$fitted.values
      
      # associated IOP with fair outcome
      iop_muhat <- with(boot_sample, gini_ineq(mu_hat, weight))
      
      # relative IOP
      iop_r <- iop_muhat/iop_total
      
      iop <- list(iop_muhat = iop_muhat, 
                  iop_total = iop_total,
                  iop_r = iop_r)
    }
    
    return(iop)
  }
  
  if(parallel){
    # End and release all clusters
    parallel::stopCluster(cl)
  }
  
  theta.var <- sum((thetastar$iop_r - theta$iop_r)^2)/nboot
  theta.cov <- sum((thetastar[[1]] - theta$iop_muhat) * (thetastar[[2]] - theta$iop_total))/nboot
  
  ans <- list(est = theta$iop_r, var = theta.var, theta.cov = theta.cov, thetastar = thetastar)
  
  return(ans)
}


#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Optimal Sample Size ####
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

n_opt <- function(alpha, beta, H0, H1, sd){
  
  # alpha: Type I error rate
  # beta: Type II error rate
  # H0: null value
  # H1: 
  # sd: standard deviation
  
  delta <- (H1 - H0)/sd
  
  ans <- ((qnorm(alpha/2) + qnorm(1 - beta))/delta)^2
  
  return(ans)
}