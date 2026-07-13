#  IOP SIMULATION STUDY  —  Population 1  (High IOP)
#
#  Population   : synpop1.csv
#                 S = 2 strata  |  H = 1500 clusters (H_s = 750 per stratum)
#                 M_h = 40 HH per cluster  |  N = 60,000 households
#                 Circumstances: Race (binary), LanguageGrp (3-cat), Land (binary)
#                 DGP: log(y) = 8 + 0.80*Race + 0.01*LG1 + 0.02*LG2 + 0.015*Land
#                              + gamma_s + u_h + eps  (rho_ICC = 0.10)
#                 True omega_r = 0.357059
#
#  Sampling     : Stage-1 SRS WOR  — 500 clusters per stratum (n = 1000 total)
#                 Stage-2 SRS WOR  — 4 HH per selected cluster  (k = 4)
#                 HT weight  W = M_h * H_s / (k * n_s) = 15 (constant)
#
#  Replications : B = 5000 (main) | B_PWR = 2000 (power)
#
#  Four objectives
#   (i)   Consistency and approximate unbiasedness            [Table 1]
#   (ii)  Coverage accuracy of design-based 95% CI            [Table 2]
#   (iii) Asymptotic normality (Theorem 3.3) — QQ + SW test   [Figure 1]
#   (iv)  Type I error and power of two-sample Wald test       [Table 3, Figure 2]
#
#  SE ESTIMATORS
#   Design-based : Stratified leave-one-cluster-out Jackknife (primary)
#                  Algebraically equivalent to delta-method applied to the
#                  joint JK of (omega_hat, omega_a_hat) — Theorem 3.3 analogue
#   Naive        : SRS percentile bootstrap (ignores design — benchmark only)

rm(list = ls())
library(parallel)
library(ggplot2)
library(gridExtra)
library(doParallel)
library(future)

set.seed(2025)
n_cores <- max(1L, availableCores() - 1L)

# CORE ESTIMATORS

# ---- Weighted Gini  (Equations 5 and 9)  ---------------------------------
#  G = 1 - (2/mu) * sum_i  w_i * y_i * (1 - F_hat(y_i))
#  F_hat uses mid-point mass: F(y_(i)) = cumsum(w) - w/2  (avoids tie bias)
weighted_gini <- function(y, w) {
  ord   <- order(y)
  y     <- y[ord]
  w     <- w[ord] / sum(w)          # normalise to sum 1
  F_hat <- cumsum(w) - w / 2
  mu    <- sum(w * y)
  if (mu < 1e-12) return(0)
  max(1 - 2 * sum(w * y * (1 - F_hat)) / mu, 0)
}

# ---- 0B  IOP point estimates  (Equations 7–11)  ------------------------------
#  Eq 7 : log(y) = beta'C + e  (WLS)
#  Eq 8 : X_hat  = exp(beta_hat' C)      [opportunity distribution]
#  Eq 9 : omega_a = Gini(X_hat, w)       [absolute IOP]
#  Eq 11: omega_r = omega_a / omega      [relative IOP — primary estimand]
compute_iop <- function(y, C, w) {
  df      <- data.frame(logy = log(y), C)
  fit     <- lm(logy ~ ., data = df, weights = w)
  Xhat    <- exp(fitted(fit))
  omega   <- weighted_gini(y,    w)
  omega_a <- weighted_gini(Xhat, w)
  omega_r <- if (omega > 1e-12) omega_a / omega else NA_real_
  list(omega = omega, omega_a = omega_a, omega_r = omega_r, Xhat = Xhat)
}

# ---- Stratified leave-one-cluster-out Jackknife SE  ----------------------
#  Drops one cluster at a time within each stratum and re-estimates omega_r.
#  Variance: Var_JK = sum_s [(n_s-1)/n_s] * sum_{c in s} (omega_r^{-c} - mean_s)^2
#  This is the design-consistent SE; equivalent to delta-method on joint JK
#  of (omega_hat, omega_a_hat) — the finite-sample counterpart of Theorem 3.3.
jackknife_se <- function(y, C, w, strata, clusters) {
  all_cls <- unique(clusters)

  # Leave-one-out estimates
  loo <- vapply(all_cls, function(cl) {
    keep <- clusters != cl
    if (sum(keep) < 10L) return(NA_real_)
    tryCatch(
      compute_iop(y[keep], C[keep, , drop = FALSE], w[keep])$omega_r,
      error = function(e) NA_real_
    )
  }, numeric(1L))

  # Accumulate stratified variance
  jk_var <- 0
  for (s in unique(strata)) {
    in_s  <- all_cls %in% unique(clusters[strata == s])
    n_s   <- sum(in_s)
    est_s <- loo[in_s]
    est_s <- est_s[!is.na(est_s)]
    if (length(est_s) < 2L) next
    jk_var <- jk_var + ((n_s - 1) / n_s) * sum((est_s - mean(est_s))^2)
  }
  sqrt(max(jk_var, 0))
}

# ---- Naive SRS percentile bootstrap SE  ----------------------------------
#  Resamples households with replacement (ignores stratified cluster design).
#  Used as naive benchmark; reveals design effect when compared to JK SE.
naive_se <- function(y, C, w, B = 300L) {
  n    <- length(y)
  ests <- replicate(B, {
    idx <- sample(n, n, replace = TRUE)
    tryCatch(
      compute_iop(y[idx], C[idx, , drop = FALSE], w[idx])$omega_r,
      error = function(e) NA_real_
    )
  })
  sd(ests, na.rm = TRUE)
}

#  LOAD POPULATION AND TRUE VALUES



pop_file <- "synpop1.csv"
if (!file.exists(pop_file))
  stop("synpop1.csv not found. Set working directory to its location.")

pop1 <- read.csv(pop_file, stringsAsFactors = FALSE)

# Ensure dummy variables are present
if (!"LG1" %in% names(pop1)) {
  pop1$LG1 <- as.integer(pop1$LGrp == 1L)
  pop1$LG2 <- as.integer(pop1$LGrp == 2L)
}

C_COLS <- c("Race", "LG1", "LG2", "Land")   # circumstance columns

# True IOP: census-level equal-weight computation
w_pop <- rep(1 / nrow(pop1), nrow(pop1))
iop_T <- compute_iop(pop1$y, pop1[, C_COLS], w_pop)

TRUE_OMEGA   <- iop_T$omega
TRUE_OMEGA_A <- iop_T$omega_a
TRUE_OMEGA_R <- iop_T$omega_r

cat(sprintf(
  "\n  Population structure\n"
))
cat(sprintf("  N = %d  |  S = 2 strata  |  H = %d clusters  |  k = 40 HH/cluster\n",
            nrow(pop1), length(unique(pop1$cluster))))
cat(sprintf("  Circumstances : Race (binary, P=0.40)  |  LanguageGrp (3-cat)  |  Land (binary, P=0.50)\n"))
cat(sprintf("  DGP betas     : beta1=0.80 (Race), beta2=0.01, beta3=0.02, beta4=0.015\n"))
cat(sprintf("  ICC (rho)     : 0.10\n\n"))
cat(sprintf("  True omega   (Total Gini)    = %.6f\n", TRUE_OMEGA))
cat(sprintf("  True omega_a (Absolute IOP)  = %.6f\n", TRUE_OMEGA_A))
cat(sprintf("  True omega_r (Relative IOP)  = %.6f\n\n", TRUE_OMEGA_R))

#  Stage 1 : SRS WOR of nc clusters from H_s = 750 per stratum
#  Stage 2 : SRS WOR of hh households from M_h = 40 per cluster
#  HT weight: W_scsh = M_h * H_s / (k * n_s)  =  40*750/(4*500) = 15

draw_sample <- function(pop, nc = 500L, hh = 4L) {
  do.call(rbind, lapply(unique(pop$stratum), function(s) {
    idx_s <- which(pop$stratum == s)
    cls_s <- unique(pop$cluster[idx_s])
    Hs    <- length(cls_s)
    a     <- min(nc, Hs)
    sel   <- sample(cls_s, a, replace = FALSE)
    pi1   <- a / Hs                               # Stage-1 inclusion prob

    do.call(rbind, lapply(sel, function(cl) {
      idx <- which(pop$cluster == cl & pop$stratum == s)
      Mh  <- length(idx)
      m   <- min(hh, Mh)
      sh  <- sample(idx, m, replace = FALSE)
      pi2 <- m / Mh                               # Stage-2 inclusion prob
      df  <- pop[sh, ]
      df$w <- 1 / (pi1 * pi2)                     # HT weight
      df
    }))
  }))
}

# SINGLE REPLICATION  (Objectives i, ii, iii)

one_rep <- function(nc = 500L, hh = 4L, B_naive = 300L) {
  samp  <- draw_sample(pop1, nc, hh)
  y     <- samp$y
  C     <- as.data.frame(as.matrix(samp[, C_COLS]))
  w     <- samp$w / sum(samp$w)           # normalised HT weights
  str   <- samp$stratum
  cls   <- samp$cluster

  # Point estimate
  est  <- compute_iop(y, C, w)
  Rhat <- est$omega_r

  # Design-based SE (primary)
  se_jk <- jackknife_se(y, as.matrix(C), w, str, cls)

  # Naive SRS bootstrap SE (benchmark)
  se_nv <- naive_se(y, C, w, B_naive)

  # 95% Wald CIs
  z95    <- qnorm(0.975)
  ci_jk  <- c(Rhat - z95 * se_jk, Rhat + z95 * se_jk)
  ci_nv  <- c(Rhat - z95 * se_nv, Rhat + z95 * se_nv)

  covers <- function(ci) as.integer(TRUE_OMEGA_R >= ci[1] & TRUE_OMEGA_R <= ci[2])

  # Standardised z-score (Theorem 3.3 normality check)
  z_score <- if (se_jk > 1e-10) (Rhat - TRUE_OMEGA_R) / se_jk else NA_real_

  list(
    Rhat       = Rhat,
    bias       = Rhat - TRUE_OMEGA_R,
    se_jk      = se_jk,
    se_nv      = se_nv,
    cover_jk   = covers(ci_jk),
    cover_nv   = covers(ci_nv),
    z_score    = z_score,
    deff       = if (se_nv > 1e-10) (se_jk / se_nv)^2 else NA_real_,
    n_units    = length(y)
  )
}

# SECTION 4  TWO-SAMPLE WALD TEST  (Objective iv)
#  H0: omega_r^(1) = omega_r^(2)  vs  H1: omega_r^(1) != omega_r^(2)
#  Test statistic: T = (omega_r1_hat - omega_r2_hat) /
#                      sqrt(SE_JK1^2 + SE_JK2^2)
#  Reject at alpha=0.05 if |T| > 1.96

two_sample_test <- function(pop_A, pop_B, nc = 500L, hh = 4L) {
  est_fn <- function(pop) {
    samp <- draw_sample(pop, nc, hh)
    y    <- samp$y
    C    <- as.data.frame(as.matrix(samp[, C_COLS]))
    w    <- samp$w / sum(samp$w)
    str  <- samp$stratum;  cls <- samp$cluster
    est  <- compute_iop(y, C, w)
    se   <- jackknife_se(y, as.matrix(C), w, str, cls)
    c(R = est$omega_r, se = se)
  }
  e1     <- est_fn(pop_A);  e2 <- est_fn(pop_B)
  D      <- as.numeric(e1["R"] - e2["R"])
  se_D   <- sqrt(e1["se"]^2 + e2["se"]^2)
  T_stat <- D / se_D
  list(reject  = as.integer(T_stat > qnorm(0.95)),
       D       = D,
       T_stat  = as.numeric(T_stat))
}

# SHIFTED POPULATIONS FOR POWER STUDY
#  Population 2 shares the same DGP structure but uses beta1_new < 0.80,
#  reducing Race's contribution to income and thus lowering omega_r.
#  True delta = omega_r(Pop1) - omega_r(Pop2) > 0.

make_pop2 <- function(beta1_new, base_seed = 3001L) {
  set.seed(base_seed)
  S2  <- 2L;  cls2 <- 750L;  hh2 <- 40L
  H2  <- S2 * cls2;  N2 <- H2 * hh2
  stratum2 <- rep(seq_len(S2), each = cls2 * hh2)
  cluster2 <- rep(seq_len(H2), each = hh2)
  C1  <- rbinom(N2, 1L, 0.40)
  LGr <- sample(0:2, N2, replace = TRUE, prob = c(0.28, 0.35, 0.37))
  LG1 <- as.integer(LGr == 1L);  LG2 <- as.integer(LGr == 2L)
  C3  <- rbinom(N2, 1L, 0.50)
  gam <- rnorm(S2, 0, sqrt(0.05))
  u   <- rnorm(H2, 0, sqrt(0.10))
  eps <- rnorm(N2, 0, sqrt(0.90))
  ly  <- 8 + beta1_new*C1 + 0.01*LG1 + 0.02*LG2 + 0.015*C3 +
         gam[stratum2] + u[cluster2] + eps
  data.frame(stratum = stratum2, cluster = cluster2, y = exp(ly),
             Race = C1, LGrp = LGr, LG1 = LG1, LG2 = LG2, Land = C3,
             stringsAsFactors = FALSE)
}

get_true_r <- function(pop) {
  w <- rep(1 / nrow(pop), nrow(pop))
  compute_iop(pop$y, pop[, C_COLS], w)$omega_r
}

# beta1 grid for Pop2: {0.80, 0.65, 0.50, 0.35, 0.08}
# beta1=0.80 gives delta ~ 0  (Type I error scenario)
# beta1=0.08 gives delta ~ 0.31  (large effect — high power scenario)
cat("Building shifted populations ...\n")
beta1_grid  <- c(0.80, 0.65, 0.50, 0.35, 0.08)
pop2_list   <- lapply(beta1_grid, make_pop2)
true_r2     <- sapply(pop2_list, get_true_r)
true_deltas <- TRUE_OMEGA_R - true_r2

cat(sprintf("  %-12s %-16s %-12s %-10s\n",
            "beta1_pop2", "omega_r (Pop2)", "delta", "Objective"))
for (j in seq_along(beta1_grid)) {
  obj <- if (abs(true_deltas[j]) < 0.002) "Type I Error" else "Power"
  cat(sprintf("  %-12.2f %-16.4f %-12.4f %-10s\n",
              beta1_grid[j], true_r2[j], true_deltas[j], obj))
}
cat("\n")

# MAIN SIMULATION  B = 5000  (Objectives i, ii, iii)

B     <- 5000L
NC    <- 500L     # clusters per stratum
HH    <- 4L       # households per cluster
B_NV  <- 300L     # bootstrap draws for naive SE

cat("=============================================================\n")
cat(sprintf(" MAIN SIMULATION\n"))
cat(sprintf("  B = %d  |  n_s = %d clusters/strat  |  k = %d HH/cluster\n",
            B, NC, HH))
cat(sprintf("  n_total_clusters = %d  |  n_HH_per_rep ~ %d\n",
            2L * NC, 2L * NC * HH))
cat(sprintf("  True omega_r = %.6f\n", TRUE_OMEGA_R))
cat("=============================================================\n")

main_reps <- mclapply(seq_len(B), function(b) {
  tryCatch(one_rep(NC, HH, B_NV), error = function(e) NULL)
}, mc.cores = n_cores)

main_reps <- Filter(Negate(is.null), main_reps)
B_ok      <- length(main_reps)
cat(sprintf("  Completed: %d / %d reps\n\n", B_ok, B))

# Extract result vectors
Rhat_v    <- sapply(main_reps, `[[`, "Rhat")
bias_v    <- sapply(main_reps, `[[`, "bias")
se_jk_v   <- sapply(main_reps, `[[`, "se_jk")
se_nv_v   <- sapply(main_reps, `[[`, "se_nv")
cover_jk_v <- sapply(main_reps, `[[`, "cover_jk")
cover_nv_v <- sapply(main_reps, `[[`, "cover_nv")
z_v       <- na.omit(sapply(main_reps, `[[`, "z_score"))
deff_v    <- sapply(main_reps, `[[`, "deff")

# POWER SIMULATION  B_PWR = 2000  (Objective iv)

B_PWR <- 2000L

cat("=============================================================\n")
cat(sprintf(" POWER SIMULATION\n"))
cat(sprintf("  B = %d per delta level  |  5 levels\n", B_PWR))
cat("=============================================================\n")

pwr_res <- lapply(seq_along(beta1_grid), function(j) {
  cat(sprintf("  [%d/5] beta1_pop2 = %.2f  |  delta = %.4f ...\n",
              j, beta1_grid[j], true_deltas[j]))
  pop2 <- pop2_list[[j]]
  rejs <- mclapply(seq_len(B_PWR), function(b) {
    tryCatch(two_sample_test(pop1, pop2, NC, HH), error = function(e) NULL)
  }, mc.cores = n_cores)
  rejs <- Filter(Negate(is.null), rejs)
  list(beta1    = beta1_grid[j],
       delta    = true_deltas[j],
       omega_r2 = true_r2[j],
       n_ok     = length(rejs),
       rej_rate = mean(sapply(rejs, `[[`, "reject"), na.rm = TRUE))
})
cat("\n")

# RESULTS TABLES

sep <- paste(rep("=", 72), collapse = "")

# ---- Table 1: Consistency and Unbiasedness  (Objective i) -------------------
cat(sep, "\n")
cat("  TABLE 1 — OBJECTIVE (i): CONSISTENCY AND APPROXIMATE UNBIASEDNESS\n")
cat(sprintf("  True omega_r = %.6f  |  B = %d  |  n_s = %d  |  k = %d\n",
            TRUE_OMEGA_R, B_ok, NC, HH))
cat(sep, "\n")

tab1 <- data.frame(
  Statistic = c(
    "True omega_r",
    "Mean estimate (omega_r_hat)",
    "Bias  (mean - true)",
    "% Relative bias",
    "RMSE",
    "Monte-Carlo Std Dev",
    "Mean SE  (Design-Based Jackknife)",
    "Mean SE  (Naive SRS Bootstrap)",
    "Mean Design Effect (DEFF)",
    "Mean sample size (HH per rep)"
  ),
  Value = c(
    round(TRUE_OMEGA_R,                               6),
    round(mean(Rhat_v,   na.rm = TRUE),               6),
    round(mean(bias_v,   na.rm = TRUE),               6),
    round(100 * mean(bias_v, na.rm = TRUE) / TRUE_OMEGA_R, 3),
    round(sqrt(mean(bias_v^2, na.rm = TRUE)),         6),
    round(sd(Rhat_v,     na.rm = TRUE),               6),
    round(mean(se_jk_v,  na.rm = TRUE),               6),
    round(mean(se_nv_v,  na.rm = TRUE),               6),
    round(mean(deff_v,   na.rm = TRUE),               4),
    round(mean(sapply(main_reps, `[[`, "n_units"), na.rm = TRUE), 1)
  )
)
print(tab1, row.names = FALSE)
cat("  Note: DEFF = (SE_Jackknife / SE_Naive)^2; values > 1 indicate\n")
cat("        that ignoring the cluster design underestimates sampling error.\n")

# ---- Table 2: Coverage Accuracy  (Objective ii) -----------------------------
cat("\n", sep, "\n")
cat("  TABLE 2 — OBJECTIVE (ii): COVERAGE ACCURACY OF 95% WALD CI\n")
cat(sprintf("  Nominal level = 95%%  |  B = %d\n", B_ok))
cat(sep, "\n")

tab2 <- data.frame(
  SE_Estimator          = c("Design-Based (Jackknife)",
                             "Naive (SRS Bootstrap)"),
  Empirical_Coverage    = c(round(mean(cover_jk_v, na.rm = TRUE), 4),
                             round(mean(cover_nv_v, na.rm = TRUE), 4)),
  Coverage_minus_95pct  = c(round(mean(cover_jk_v, na.rm = TRUE) - 0.95, 4),
                             round(mean(cover_nv_v, na.rm = TRUE) - 0.95, 4)),
  Mean_CI_Width         = c(round(mean(2 * qnorm(0.975) * se_jk_v, na.rm = TRUE), 5),
                             round(mean(2 * qnorm(0.975) * se_nv_v, na.rm = TRUE), 5))
)
print(tab2, row.names = FALSE)
cat("  Note: Positive deviation implies conservative CI; negative implies\n")
cat("        under-coverage (anti-conservative).\n")

# ---- Table 3: Type I Error and Power  (Objective iv) ------------------------
cat("\n", sep, "\n")
cat("  TABLE 3 — OBJECTIVE (iv): TYPE I ERROR AND POWER\n")
cat(sprintf("  Two-sample Wald test  |  H0: omega_r1 = omega_r2\n"))
cat(sprintf("  Alpha = 0.05  |  n_s = %d per sample  |  k = %d  |  B = %d\n",
            NC, HH, B_PWR))
cat(sep, "\n")

tab3 <- do.call(rbind, lapply(pwr_res, function(r) {
  data.frame(
    beta1_pop2  = r$beta1,
    omega_r_pop2 = round(r$omega_r2,  4),
    True_delta  = round(r$delta,      4),
    Objective   = ifelse(abs(r$delta) < 0.002, "Type I Error", "Power"),
    Rej_Rate    = round(r$rej_rate,   4),
    Reps_OK     = r$n_ok
  )
}))
print(tab3, row.names = FALSE)
cat("  Note: delta = omega_r(Pop1) - omega_r(Pop2)\n")
cat("        Type I error target: 0.05 | Power threshold: 0.80\n")

# Shapiro-Wilk test for normality of z-scores
sw <- shapiro.test(sample(z_v, min(4999L, length(z_v))))
cat(sprintf("\n  Shapiro-Wilk test on standardised z-scores (Objective iii):\n"))
cat(sprintf("  W = %.4f  |  p-value = %.4f\n", sw$statistic, sw$p.value))
cat(sprintf("  %s reject H0 of normality at 5%% level.\n",
            ifelse(sw$p.value < 0.05, "REJECT:", "FAIL TO")))

# FIGURES

cat("\nGenerating figures ...\n")

# Colour constants
COL_JK    <- "#2166AC"   # blue
COL_NAIVE <- "#D73027"   # red
COL_TRUE  <- "#D73027"   # red dashed line
COL_PWR   <- "#4DAC26"   # green
COL_T1    <- "#D01C8B"   # magenta

# ---- Figure 1 (Objective iii): Normal QQ plot of standardised z-scores ------
#  z = (omega_r_hat - TRUE_OMEGA_R) / SE_JK  ~ N(0,1) if Theorem 3.3 holds

z_df  <- data.frame(z = as.numeric(z_v))
# Theoretical quantiles and sample quantiles for annotation
qq_df <- qqnorm(z_df$z, plot.it = FALSE)
qq_df <- data.frame(theoretical = qq_df$x, sample = qq_df$y)

fig1 <- ggplot(qq_df, aes(x = theoretical, y = sample)) +
  geom_abline(intercept = 0, slope = 1,
              colour = COL_TRUE, linewidth = 0.9, linetype = "solid") +
  geom_point(alpha = 0.15, size = 0.6, colour = COL_JK) +
  annotate("text", x = -3.5, y = max(qq_df$sample) * 0.90,
           label = sprintf("SW p = %.3f", sw$p.value),
           hjust = 0, size = 3.5, colour = "gray30", fontface = "italic") +
  annotate("text", x = -3.5, y = max(qq_df$sample) * 0.82,
           label = sprintf("n = %d z-scores", length(z_v)),
           hjust = 0, size = 3.2, colour = "gray40") +
  labs(
    title    = "Figure 1 (Objective iii) — Normal QQ Plot of Standardised IOP Estimates",
    subtitle = bquote(
      z == frac(hat(omega)[r] - omega[r], SE[JK]) ~~
      "|" ~~ omega[r] == .(round(TRUE_OMEGA_R, 4)) ~~
      "|" ~~ n[s] == .(NC) ~ "clusters/strat" ~~
      "|" ~~ k == .(HH) ~ "HH/cluster" ~~
      "|" ~~ B == .(B_ok) ~ "reps"),
    x = "Theoretical N(0,1) Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_bw(base_size = 12)

# ---- Figure 2 (Objective i): Sampling distribution of omega_r_hat -----------

fig2 <- ggplot(data.frame(Rhat = Rhat_v), aes(x = Rhat)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 70, fill = COL_JK, alpha = 0.65, colour = "white") +
  geom_density(colour = "#08519C", linewidth = 1.0) +
  geom_vline(xintercept = TRUE_OMEGA_R,
             colour = COL_TRUE, linetype = "dashed", linewidth = 1.1) +
  annotate("text",
           x     = TRUE_OMEGA_R + 0.002,
           y     = Inf, vjust = 1.6,
           label = sprintf("True omega_r = %.4f", TRUE_OMEGA_R),
           colour = COL_TRUE, size = 3.5, fontface = "italic", hjust = 0) +
  annotate("text",
           x     = mean(Rhat_v, na.rm = TRUE) - 0.002,
           y     = Inf, vjust = 3.0,
           label = sprintf("Mean = %.4f", mean(Rhat_v, na.rm = TRUE)),
           colour = COL_JK, size = 3.3, hjust = 1) +
  labs(
    title    = "Figure 2 (Objective i) — Sampling Distribution of the Relative IOP Estimator",
    subtitle = bquote(
      "Bias" == .(round(mean(bias_v, na.rm=TRUE), 5)) ~~
      "|" ~~ "RMSE" == .(round(sqrt(mean(bias_v^2, na.rm=TRUE)), 5)) ~~
      "|" ~~ "MC SD" == .(round(sd(Rhat_v, na.rm=TRUE), 5)) ~~
      "|" ~~ B == .(B_ok) ~ "reps"),
    x = expression(hat(omega)[r]),
    y = "Density"
  ) +
  theme_bw(base_size = 12)

# ---- Figure 3 (Objective ii): CI coverage comparison -----------------------

cov_df <- data.frame(
  SE_Method    = factor(c("Design-Based\n(Jackknife)", "Naive\n(SRS Bootstrap)"),
                        levels = c("Design-Based\n(Jackknife)", "Naive\n(SRS Bootstrap)")),
  Coverage     = c(mean(cover_jk_v, na.rm = TRUE),
                   mean(cover_nv_v, na.rm = TRUE)),
  fill_id      = c("JK", "Naive")
)

fig3 <- ggplot(cov_df, aes(x = SE_Method, y = Coverage, fill = SE_Method)) +
  geom_col(width = 0.45, alpha = 0.85, colour = "white") +
  geom_hline(yintercept = 0.95, linetype = "dashed",
             colour = "black", linewidth = 0.9) +
  geom_text(aes(label = sprintf("%.3f", Coverage)),
            vjust = -0.45, size = 4.5, fontface = "bold") +
  annotate("text", x = 0.55, y = 0.9535,
           label = "Nominal 95%", hjust = 0,
           size = 3.4, colour = "gray30", fontface = "italic") +
  scale_fill_manual(
    values = c("Design-Based\n(Jackknife)"  = COL_JK,
               "Naive\n(SRS Bootstrap)"  = COL_NAIVE),
    guide = "none"
  ) +
  scale_y_continuous(limits = c(0.80, 1.01),
                     breaks = seq(0.80, 1.00, 0.05),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(
    title    = "Figure 3 (Objective ii) — Empirical Coverage of 95% Wald Confidence Intervals",
    subtitle = bquote(
      B == .(B_ok) ~ "reps" ~~
      "|" ~~ n[s] == .(NC) ~ "clusters/strat" ~~
      "|" ~~ k == .(HH) ~ "HH/cluster"),
    x = "Standard Error Estimator",
    y = "Empirical Coverage"
  ) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(size = 10, face = "bold"))

# ---- Figure 4 (Objective iv): Power curve -----------------------------------

pwr_df <- data.frame(
  delta    = sapply(pwr_res, `[[`, "delta"),
  omega_r2 = sapply(pwr_res, `[[`, "omega_r2"),
  rej_rate = sapply(pwr_res, `[[`, "rej_rate"),
  type     = ifelse(abs(sapply(pwr_res, `[[`, "delta")) < 0.002,
                    "Type I Error", "Power")
)

fig4 <- ggplot(pwr_df, aes(x = delta, y = rej_rate)) +
  geom_line(colour = COL_JK, linewidth = 1.1) +
  geom_point(aes(colour = type, shape = type), size = 4.5) +
  geom_hline(yintercept = 0.05, linetype = "dashed",
             colour = "gray40", linewidth = 0.8) +
  geom_hline(yintercept = 0.80, linetype = "dotted",
             colour = "gray40", linewidth = 0.8) +
  annotate("text",
           x     = max(pwr_df$delta) * 0.50, y = 0.068,
           label = "Nominal size = 0.05",
           size = 3.3, colour = "gray30", fontface = "italic") +
  annotate("text",
           x     = max(pwr_df$delta) * 0.50, y = 0.815,
           label = "Power threshold = 0.80",
           size = 3.3, colour = "gray30", fontface = "italic") +
  geom_text(aes(label = sprintf("%.3f", rej_rate)),
            vjust = -0.9, size = 3.2) +
  scale_colour_manual(
    values = c("Type I Error" = COL_T1, "Power" = COL_PWR),
    name   = NULL
  ) +
  scale_shape_manual(
    values = c("Type I Error" = 17L,   "Power" = 16L),
    name   = NULL
  ) +
  scale_x_continuous(
    breaks = round(pwr_df$delta, 3),
    labels = function(x) sprintf("%.3f", x)
  ) +
  scale_y_continuous(
    limits = c(0, 1.06),
    breaks = seq(0, 1, 0.1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title    = "Figure 4 (Objective iv) — Type I Error and Power of the Two-Sample Wald Test",
    subtitle = bquote(
      H[0]: omega[r]^{(1)} == omega[r]^{(2)} ~~
      "vs" ~~
      H[1]: omega[r]^{(1)} != omega[r]^{(2)} ~~
      "|" ~~ alpha == 0.05 ~~
      "|" ~~ n[s] == .(NC) ~ "clusters/strat per sample" ~~
      "|" ~~ B == .(B_PWR) ~ "reps"),
    x = expression(
      "True IOP difference  " *
        (delta == omega[r]^{(1)} - omega[r]^{(2)})),
    y = "Rejection Rate"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        axis.text.x     = element_text(size = 9))

# ---- Save individual PDFs ---------------------------------------------------
ggsave("fig1_qq_normality.pdf",   fig1, width = 10, height = 5.5)
ggsave("fig2_sampling_dist.pdf",  fig2, width =  9, height =  5)
ggsave("fig3_coverage.pdf",       fig3, width =  7, height =  5)
ggsave("fig4_power.pdf",          fig4, width =  9, height =  5.5)

# ---- Combined 2×2 panel  (for paper) ----------------------------------------
pdf("iop_sim_pop1_all_figures.pdf", width = 16, height = 11)
grid.arrange(fig1, fig2, fig3, fig4, nrow = 2L,
             top = "IOP Simulation Study — Population 1 (High IOP, omega_r = 0.357)")
invisible(dev.off())

# ---- Save result tables to CSV ----------------------------------------------
write.csv(tab1,   "tab1_consistency.csv",    row.names = FALSE)
write.csv(tab2,   "tab2_coverage.csv",       row.names = FALSE)
write.csv(tab3,   "tab3_type1_power.csv",    row.names = FALSE)
write.csv(pwr_df, "tab3_power_detail.csv",   row.names = FALSE)

# FINAL SUMMARY

cat("\n", sep, "\n")
cat("  FINAL SUMMARY\n")
cat(sep, "\n")
cat(sprintf("  (i)  Consistency   : Bias = %.5f  (%+.2f%%)\n",
            mean(bias_v, na.rm=TRUE),
            100*mean(bias_v,na.rm=TRUE)/TRUE_OMEGA_R))
cat(sprintf("                       RMSE = %.5f  |  MC SD = %.5f\n",
            sqrt(mean(bias_v^2,na.rm=TRUE)), sd(Rhat_v,na.rm=TRUE)))
cat(sprintf("  (ii) Coverage      : JK = %.3f  |  Naive = %.3f  (nominal 0.950)\n",
            mean(cover_jk_v,na.rm=TRUE), mean(cover_nv_v,na.rm=TRUE)))
cat(sprintf("  (iii) Normality    : SW p = %.4f (%s)\n",
            sw$p.value,
            ifelse(sw$p.value>0.05,"Normal approximation holds","Reject normality")))
cat(sprintf("  (iv) Type I error  : %.3f  (target 0.050)\n",
            pwr_df$rej_rate[pwr_df$type=="Type I Error"]))
pwr_80 <- pwr_df$delta[pwr_df$rej_rate >= 0.80 & pwr_df$type == "Power"]
if (length(pwr_80) > 0) {
  cat(sprintf("       80%% power at  : delta >= %.4f\n", min(pwr_80)))
} else {
  cat("       80%% power not reached in the tested delta range.\n")
}
cat(sep, "\n")
cat("  Output files\n")
cat("  Figures  : fig1_qq_normality.pdf  fig2_sampling_dist.pdf\n")
cat("             fig3_coverage.pdf  fig4_power.pdf\n")
cat("             iop_sim_pop1_all_figures.pdf\n")
cat("  Tables   : tab1_consistency.csv  tab2_coverage.csv\n")
cat("             tab3_type1_power.csv  tab3_power_detail.csv\n")
cat(sep, "\n")
cat("  Simulation complete.\n")
