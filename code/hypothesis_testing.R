#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Installing and Loading Required Packages ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("load_packages.R")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load Custom Functions ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("custom_functions.R")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load Data Set ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Directory to save results
outputs_dir <- file.path("..", "Outputs")

# Survey name
survey_names <- c("nss64", "nss_hce23_24")

all_iop_results <- sapply(survey_names, 
                         function(x) {
                           df <- readr::read_csv(file.path(outputs_dir, paste0(x, "_iop_results_v2.csv")))
                           df$survey_name <- x
                           return(df)
                         },
                         simplify = FALSE
                         )  %>% 
  purrr::list_rbind() %>% 
  dplyr::relocate(survey_name) 


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Hypothesis Testing ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

sig_codes <- "p-value \\leq 0.001: ***; 0.001 < p-value \\leq 0.01: **; 0.01 < p-value \\leq 0.05: *; 0.05 < p-value \\leq 0.1: .; 0.1 < p-value \\leq 1: ns"
## Test Across Time Periods: NSS64 vs NSS 23-24 ####

# Convert data into wide format
iop_results_wide_time <- all_iop_results %>% 
  dplyr::select(survey_name, state, state_name, sector_name, iop_rel_wtd, var) %>% 
  tidyr::pivot_wider(names_from = "survey_name", values_from = c("iop_rel_wtd", "var"))

# Compute hypothesis testing values 
## estimates, standard errors, test statistics, p-values
hyp_test_results_time <- iop_results_wide_time %>% 
  dplyr::mutate(diff_est = iop_rel_wtd_nss64 - iop_rel_wtd_nss_hce23_24,
                diff_var = var_nss64 + var_nss_hce23_24,
                diff_se = sqrt(diff_var),
                test_stat = diff_est/diff_se,
                p_value_rt = pnorm(test_stat, lower.tail = FALSE),
                p_value_2t = 2*pnorm(abs(test_stat), lower.tail = FALSE)) %>% 
  dplyr::mutate(sig_rt = dplyr::case_when(
    p_value_rt <= 0.001 ~ "***",
    p_value_rt <= 0.01 ~ "**",
    p_value_rt <= 0.05 ~ "*",
    p_value_rt <= 0.1 ~ ".",
    .default = "")
    ) %>% 
  dplyr::mutate(sig_2t = dplyr::case_when(
    p_value_2t <= 0.001 ~ "***",
    p_value_2t <= 0.01 ~ "**",
    p_value_2t <= 0.05 ~ "*",
    p_value_2t <= 0.1 ~ ".",
    .default = "")
  ) 


# Save Results as CSV file
readr::write_csv(hyp_test_results_time,
                 file.path(outputs_dir, "hyp_test_results_time_v3.csv"))


# Save Results as tex file
hyp_test_results_time %>% 
  dplyr::select(state_name, sector_name, diff_est, diff_se, test_stat, p_value_rt, sig_rt) %>% 
  dplyr::mutate(across(where(is.numeric), \(x) round(x, digits = 4))) %>% 
  dplyr::mutate(state_name = if_else(duplicated(state_name), "", state_name)) %>% 
  knitr::kable(format = "latex", booktabs = TRUE,
               col.names = c("State", "Sector", "Difference", "Std. Error", "Test Stat", "p-value", "Sig."),
               linesep = ifelse(c(.$state_name[-1], NA) == "", "", "\\addlinespace")
  ) %>% 
  kableExtra::row_spec(0, bold = TRUE) %>% 
  kableExtra::row_spec(0, extra_latex_after = colnumbering(7)) %>%
  # kableExtra::add_footnote(sig_codes, escape = FALSE) %>% 
  kableExtra::save_kable(file = file.path(outputs_dir, "tab_hyp_test_results_time_v3.tex"),
                         keep_tex = TRUE,
                         self_contained = TRUE)

#...............................................................................
## Test Rural vs Urban for Each Survey####

# Convert data into wide format
iop_results_wide_sector <- all_iop_results %>% 
  dplyr::filter(sector_name %in% c("Rural", "Urban")) %>% 
  dplyr::select(survey_name, state, state_name, sector_name, iop_rel_wtd, var) %>% 
  tidyr::pivot_wider(names_from = "sector_name", values_from = c("iop_rel_wtd", "var"))

# Compute hypothesis testing values 
## estimates, standard errors, test statistics, p-values
hyp_test_results_sector <- iop_results_wide_sector %>% 
  dplyr::mutate(diff_est = iop_rel_wtd_Rural - iop_rel_wtd_Urban,
                diff_var = var_Rural + var_Urban,
                diff_se = sqrt(diff_var),
                test_stat = diff_est/diff_se,
                p_value = 2*pnorm(abs(test_stat), lower.tail = FALSE)) %>% 
  dplyr::mutate(sig = dplyr::case_when(
    p_value <= 0.001 ~ "***",
    p_value <= 0.01 ~ "**",
    p_value <= 0.05 ~ "*",
    p_value <= 0.1 ~ ".",
    .default = "")
  ) 


# Save Results as CSV file
readr::write_csv(hyp_test_results_sector,
                 file.path(outputs_dir, "hyp_test_results_sector_v2.csv"))


# Save Results as tex file
hyp_test_results_sector %>% 
  dplyr::select(survey_name, state_name, diff_est, diff_se, test_stat, p_value, sig) %>% 
  dplyr::mutate(across(where(is.numeric), \(x) round(x, digits = 4))) %>% 
  dplyr::mutate(survey_name = if_else(duplicated(survey_name), "", survey_name)) %>%
  dplyr::mutate(survey_name = dplyr::case_match(survey_name,
                                                "nss_hce23_24" ~ "NSS 2023-2024",
                                                "nss64" ~ "NSS 2007-2008",
                                                .default = survey_name)
                ) %>% 
  knitr::kable(format = "latex", booktabs = TRUE,
               col.names = c("Survey", "State", "Difference", "Std. Error", "Test Stat", "p-value", "Sig."),
               linesep = ifelse(c(.$survey_name[-1], NA) == "", "", "\\addlinespace")
  ) %>% 
  kableExtra::row_spec(0, bold = TRUE) %>% 
  kableExtra::row_spec(0, extra_latex_after = colnumbering(7)) %>%
  kableExtra::save_kable(file = file.path(outputs_dir, "tab_hyp_test_results_sector_v2.tex"),
                         keep_tex = TRUE,
                         self_contained = TRUE)
