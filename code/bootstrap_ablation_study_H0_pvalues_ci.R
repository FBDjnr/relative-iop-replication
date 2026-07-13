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
outputs_dir <- file.path("..", "output", "tables")

# Survey name
survey_name1 <- "nss64"
survey_name2 <- "nss_hce23_24"

# Read data file
nss07_ablation_results <- readr::read_csv(
  file.path(outputs_dir, paste0(survey_name1, "_ablation_results", ".csv"))
)

nss23_ablation_results <- readr::read_csv(
  file.path(outputs_dir, paste0(survey_name2, "_ablation_results", ".csv"))
)


boot_ablation_results <- readr::read_csv(
  file.path(outputs_dir, paste0("boot_ablation_results_H0", ".csv"))
)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Extract Original Relative IOP and Ablation Relative IOP ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

ablation_results <- dplyr::bind_rows(
  nss07_ablation_results %>% dplyr::mutate(Year = "2007"),
  nss23_ablation_results %>% dplyr::mutate(Year = "2023"),
)

# full data relative IOP
iop_rel_rslt <- ablation_results %>% 
  dplyr::filter(ablation == "none") %>% 
  dplyr::select(Year, state, state_name, sector_name, iop_rel_wtd)

# ablation result for relative IOP
orig_ablation_rslt <- ablation_results %>% 
  dplyr::filter(ablation != "none") %>% 
  dplyr::select(Year, ablation, state, state_name, sector_name, iop_rel_wtd) %>% 
  dplyr::rename(ablated_var = ablation, orig_abl_iop_rel_wtd = iop_rel_wtd)

# orig_ablation_rslt_wide <- orig_ablation_rslt %>% 
#   tidyr::pivot_wider(names_from = Year,
#                      values_from = orig_abl_iop_rel_wtd,
#                      names_prefix = "orig_abl_rel_iop_") %>% 
#   dplyr::mutate(orig_abl_rel_iop_diff)

boot_ablation_results_long <- boot_ablation_results %>% 
  tidyr::pivot_longer(cols = starts_with("boot_rel_iop"),
                      names_to = "Year",
                      values_to = "boot_abl_iop_rel_wtd",
                      names_prefix = "boot_rel_iop_") %>% 
  dplyr::mutate(Year = paste0("20", Year))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#  Join Original and Bootstrap Results####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Join bootstrap ablation study results to relative IOP results
boot_ablation_results_full <- iop_rel_rslt %>% 
  dplyr::left_join(orig_ablation_rslt) %>% 
  dplyr::left_join(boot_ablation_results_long) %>% 
  dplyr::mutate(
    # percentage change for original ablation
    orig_abl_pc = (1 - orig_abl_iop_rel_wtd/iop_rel_wtd),
    # percentage change for bootstrap ablation
    boot_abl_pc = (1 - boot_abl_iop_rel_wtd/iop_rel_wtd)
  )

boot_ablation_results_full_wide <- boot_ablation_results_full %>% 
  dplyr::select(Year:sector_name, ablated_var, boot, orig_abl_pc, boot_abl_pc) %>% 
  tidyr::pivot_wider(names_from = Year,
                     values_from = ends_with("_pc"))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Summarize Bootstrap Results ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Find difference in percentage change
boot_ablation_results_pc_diff <- boot_ablation_results_full_wide %>% 
  dplyr::mutate(diff_orig_pc = orig_abl_pc_2023 - orig_abl_pc_2007,
                diff_boot_pc = boot_abl_pc_2023 - boot_abl_pc_2007) 

ablation_pvalues <- boot_ablation_results_pc_diff %>% 
  dplyr::group_by(state, state_name, sector_name, ablated_var, diff_orig_pc) %>% 
  dplyr::summarise(mean_diff_boot_pc = mean(diff_boot_pc),
                   se = sd(diff_boot_pc),
                   pval = mean(abs(diff_boot_pc) >= abs(diff_orig_pc))) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(sig = dplyr::case_when(
    pval <= 0.001 ~ "***",
    pval <= 0.01 ~ "**",
    pval <= 0.05 ~ "*",
    pval <= 0.1 ~ ".",
    .default = "")
  )

# States
state_codes <- c("All", "09", "27", "10", "19", "23", "08")

# sector
sectors <- c("All", "Rural", "Urban")

# Circumstances variables
# Type_of_structure is present in nss64 but not nss-hce-23-24 data set
circum_vars <- c("HH_Size", "hh_type_code", "Religion", "Social_Group", 
                 "Land_possessed_code", "Dwelling_unit_code")#, "Type_of_structure")

ablation_pvalues <- ablation_pvalues %>% 
  dplyr::mutate(state = factor(state, levels = state_codes),
                sector_name = factor(sector_name, levels = sectors),
                ablated_var = factor(ablated_var, levels = circum_vars)) %>% 
  dplyr::arrange(state, sector_name, ablated_var) 

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Save Results ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Save the full bootstrap results to csv
boot_ablation_results_pc_diff %>% 
  readr::write_csv(file.path(outputs_dir, "boot_ablation_results_pc_diff_H0.csv"))

# Save the p-values from the bootstrap
ablation_pvalues %>% 
  readr::write_csv(file.path(outputs_dir, "boot_ablation_pvalues_H0.csv"))

# Save Results as tex file
#...............................................................................

sig_codes <- "$p\\text{-value} \\leq 0.001: {}^{***}; 
0.001 < p\\text{-value} \\leq 0.01: {}^{**}; 
0.01 < p\\text{-value} \\leq 0.05: {}^{*}; 
0.05 < p\\text{-value} \\leq 0.1: {}^{.}$"


ablation_pvalues %>% 
  dplyr::mutate(diff_orig_pc = round(diff_orig_pc * 100, digits = 2)) %>% 
  dplyr::mutate(diff_orig_pc = sprintf("%.2f", diff_orig_pc),
                sig = paste0("${}^{", sig, "}$")) %>% 
  dplyr::mutate(diff_orig_pc = paste0(diff_orig_pc, sig)) %>% 
  dplyr::select(state_name, sector_name, ablated_var, diff_orig_pc) %>% 
  # dplyr::mutate(diff_orig_pc = str_replace(diff_orig_pc, "\\.$", "$^\\.$")) %>% 
  tidyr::pivot_wider(names_from = ablated_var,
                     values_from = diff_orig_pc) %>% 
  dplyr::mutate(state_name = if_else(duplicated(state_name), "", state_name)) %>% 
  dplyr::rename_with(~ gsub("_name$", "", .x), ends_with("name")) %>% 
  dplyr::rename_with(~ gsub("_", " ", .x), contains("_")) %>%
  janitor::clean_names(case = "title", abbreviations = "HH") %>%
  knitr::kable(format = "latex", booktabs = TRUE,
               linesep = c("", "", "\\addlinespace"),
               align = c(rep("l", 2), rep("S[table-format=3.2]", 6)),
               escape = FALSE) %>% 
  kableExtra::row_spec(0, bold = TRUE) %>% 
  kableExtra::row_spec(0, extra_latex_after = colnumbering(8)) %>% 
  kableExtra::add_footnote(sig_codes, escape = FALSE, notation = "none") %>%
  kableExtra::save_kable(
    file = file.path(outputs_dir, paste0("tab_boot_ablation_pc_iop", ".tex")
  ), 
  keep_tex = TRUE,
  self_contained = TRUE)
