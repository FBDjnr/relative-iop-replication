#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Updates ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# v3: 2025-10-24
## Updated filenaming to include a version version variable

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Select Parameters of Interest ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# States
state_codes <- c("All", "09", "27", "10", "19", "23", "08")

sectors <- c("All", "Rural", "Urban")

# Economic outcome variables
outcome_var <- "Total_Exp"

# Weight variable
weight_var <- "Multiplier"

# Circumstances variables
# Type_of_structure is present in nss64 but not nss-hce-23-24 data set
circum_vars <- c("HH_Size", "hh_type_code", "Religion", "Social_Group", 
                 "Land_possessed_code", "Dwelling_unit_code")#, "Type_of_structure")



# Remove missing values
nss_df_clean <- nss_df %>% 
  tidyr::drop_na(all_of(c("HH_ID", "FSUno", "State", "Stratum_ID", "HHS_No", "Sector",
                          circum_vars, outcome_var, weight_var)))
  
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Summary Statistics ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

## Descriptive Statistics for Circumstance Variables ####
nss_df_clean %>% 
  dplyr::count(hh_type_code) %>% 
  dplyr::mutate(precentage = round(n/sum(n) * 100, 2))
  

nss_circum_desc <- map(circum_vars[-1], 
    \(x) nss_df_clean %>% 
      dplyr::count(.data[[x]]) %>% 
      dplyr::mutate(percentage = round(n/sum(n) * 100, 4),
                    variable = x) %>% 
      dplyr::rename(category = 1) %>% 
      dplyr::select(variable, category, n, percentage)
    ) %>% 
  purrr::list_rbind()

readr::write_csv(nss_circum_desc, 
                 file.path(outputs_dir, paste0(survey_name, "_circum_summary", ".csv"))
                 )


#...............................................................................
## By Sector ####

nss_sector_summary <- nss_df_clean %>% 
  dplyr::group_by(State, State_Name, Sector) %>%
  dplyr::summarise(n = n(),
                   avg = mean(!!sym(outcome_var)),
                   avg_wtd = weighted.mean(!!sym(outcome_var), w = !!sym(weight_var)),
                   sd = sd(!!sym(outcome_var))
  ) %>% 
  dplyr::ungroup() %>% 
  pivot_wider(names_from = Sector,
              values_from = c(n, avg, avg_wtd, sd)) 

# Save results to csv file
readr::write_csv(nss_sector_summary, 
                 file.path(outputs_dir, paste0(survey_name, "_sector_summary", ".csv"))
)

nss_sector_summary %>% 
  dplyr::arrange(desc(n_Rural + n_Urban))

# Save results to latex file
nss_sector_summary %>%  
  dplyr::select(!State) %>% 
  dplyr::arrange(desc(n_Rural + n_Urban)) %>% 
  knitr::kable(format = "latex", booktabs = TRUE, digits = 2,
               col.names = c("State", rep(c("Rural", "Urban"), 4))
               ) %>% 
  kableExtra::row_spec(0, bold = TRUE) %>% 
  # kableExtra::row_spec(0, extra_latex_after = colnumbering(ncol(nss_sector_summary))) %>% 
  kableExtra::add_header_above(header = c(" " = 1, "n" = 2, "average" = 2, 
                                 "weighted average" = 2, "sd" = 2,
                                 bold = TRUE)) %>% 
  kableExtra::save_kable(file = file.path(outputs_dir, 
                                          paste0("tab_", survey_name, "_sector_summary", ".tex")
                                          ), 
                         keep_tex = TRUE,
                         self_contained = TRUE)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Standardized Distribution ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

summary_stats_results <- foreach::foreach(state = state_codes, .combine = rbind) %:%
  foreach::foreach(sector_name = sectors, .combine = rbind) %do% {
    
    source("load_packages.R")
    source("custom_functions.R")
    
    dt <- nss_df_clean
    
    if(state != "All"){
      dt <- dt |>
        dplyr::filter(State == state)
      
      state_name <- unique(dt$State_Name)
    }else{
      state_name <- "All States"
    }
    
    if(sector_name != "All"){
      dt <- dt |>
        dplyr::filter(Sector == sector_name)
    }
    
    out <- dt |>
      # dplyr::group_by(religion) |>
      dplyr::summarise(n = n(),
                       avg = mean(!!sym(outcome_var)),
                       avg_wtd = weighted.mean(!!sym(outcome_var), w = !!sym(weight_var)),
                       sd = sd(!!sym(outcome_var))
      ) 
    
    ans <- out |>
      dplyr::mutate(state, state_name, sector_name) |>
      dplyr::relocate(state, state_name, sector_name, .before = n)
    
    return(ans)
  }

summary_stats_results

# Save results to csv file
readr::write_csv(summary_stats_results, 
                 file.path(outputs_dir,
                           paste0(survey_name, "_summary_stats_results", ".csv"))
                 )

# Save results to latex file
summary_stats_results %>% 
  dplyr::select(!state) %>% 
  dplyr::mutate(state_name = if_else(duplicated(state_name), "", state_name)) %>% 
  knitr::kable(format = "latex", booktabs = TRUE, digits = 2,
               col.names = c("State", "Sector", "n", "Avg.", "Wtd. Avg.", "Std. Dev."),
               linesep = c("", "", "\\addlinespace")
  ) %>% 
  kableExtra::row_spec(0, bold = TRUE) %>% 
  kableExtra::save_kable(file = file.path(outputs_dir, 
                                          paste0("tab_", survey_name,"_summary_stats_results", ".tex")), 
                         keep_tex = TRUE,
                         self_contained = TRUE)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Gini Index ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

gini_results <- foreach::foreach(state = state_codes, .combine = rbind) %:%
  foreach::foreach(sector_name = sectors, .combine = rbind) %do% {
    
    source("load_packages.R")
    source("custom_functions.R")
    
    dt <- nss_df_clean
    
    if(state != "All"){
      dt <- dt |>
        dplyr::filter(State == state)
      
      state_name <- unique(dt$State_Name)
      
    }else{
      state_name <- "All States"
    }
    
    
    if(sector_name != "All"){
      dt <- dt |>
        dplyr::filter(Sector == sector_name)
    }
    
    
    # Gini index from Simple Random Sample
    out_naive <- dt |> 
      gini_index(x = sym(outcome_var), 
                 variance = FALSE,
                 var.decompose = FALSE,
                 data = _) 
    
    # Gini index from a complex household survey
    out_wtd <- dt |> 
      gini_index(x = sym(outcome_var), 
                 stratum = Stratum_ID, 
                 cluster = FSUno, 
                 weight = sym(weight_var),
                 variance = TRUE,
                 var.decompose = TRUE,
                 data = _) 
    
    ans <- data.frame(state,
                      state_name,
                      sector_name, 
                      gini_naive = out_naive,
                      gini_wtd = out_wtd$est, 
                      var = out_wtd$var,
                      naive = out_wtd$var.decompose["naive"],
                      cluster = out_wtd$var.decompose["cluster"],
                      stratum = out_wtd$var.decompose["stratum"]
    )
    
    rownames(ans) <- NULL
    
    return(ans)
  }

gini_results

# Save results to csv file
readr::write_csv(gini_results, 
                 file.path(outputs_dir, paste0(survey_name, "_gini_results", ".csv"))
                 )

#...............................................................................
## Summarize results ####
gini_results_summary <- gini_results |> 
  dplyr::mutate(se_correct = sqrt(var), se_naive = sqrt(naive)) |> 
  # percentage change in naive se
  dplyr::mutate(cluster_impact = sqrt(naive + cluster)/se_naive - 1,
                stratum_impact = sqrt(naive + stratum)/se_naive - 1,
                total_impact = se_correct/se_naive - 1
  )

#...............................................................................
## Gini: Table 1: Gini Coefficient (naive vs weighted) ####
gini_naive_vs_wtd <- gini_results_summary |> 
  dplyr::select(state_name, sector_name, gini_naive, gini_wtd) |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, digits = 4))) 

gini_naive_vs_wtd

# Save results to csv file
readr::write_csv(gini_naive_vs_wtd, 
                 file.path(outputs_dir, paste0(survey_name, "_gini_naive_vs_wtd", ".csv"))
                 )

# Save results to latex file
gini_naive_vs_wtd %>%  
  dplyr::mutate(state_name = if_else(duplicated(state_name), "", state_name)) %>% 
  knitr::kable(format = "latex", booktabs = TRUE,
               col.names = c("State", "Sector", "Gini: naive" , "Gini: weighted"),
               linesep = c("", "", "\\addlinespace")) %>% 
  kableExtra::row_spec(0, bold = TRUE) %>% 
  kableExtra::save_kable(file = file.path(outputs_dir, 
                                          paste0("tab_", survey_name, "_gini_naive_vs_wtd", ".tex")
                                          ), 
                         keep_tex = TRUE,
                         self_contained = TRUE)

#...............................................................................
## Gini: Table 2: Design Effect on Standard Errors ####
gini_design_effect <- gini_results_summary |> 
  dplyr::select(state_name, sector_name, gini_wtd,
                se_correct, se_naive, stratum_impact, cluster_impact, total_impact) |>
  dplyr::mutate(dplyr::across(gini_wtd:se_naive, ~round(.x, digits = 4)),
                dplyr::across(tidyselect::ends_with("impact"), ~round(.x*100, digits = 2))
  )

# Save results to latex file
gini_design_effect %>%  
  dplyr::mutate(state_name = if_else(duplicated(state_name), "", state_name)) %>% 
  knitr::kable(format = "latex", booktabs = TRUE,
               col.names = c("State", "Sector", "(wtd)", "Correct", "Naive", "Stratum", "Cluster", "Total"),
               linesep = c("", "", "\\addlinespace")) %>% 
  kableExtra::row_spec(0, bold = TRUE) %>% 
  kableExtra::add_header_above(header = c(" " = 2, "Gini" = 1, "Standard Error" = 2, "Structure Effect (%)" = 3),
                               bold = TRUE) %>% 
  kableExtra::save_kable(file = file.path(outputs_dir, 
                                          paste0("tab_", survey_name, "_gini_design_effect", ".tex")
                                          ), 
                         keep_tex = TRUE,
                         self_contained = TRUE)

# Save results to csv file
readr::write_csv(gini_design_effect, 
                 file.path(outputs_dir, paste0(survey_name, "_gini_design_effect", ".csv"))
                 )

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Relative IOP: Smoothed Distribution ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#...............................................................................
## Relative IOP Using Complex Household Survey Structure  ####
#...............................................................................

iop_results <- foreach(state = state_codes, .combine = rbind) %:%
  foreach(sector_name = sectors, .combine = rbind) %do% {
    
    source("load_packages.R")
    source("custom_functions.R")
    # source("cov_gini.R")
    
    dt <- nss_df_clean
    
    if(state != "All"){
      dt <- dt |>
        dplyr::filter(State == state)
      
      state_name <- unique(dt$State_Name)
      
    }else{
      state_name <- "All States"
    }
    
    if(sector_name != "All"){
      dt <- dt |>
        dplyr::filter(Sector == sector_name)
    }
    
    # Relative IOP for SRS
    out_naive <- dt |> 
      iop_rel(x = sym(outcome_var), 
              circumstances = circum_vars, 
              distribution = "smoothed",
              variance = FALSE,
              var.decompose = FALSE,
              data = _) 
    
    # Relative IOP for Complex Household Survey
    out_wtd <- dt |> 
      iop_rel(x = sym(outcome_var),
              stratum = Stratum_ID,
              cluster = FSUno,
              circumstances = circum_vars,
              weight = sym(weight_var),
              distribution = "smoothed",
              variance = TRUE,
              var.decompose = TRUE,
              data = _)
    
    ans <- data.frame(state, 
                      state_name, 
                      sector_name,
                      iop_total_naive = out_naive$total_iop,
                      iop_abs_naive = out_naive$abs_iop,
                      iop_rel_naive = out_naive$rel_iop,
                      iop_total_wtd = out_wtd["total_iop", "est"],
                      iop_abs_wtd = out_wtd["abs_iop", "est"],
                      iop_rel_wtd = out_wtd["rel_iop", "est"],
                      var = out_wtd["rel_iop", "var"],
                      var_naive = out_wtd["rel_iop", "var.naive"],
                      var_stratum = out_wtd["rel_iop", "var.stratum"],
                      var_cluster = out_wtd["rel_iop", "var.cluster"]
    )
    
    return(ans)
  }

iop_results

# Save results to csv file
readr::write_csv(iop_results, 
                 file.path(outputs_dir, paste0(survey_name, "_iop_results", ".csv")))

#...............................................................................
## Summarize results ####
#...............................................................................

iop_results_summary <- iop_results |> 
  dplyr::mutate(se_correct = sqrt(var), se_naive = sqrt(var_naive)) |> 
  # percentage change in naive se
  dplyr::mutate(cluster_impact = sqrt(var_naive + var_cluster)/se_naive - 1,
                stratum_impact = sqrt(var_naive + var_stratum)/se_naive - 1,
                total_impact = se_correct/se_naive - 1
  )

#...............................................................................
## Naive vs Weighted Relative IOP ####
#...............................................................................

iop_naive_vs_wtd <- iop_results_summary |> 
  dplyr::select(state_name, sector_name, tidyselect::matches("^iop.*(naive|wtd)$")) |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, digits = 4))) 

iop_naive_vs_wtd

# Save results to csv file
readr::write_csv(iop_naive_vs_wtd, 
                 file.path(outputs_dir, paste0(survey_name, "_iop_naive_vs_wtd", ".csv"))
                 )

# Save results to latex file
iop_naive_vs_wtd %>%  
  dplyr::mutate(state_name = if_else(duplicated(state_name), "", state_name)) %>% 
  knitr::kable(format = "latex", booktabs = TRUE,
               col.names = c("State", "Sector", 
                             "Total Ineq", "Abs IOP", "Rel IOP", 
                             "Total Ineq", "Abs IOP", "Rel IOP"),
               linesep = c("", "", "\\addlinespace")) %>% 
  kableExtra::row_spec(0, bold = TRUE) %>%
  kableExtra::row_spec(0, extra_latex_after = colnumbering(ncol(iop_naive_vs_wtd))) %>% 
  kableExtra::add_header_above(header = c(" " = 2, "Naive" = 3, "Weighted" = 3),
                               bold = TRUE) %>% 
  kableExtra::save_kable(file = file.path(outputs_dir, 
                                          paste0("tab_", survey_name, "_iop_naive_vs_wtd", ".tex")
                                          ), 
                         keep_tex = TRUE,
                         self_contained = TRUE)


#...............................................................................
## Design Effect on Standard Errors for Relative IOP####
#...............................................................................

iop_design_effect <- iop_results_summary |> 
  dplyr::select(state_name, sector_name, iop_rel_wtd,
                se_correct, se_naive, stratum_impact, cluster_impact, total_impact) |>
  dplyr::mutate(dplyr::across(iop_rel_wtd:se_naive, ~round(.x, digits = 4)),
                dplyr::across(dplyr::ends_with("impact"), ~round(.x*100, digits = 2))
  )

iop_design_effect

# Save results to latex file
iop_design_effect %>% 
  dplyr::mutate(state_name = if_else(duplicated(state_name), "", state_name)) %>% 
  knitr::kable(format = "latex", booktabs = TRUE,
               col.names = c("State", "Sector", "(wtd)", "Correct", "Naive", "Stratum", "Cluster", "Total"),
               linesep = c("", "", "\\addlinespace")) %>% 
  kableExtra::row_spec(0, bold = TRUE) %>% 
  kableExtra::row_spec(0, extra_latex_after = colnumbering(ncol(iop_design_effect))) %>% 
  kableExtra::add_header_above(header = c(" " = 2, "Rel IOP" = 1, "Standard Error" = 2, "Structure Effect (%)" = 3),
                               bold = TRUE) %>% 
  kableExtra::save_kable(file = file.path(outputs_dir, 
                                          paste0("tab_", survey_name, "_iop_design_effect", ".tex")
                                          ), 
                         keep_tex = TRUE,
                         self_contained = TRUE)

# Save results to csv file
readr::write_csv(iop_design_effect,
                 file.path(outputs_dir, paste0(survey_name, "_iop_design_effect", ".csv"))
                 )


#...............................................................................
## Ablation Study ####
#...............................................................................

ablation_results <- foreach(i = seq_along(circum_vars), .combine = rbind) %:%
  foreach(state = state_codes, .combine = rbind) %:%
  foreach(sector_name = sectors, .combine = rbind) %do% {
    
    source("load_packages.R")
    source("custom_functions.R")
    # source("cov_gini.R")
    
    dt <- nss_df_clean
    
    if(state != "All"){
      dt <- dt |>
        dplyr::filter(State == state)
      
      state_name <- unique(dt$State_Name)
      
    }else{
      state_name <- "All States"
    }
    
    if(sector_name != "All"){
      dt <- dt |>
        dplyr::filter(Sector == sector_name)
    }
    
    # Relative IOP for SRS
    out_naive <- dt |> 
      iop_rel(x = sym(outcome_var), 
              circumstances = circum_vars[-i], 
              distribution = "smoothed",
              variance = FALSE,
              var.decompose = FALSE,
              data = _) 
    
    # Relative IOP for Complex Household Survey
    out_wtd <- dt |> 
      iop_rel(x = sym(outcome_var),
              stratum = Stratum_ID,
              cluster = FSUno,
              circumstances = circum_vars[-i],
              weight = sym(weight_var),
              distribution = "smoothed",
              variance = TRUE,
              var.decompose = TRUE,
              data = _)
    
    ans <- data.frame(ablation = circum_vars[i],
                      state, 
                      state_name, 
                      sector_name,
                      iop_total_naive = out_naive$total_iop,
                      iop_abs_naive = out_naive$abs_iop,
                      iop_rel_naive = out_naive$rel_iop,
                      iop_total_wtd = out_wtd["total_iop", "est"],
                      iop_abs_wtd = out_wtd["abs_iop", "est"],
                      iop_rel_wtd = out_wtd["rel_iop", "est"],
                      var = out_wtd["rel_iop", "var"],
                      var_naive = out_wtd["rel_iop", "var.naive"],
                      var_stratum = out_wtd["rel_iop", "var.stratum"],
                      var_cluster = out_wtd["rel_iop", "var.cluster"]
    )
    
    return(ans)
  }

# Join ablation study results to relative IOP results
ablation_results_full <- iop_results %>% 
  dplyr::mutate(ablation = "none") %>% 
  dplyr::relocate(ablation) %>% 
  dplyr::bind_rows(ablation_results) 


# Save ablation study results to csv file
readr::write_csv(ablation_results_full, 
                 file.path(outputs_dir, paste0(survey_name, "_ablation_results", ".csv")))

# Converting ablation study results from long to wide
ablation_results_full_wide <- ablation_results_full %>% 
  dplyr::select(ablation, state, state_name, sector_name, iop_total_wtd, iop_abs_wtd, iop_rel_wtd) %>% 
  tidyr::pivot_wider(names_from = ablation,
                     values_from = c(iop_abs_wtd, iop_rel_wtd)) 


# Finding the difference between the result for each circumstance ablation and the original results

ablation_results_full_wide_diff <- ablation_results_full_wide %>% 
  mutate(
    # diff for ablation absolute iop columns
    across(dplyr::starts_with("iop_abs_wtd"), 
           ~ iop_abs_wtd_none - .x, 
           .names = "diff_{.col}"), 
    # diff for ablation relative iop columns
    across(dplyr::starts_with("iop_rel_wtd"), 
           ~ iop_rel_wtd_none - .x, 
           .names = "diff_{.col}"),
    # percentage change for ablation absolute iop columns
    across(dplyr::starts_with("iop_abs_wtd"), 
           ~ ((iop_abs_wtd_none - .x)/iop_abs_wtd_none)*100, 
           .names = "pc_{.col}"), 
    # percentage change for ablation relative iop columns
    across(dplyr::starts_with("iop_rel_wtd"), 
           ~ ((iop_rel_wtd_none - .x)/iop_rel_wtd_none)*100, 
           .names = "pc_{.col}")
  )


# Save ablation study results with differences calculated to csv file
readr::write_csv(ablation_results_full_wide_diff, 
                 file.path(outputs_dir, paste0(survey_name, "_ablation_results_with_diff", ".csv")))


# Save results of percentage change in IOP to latex file
ablation_results_full_wide_diff %>% 
  dplyr::select(state_name, sector_name, matches("^pc_iop_abs")) %>% 
  dplyr::select(!ends_with("none")) %>% 
  # find percentage change in relative IOP
  dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))) %>% 
  dplyr::mutate(state_name = if_else(duplicated(state_name), "", state_name)) %>%
  dplyr::rename_with(~ gsub("^pc_iop_abs_wtd_", "", .x), starts_with("pc_")) %>%
  dplyr::rename_with(~ gsub("_name$", "", .x), ends_with("name")) %>% 
  dplyr::rename_with(~ gsub("_", " ", .x), contains("_")) %>% 
  janitor::clean_names(case = "title", abbreviations = "HH") %>% 
  knitr::kable(format = "latex", booktabs = TRUE,
               linesep = c("", "", "\\addlinespace")) %>% 
  kableExtra::row_spec(0, bold = TRUE) %>% 
  kableExtra::row_spec(0, extra_latex_after = colnumbering(8)) %>% 
  kableExtra::save_kable(file = file.path(outputs_dir, 
                                          paste0("tab_", survey_name, "_ablation_pc_iop", ".tex")
  ), 
  keep_tex = TRUE,
  self_contained = TRUE)


