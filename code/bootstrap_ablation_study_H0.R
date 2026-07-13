#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Installing and Loading Required Packages ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("load_packages.R")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load Custom Functions ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("custom_functions.R")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Load and Clean Data Set ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# Directory of survey data
data_dir <- file.path("..", "data", "processed")

# Directory to save results
outputs_dir <- file.path("..", "output", "tables")

# Read data file
nss07_aggregate_df_clean <- readr::read_csv(
  file.path(data_dir,
            paste0("nss64_aggregate_df_clean", ".csv"))
)

nss23_aggregate_df_clean <- readr::read_csv(
  file.path(data_dir,
            paste0("nss_hce23_24_aggregate_df_clean", ".csv"))
)

# Combine the two surveys
## assign unique IDs for Strata and Clusters from each surveys
nss07_data <- nss07_aggregate_df_clean %>% 
  dplyr::mutate(
    # Join Dadra & Nagar Haveli and Daman & Diu as one State
    State = if_else(State == "26", "25", State),
    State_Name = if_else(State %in% c("25", "26"), 
                         "Dadra & Nagar Haveli and Daman & Diu", State_Name)
  ) %>% 
  dplyr::mutate(Year = 2007,
                Stratum_pool = paste0(State, Sector),
                FSUno = paste0("nss07_", FSUno)) 

nss23_data <- nss23_aggregate_df_clean %>% 
  dplyr::mutate(
    # Join Telangana State to Andhra Pradesh
    State = if_else(State == "36", "28", State),
    State_Name = if_else(State == "36", "Andhra Pradesh", State_Name),
    # Join Ladakh (U.T.) to Jammu and Kashmir
    State = if_else(State == "37", "01", State),
    State_Name = if_else(State == "37", "Jammu and Kashmir", State_Name)
  ) %>% 
  dplyr::mutate(Year = 2023,
                Stratum_pool = paste0(State, Sector),
                FSUno = paste0("nss23_", FSUno),
                Stratum = as.character(Stratum)) 
  

## pooled survey
nss_pooled <- dplyr::bind_rows(nss07_data, nss23_data)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Prepare for cluster bootstrap #####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# Number of bootstrap replications
n_boot <- 999

# States
state_codes <- c("All", "09", "27", "10", "19", "23", "08")

# Sectors
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
nss_pooled_df_clean <- nss_pooled %>% 
  tidyr::drop_na(all_of(c( "HH_ID", "FSUno", "State", "Stratum_ID", "HHS_No", "Sector",
                          circum_vars, outcome_var, weight_var))) %>% 
  dplyr::group_by(Year, Stratum_pool) %>% 
  dplyr::mutate(stratum_pool_weight = sum(!!sym(weight_var))) %>% 
  dplyr::select(all_of(c("Year", "Stratum_pool",
                         "HH_ID", "FSUno", "State", "State_Name", "Stratum_ID", 
                         "HHS_No", "Sector", "stratum_pool_weight",
                         circum_vars, outcome_var, weight_var))) %>% 
  dplyr::ungroup()
  

#------------------------------------------------------
# sanity: how many clusters per round × harmonized stratum?
# All Strata and Clusters
stra_clus_count <- nss_pooled_df_clean %>% 
  dplyr::distinct(Year, State, Sector, Stratum_pool, FSUno) %>% 
  dplyr::count(Year, State, Sector, Stratum_pool, name = "n_clusters") 

stra_clus_count %>% 
  dplyr::arrange(n_clusters)

# number of clusters per stratum for each year
stra_clus_count_wide <- stra_clus_count %>% 
  tidyr::pivot_wider(id_cols = c("State", "Sector", "Stratum_pool"),
                     names_from = Year,
                     values_from = n_clusters, 
                     names_prefix = "n_clusters_") 
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Bootstrapping Ablation Study ####
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Parallel setup
no_cores <- future::availableCores()
cl <- parallel::makeCluster(max(no_cores - 2, 1))
doParallel::registerDoParallel(cl)

# start timer
tictoc::tic()

boot_ablation_results <- foreach(i = seq_along(circum_vars), .combine = rbind) %:%
  foreach(j = seq_along(state_codes), .combine = rbind) %:%
  foreach(k = seq_along(sectors), .combine = rbind) %:%
  foreach(b = 1:n_boot, .combine = rbind) %dopar% {
    
    source("load_packages.R")
    source("custom_functions.R")
    # source("cov_gini.R")
    
    set.seed(1234 + i + j + k + b)
    
    state <- state_codes[j]
    sector_name <- sectors[k]

    dt <- nss_pooled_df_clean %>% 
      dplyr::left_join(stra_clus_count_wide) 
    
    
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
    
    # Bootstrap sample for 2007
    dt_2007 <- dt %>% 
      split(~ Stratum_pool) %>% 
      lapply(FUN = function(x){
        dplyr::slice_sample(x, n = unique(x$n_clusters_2007), replace = TRUE)
      }) %>% 
      dplyr::bind_rows() %>% 
      dplyr::group_by(Stratum_pool) %>% 
      dplyr::mutate(boot_weight = !!sym(weight_var) * stratum_pool_weight/sum(!!sym(weight_var))) %>% 
      dplyr::ungroup()
      
        
    # Bootstrap sample for 2023
    dt_2023 <- dt %>% 
      split(~ Stratum_pool) %>% 
      lapply(FUN = function(x){
        dplyr::slice_sample(x, n = unique(x$n_clusters_2023), replace = TRUE)
      }) %>% 
      dplyr::bind_rows() %>% 
      dplyr::group_by(Stratum_pool) %>% 
      dplyr::mutate(boot_weight = !!sym(weight_var) * stratum_pool_weight/sum(!!sym(weight_var))) %>% 
      dplyr::ungroup()
    
    
    
    
    # Relative IOP for Complex Household Survey
    out_wtd_2007 <- dt_2007 |> 
      iop_rel(x = sym(outcome_var),
              stratum = Stratum_ID,
              cluster = FSUno,
              circumstances = circum_vars[-i],
              weight = boot_weight,
              distribution = "smoothed",
              variance = FALSE,
              var.decompose = FALSE,
              data = _)
    
    out_wtd_2023 <- dt_2023 |> 
      iop_rel(x = sym(outcome_var),
              stratum = Stratum_ID,
              cluster = FSUno,
              circumstances = circum_vars[-i],
              weight = boot_weight,
              distribution = "smoothed",
              variance = FALSE,
              var.decompose = FALSE,
              data = _)
    
    ans <- data.frame(ablated_var = circum_vars[i],
                      state, 
                      state_name, 
                      sector_name,
                      boot = b,
                      # iop_total_wtd = out_wtd$total_iop,
                      # iop_abs_wtd = out_wtd$abs_iop,
                      boot_rel_iop_07 = out_wtd_2007$rel_iop,
                      boot_rel_iop_23 = out_wtd_2023$rel_iop
    )
    return(ans)
  }

# End and release all clusters
parallel::stopCluster(cl)

# end timer
tictoc::toc()

# Save ablation study results to csv file
readr::write_csv(boot_ablation_results, 
                 file.path(outputs_dir, paste0("boot_ablation_results_H0", ".csv")))

