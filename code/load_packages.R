# Install "pacman", needed for loading all packages
if (!require(pacman)) {
  install.packages("pacman", repos = "http://cran.r-project.org")
}

# List of CRAN packages
cran_pkgs <- c("MASS", "matrixStats"
               , "doParallel" # Foreach Parallel Adaptor for the 'parallel' Package
               , "future"     # Unified Parallel and Distributed Processing in R
               # , "sf"         # Simple Features for R
               , "tidyverse"  # R packages for data science
               , "janitor"    # Simple Tools for Examining and Cleaning Dirty Data
               , "remotes"    # R Package Installation from Remote Repositories, Including 'GitHub'
               , "haven"      # Import and Export 'SPSS', 'Stata' and 'SAS' Files
               # , "spatstat.univar"   # One-Dimensional Probability Distribution Support for the 'spatstat'
               , "EnvStats"   # Environmental Statistics, Including US EPA Guidance (mainly for Pareto Distribution),
               , "wakefield"  # Generate Random Data Sets
               # , "reldist"    # Relative distribution methods (for gini() function)
               , "tictoc"     # Functions for Timing R Scripts
               , "ggdist"    # For weighted_cdf
               , "knitr"    # A General-Purpose Package for Dynamic Report Generation in R
               , "kableExtra" # Construct Complex Table with 'kable' and Pipe Syntax
               , "reldist"   # Relative Distribution Methods
)


# List of Github packages
github_pkgs <- c()

# Install/update and load CRAN packages
pacman::p_load(char = cran_pkgs, install = TRUE, update = FALSE, character.only = TRUE)
pacman::p_load_gh(char = github_pkgs, install = TRUE, update = TRUE)

#===============================================================================
