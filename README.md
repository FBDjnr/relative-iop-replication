# Relative IOP — Replication Materials

Replication code and manuscript sources for *Inequality of Opportunity
Inference under a Complex Household Survey Framework*. The paper develops
estimation and inference for inequality of opportunity (IOP) and the Gini index
under complex survey designs (stratification, clustering, unequal selection
probabilities), with analytical (influence-function) and bootstrap variance
methods, and applies them to two rounds of India's National Sample Survey.

## Repository layout

```
code/                 R scripts for the empirical application
  simulation/         Simulation study (HPC artifacts) behind the supplement
data/                 Data folder skeleton (contents git-ignored; see data/README.md)
  raw/                Licensed NSS microdata goes here (not distributed)
  processed/          Derived household-level aggregates (created by the code)
output/
  tables/             Generated LaTeX tables (\input by the manuscript) + result CSVs
  figures/            Generated figures
manuscript/           Manuscript + supplement (LaTeX sources, .bib, .bst, PDFs)
```

## Data

The empirical application uses **licensed NSS microdata that cannot be
redistributed**, so no data are included in this repository. See
[`data/README.md`](data/README.md) for the data sources, how to obtain the raw
files, and how to regenerate the derived aggregates.

| Survey | Round / year | Source |
|--------|--------------|--------|
| NSS 64th Round — Consumer Expenditure | 2007–2008 | https://microdata.gov.in/NADA/index.php/catalog/116 |
| NSS Household Consumption Expenditure Survey | 2023–2024 | https://microdata.gov.in/NADA/index.php/catalog/237 |

## Requirements

- R (≥ 4.1). Packages are installed/loaded on demand by
  [`code/load_packages.R`](code/load_packages.R) (via **pacman**).
- The core estimators also exist as a standalone R package, **iopr**; the
  scripts here use the equivalent functions in `code/custom_functions.R`.
- A LaTeX distribution (e.g. TeX Live) with `latexmk` to build the manuscript.

## Reproducing the results

Run the R scripts **from the `code/` directory** (paths are relative to it).
After placing the raw data (see `data/README.md`):

```r
# 1. Build the derived household-level data  ->  data/processed/
source("Clean and Aggregate NSS64 Data.R")
source("Clean and Aggregate NSS-HCES23-24 Data.R")

# 2. Main empirical application (each sources "Application of Gini and IOP.R",
#    which writes the per-survey tables + intermediate CSVs to output/tables/)
source("Application to NSS64.R")
source("Application to NSS-HCES23-24.R")

# 3. Circumstance distribution, hypothesis tests, bootstrap ablation
source("circumstance_variable_desc.R")
source("hypothesis_testing.R")
source("bootstrap_ablation_study_H0.R")
source("bootstrap_ablation_study_H0_pvalues_ci.R")
```

Then build the documents (from `manuscript/`):

```bash
latexmk -pdf JASA_Supplement.tex          # compile first (cross-references)
latexmk -pdf JASA_IOP_Complex_Survey.tex
```

### Which script produces which table

| Manuscript table(s) | Produced by |
|---|---|
| `tab_circumstance_variables` | *static* (hand-authored variable definitions) |
| `tab_circumstance_variable_desc` | `circumstance_variable_desc.R` |
| `tab_{nss64,nss_hce23_24}_summary_stats_results` | `Application of Gini and IOP.R` |
| `tab_{nss64,nss_hce23_24}_iop_naive_vs_wtd` | `Application of Gini and IOP.R` |
| `tab_{nss64,nss_hce23_24}_iop_design_effect` | `Application of Gini and IOP.R` |
| `tab_{nss64,nss_hce23_24}_ablation_pc_iop` | `Application of Gini and IOP.R` |
| `tab_hyp_test_results_time`, `tab_hyp_test_results_sector` | `hypothesis_testing.R` |
| `tab_boot_ablation_pc_iop` | `bootstrap_ablation_study_H0*.R` |
| Supplement simulation tables/figures | `code/simulation/` |

## Simulation study

`code/simulation/` contains the finite-sample simulation used in the supplement,
including the HPC (SLURM array) driver scripts, the per-configuration outputs
(`*_nc<clusters>` = number of clusters per stratum), the combined summary CSVs,
and the plotting scripts (`plot_tab*.R`). The full study is designed to run on a
cluster; the tables and figures can be regenerated from the combined summary
CSVs with the plotting scripts without re-running every replication.
