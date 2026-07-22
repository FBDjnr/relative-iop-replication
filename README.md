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
  sync_manuscript_assets.R  Copies output/ tables & figures into manuscript/
data/                 Data folder skeleton (contents git-ignored; see data/README.md)
  raw/                Licensed NSS microdata goes here (not distributed)
  processed/          Derived household-level aggregates (created by the code)
output/               Assets produced by the code (self-contained for code/)
  tables/             Generated LaTeX tables + their result CSVs
  figures/            Generated figures
manuscript/           Self-contained manuscript (compiles on its own)
  tables/             Duplicate of output/tables/*.tex, \input by the .tex
  figures/            Duplicate of output/figures/*
  *.tex, *.bib, *.bst, *.pdf, latexmkrc
```

The `manuscript/` folder is **self-contained**: it carries its own copies of the
LaTeX tables and figures, so it can be zipped and compiled anywhere without the
rest of the repository. Those copies are duplicates of what the code writes to
`output/`; `code/sync_manuscript_assets.R` refreshes them (it only copies into
`manuscript/`, and never modifies the analysis scripts or `output/`).

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
source("clean_and_aggregate_nss64_data.R")
source("clean_and_aggregate_nss_hces23_24_data.R")

# 2. Main empirical application (each sources "application_of_gini_and_iop.R",
#    which writes the per-survey tables + intermediate CSVs to output/tables/)
source("application_to_nss64.R")
source("application_to_nss_hces23_24.R")

# 3. Circumstance distribution, hypothesis tests, bootstrap ablation
source("circumstance_variable_desc.R")
source("hypothesis_testing.R")
source("bootstrap_ablation_study_h0.R")
source("bootstrap_ablation_study_h0_pvalues_ci.R")

# 4. Refresh the manuscript's own copies of the tables/figures
source("sync_manuscript_assets.R")
```

Then build the documents (from `manuscript/`). The folder is self-contained, so
this works even on a standalone copy of `manuscript/`:

```bash
latexmk -pdf JASA_Supplement.tex          # compile first (cross-references)
latexmk -pdf JASA_IOP_Complex_Survey.tex
```

### Which script produces which table

| Manuscript table(s) | Produced by |
|---|---|
| `tab_circumstance_variables` | *static* (hand-authored variable definitions) |
| `tab_circumstance_variable_desc` | `circumstance_variable_desc.R` |
| `tab_{nss64,nss_hce23_24}_summary_stats_results` | `application_of_gini_and_iop.R` |
| `tab_{nss64,nss_hce23_24}_iop_naive_vs_wtd` | `application_of_gini_and_iop.R` |
| `tab_{nss64,nss_hce23_24}_iop_design_effect` | `application_of_gini_and_iop.R` |
| `tab_{nss64,nss_hce23_24}_ablation_pc_iop` | `application_of_gini_and_iop.R` |
| `tab_hyp_test_results_time`, `tab_hyp_test_results_sector` | `hypothesis_testing.R` |
| `tab_boot_ablation_pc_iop` | `bootstrap_ablation_study_h0*.R` |
| Supplement simulation tables | `code/simulation/` |

## Simulation study

`code/simulation/` contains the finite-sample simulation behind the supplement's
three tables. Each driver `iop_sim_pop1_nc<NC>.R` runs 5,000 replications on the
fixed synthetic population `synpop1.csv` for a given number of clusters per
stratum `NC`, and writes the per-configuration result CSVs:

- `tab1_consistency_nc<NC>.csv` — finite-sample performance / design effect
- `tab2_coverage_nc<NC>.csv` — Wald CI coverage and mean width
- `tab3_type1_power_nc<NC>.csv`, `tab3_power_detail_nc<NC>.csv` — two-sample Wald test rejection rates

Only the four configurations reported in the supplement are kept:
`NC = 50, 75, 100, 500` (total clusters 100, 150, 200, 1000). The supplement
tables report these CSVs' values; results reproduce up to Monte-Carlo noise.
