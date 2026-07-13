# Data

The empirical application uses **licensed unit-level microdata** from India's
National Sample Survey (NSS). These files **cannot be redistributed**, so they
are not included in this repository (the whole `data/` tree except this README
and the folder placeholders is git-ignored). This file explains how to obtain
the raw data and regenerate every derived file the analysis needs.

## Data sources

| Survey | Round / year | Source |
|--------|--------------|--------|
| NSS 64th Round — Consumer Expenditure | 2007–2008 | https://microdata.gov.in/NADA/index.php/catalog/116 |
| NSS Household Consumption Expenditure Survey (HCES) | 2023–2024 | https://microdata.gov.in/NADA/index.php/catalog/237 |

Access requires free registration on the MoSPI microdata portal and acceptance
of the data-use agreement ("Rider for users of unit level data"). The HCES 2023–24
microdata are distributed as a Nesstar (`.Nesstar`) package; export the level
files to Stata `.dta` using the Nesstar Explorer.

## Expected folder layout

Place the downloaded files as follows (create the folders if needed):

```
data/
  raw/
    nss64/                     # NSS 64th round
      Household Characteristics.dta
      Demographic and other particulars of household members.dta
      Consumption of clothing, bedding and footwear during the last 365 days.dta
      Consumption of food, pan, tobacco , intoxicants and fuel during  the last 30 days.dta
      Expenditure for purchase and construction.dta
      Expenditure on education, medical goods and services.dta
      State Codes and Names.csv
    nss_hce23_24/              # NSS-HCES 2023-24 (Stata exports of the level files)
      LEVEL - 02 (Section 3).dta
      LEVEL - 03.dta
      LEVEL - 14 (Section  A1,B1 & C1).dta
      State Codes and Names.csv
      item_codes_for_consumption_and_expenditure.csv
  processed/                   # created by the cleaning scripts (see below)
```

The two small lookup tables — `State Codes and Names.csv` (state code ↔ name)
and `item_codes_for_consumption_and_expenditure.csv` (HCES item classification) —
**are included in this repository** under `data/raw/`, so you only need to add
the licensed level files. Everything else in `data/` is git-ignored.

## Regenerating the derived data

From the `code/` directory, run the cleaning/aggregation scripts. They read the
raw level files above and write the household-level analysis files into
`data/processed/`:

```r
source("Clean and Aggregate NSS64 Data.R")          # -> data/processed/nss64_aggregate_df.csv (+ .dta)
source("Clean and Aggregate NSS-HCES23-24 Data.R")  # -> data/processed/nss_hce23_24_aggregate_df.csv (+ .dta)
```

The empirical-application scripts (`Application to NSS64.R`,
`Application to NSS-HCES23-24.R`) then read the files in `data/processed/` and
write the manuscript tables into `output/tables/`. See the top-level `README.md`
for the full replication order.
