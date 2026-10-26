# armed_conflict

Reproducing and replicating the analysis in "Implications of armed conflict for maternal and child health: A regression analysis of data from 181 countries for 2000-2019" (Jawad et al., PLOS Medicine, 2021).

Course project for CHL5233, Fall 2026.

## Structure

- `data/raw/`: raw data files as downloaded
- `data/processed/`: the merged analytical data set
- `R/`: R scripts that build the analytical data set
- `reports/`: Quarto documents and rendered output

## Building the data

`R/create_final_data.R` reshapes the four World Bank mortality files with a single reusable function, builds the earthquake and drought indicators from the disaster data, derives the binary armed conflict exposure and its one-year lag, merges in the covariates, and writes `data/processed/final_data.csv` (186 countries, 2000 to 2019, 3720 rows).

`R/create_final_data_agent.R` builds the same data set from the same specification using base R only, and writes `data/processed/final_data_agent.csv`. It exists so the two implementations can be compared against each other.

Both scripts run from a clean R session and only need the project to be the working directory.

## Reports

- `reports/quarto_inclass.qmd`: week 2 Quarto assignment, country and region counts
- `reports/compare_final_data.qmd`: compares the two builds of the analytical data set cell by cell, documents the judgment calls where they could have diverged, and reconciles the exposure count against the published paper
- `reports/eda.qmd`: exploratory analysis of the panel, covering missingness, the distribution and time trend of the four mortality outcomes, how common the conflict exposure is, and the unadjusted association between the two
