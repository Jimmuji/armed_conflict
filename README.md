# armed_conflict

Reproducing and replicating the analysis in "Implications of armed conflict for maternal and child health: A regression analysis of data from 181 countries for 2000-2019" (Jawad et al., PLOS Medicine, 2021).

Course project for CHL5233, Fall 2026.

## Structure

- `data/raw/`: raw data files as downloaded
- `data/processed/`: cleaned and merged data
- `scripts/`: R scripts for importing, cleaning, and analysing the data
- `outputs/`: figures and tables
- `reports/`: Quarto documents and rendered output

## Week 3: creating the analytical data set

`scripts/create_final_data.R` reshapes the four World Bank mortality files with a single reusable function, builds the earthquake and drought indicators from the disaster data, derives the binary armed conflict exposure and its one-year lag, merges in the covariates, and writes `data/processed/final_data.csv` (186 countries, 2000 to 2019).
