# armed_conflict

Reproducing and replicating the analysis in "Implications of armed conflict for maternal and child health: A regression analysis of data from 181 countries for 2000-2019" (Jawad et al., PLOS Medicine, 2021).

Course project for CHL5233, Fall 2026.

## Structure

- `data/raw/`: raw data files as downloaded
- `data/processed/`: the merged analytical data set
- `R/`: R scripts that build the analytical data set and produce the tables and figures
- `outputs/tables/`, `outputs/figures/`: rendered tables and figures
- `reports/`: Quarto documents and rendered output

## Building the data

`R/create_final_data.R` reshapes the four World Bank mortality files with a single reusable function, builds the earthquake and drought indicators from the disaster data, derives the binary armed conflict exposure and its one-year lag, merges in the covariates, and writes `data/processed/final_data.csv` (186 countries, 2000 to 2019, 3720 rows).

`R/create_final_data_agent.R` builds the same data set from the same specification using base R only, and writes `data/processed/final_data_agent.csv`. It exists so the two implementations can be compared against each other.

Both scripts run from a clean R session and only need the project to be the working directory.

## Reports

- `reports/quarto_inclass.qmd`: week 2 Quarto assignment, country and region counts
- `reports/compare_final_data.qmd`: compares the two builds of the analytical data set cell by cell, documents the judgment calls where they could have diverged, and reconciles the exposure count against the published paper
- `reports/eda.qmd`: exploratory analysis of the panel, covering missingness, the distribution and time trend of the four mortality outcomes, how common the conflict exposure is, and the unadjusted association between the two

## Tables and figures

Each script produces one output and is named after it.

- `R/table01_descriptive.R` builds Table 1 for the primary analyses with the binary exposure, stratified by armed conflict in the previous year, with the four outcomes, all ten adjusted covariates, and the number missing for every variable. It writes `outputs/tables/table01_descriptive.pdf` and `outputs/tables/table01_descriptive.docx`.
- `R/fig01_matmor_increase.R` plots the maternal mortality trend for the 13 countries where it rose between 2000 and 2017, one colour and one direct label per country. It writes `outputs/figures/fig01_matmor_increase.png`.

The window ends in 2017 because the World Bank maternal mortality series has no values for 2018 or 2019 for any country.
- `R/table02_regression.R` fits the four mortality models with `plm()`, using country and year fixed effects, armed conflict lagged by one year, and GDP per capita on the log scale, with standard errors clustered by country. It writes `outputs/tables/table02_regression.pdf`, laid out like Table 2 in the paper for the binary conflict specification, and prints a comparison of the GDP coefficient on the raw and log scales.
