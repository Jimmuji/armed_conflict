### CHL5233 Week 3 in-class assignment
### Create the analytical data set for the armed conflict paper

library(tidyverse)
library(janitor)
library(here)

raw_path <- here("data", "raw")

### ---------------------------------------------------------------
### 1. World Bank mortality data
### ---------------------------------------------------------------

# The four World Bank files share the same wide structure, so the
# reshaping is written once as a function and applied to each file.
wbfun <- function(filename, varname) {
  read.csv(file.path(raw_path, filename), header = TRUE) |>
    dplyr::select(iso, X2000:X2019) |>
    pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X",
                 values_to = varname) |>
    mutate(year = as.numeric(year)) |>
    arrange(iso, year)
}

wbfiles <- c(matmor = "maternal_mortality.csv",
             infmor = "infant_mortality.csv",
             neomor = "neonatal_mortality.csv",
             un5mor = "under5_mortality.csv")

wblist <- map2(wbfiles, names(wbfiles), wbfun)

wbdata <- wblist |>
  reduce(full_join, by = c("iso", "year"))

### ---------------------------------------------------------------
### 2. Disaster data
### ---------------------------------------------------------------

disaster <- read.csv(file.path(raw_path, "disaster.csv"), header = TRUE) |>
  clean_names() |>
  filter(year >= 2000, year <= 2019,
         disaster_type %in% c("Earthquake", "Drought")) |>
  dplyr::select(year, iso, disaster_type) |>
  distinct() |>
  mutate(value = 1) |>
  pivot_wider(names_from = disaster_type,
              values_from = value,
              values_fill = 0) |>
  clean_names() |>
  dplyr::select(year, iso, earthquake, drought)

### ---------------------------------------------------------------
### 3. Conflict data
### ---------------------------------------------------------------

# A country-year is coded as having an armed conflict when the total
# number of battle related deaths reaches the UCDP threshold of 25,
# which is the definition of the binary exposure used in the paper.
conflict <- read.csv(file.path(raw_path, "conflict.csv"), header = TRUE) |>
  group_by(iso, year) |>
  summarise(deaths = sum(best, na.rm = TRUE), .groups = "drop") |>
  mutate(armedconflict = if_else(deaths >= 25, 1, 0)) |>
  dplyr::select(iso, year, armedconflict)

### ---------------------------------------------------------------
### 4. Covariates
### ---------------------------------------------------------------

covariates <- read.csv(file.path(raw_path, "covariates.csv"), header = TRUE)

### ---------------------------------------------------------------
### 5. Merge everything
### ---------------------------------------------------------------

final_data <- wbdata |>
  left_join(conflict, by = c("iso", "year")) |>
  left_join(disaster, by = c("iso", "year")) |>
  left_join(covariates, by = c("iso", "year")) |>
  # countries without a conflict or a disaster in a given year are
  # absent from those files, so those missing values are true zeros
  mutate(across(c(armedconflict, earthquake, drought),
                \(x) replace_na(x, 0))) |>
  # the exposure enters the models lagged by one year
  arrange(iso, year) |>
  group_by(iso) |>
  mutate(armedconflict_lag1 = lag(armedconflict, n = 1)) |>
  ungroup()

write.csv(final_data,
          here("data", "processed", "final_data.csv"),
          row.names = FALSE)
