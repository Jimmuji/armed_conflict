### CHL5233 Week 7 in-class assignment
### Table 2 for the armed conflict paper, binary conflict specification only
###
### Four panel regressions, one per mortality outcome, each with country and
### year fixed effects (the "twoways within" estimator in plm). The exposure is
### armed conflict lagged by one year, as in the paper. GDP per capita enters
### on the log scale, which is the change the assignment asks for; see the
### comparison written to the console at the end of this script.
###
### Standard errors are clustered by country, as in the paper, and the table
### reports the coefficient with a 95% confidence interval so that it reads
### like Table 2 rather than like default regression output.
###
### Output: outputs/tables/table02_regression.pdf

library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(texreg)
library(here)

dat <- read.csv(here("data", "processed", "final_data.csv")) |>
  mutate(log_gdp = log(gdp_1000))

### ---------------------------------------------------------------
### 1. Fit the four models
### ---------------------------------------------------------------

# The predictors are identical across the four outcomes, so the right hand
# side is written once and the outcome is substituted in.
predictors <- as.formula(
  " ~ armedconflict_lag1 + log_gdp + oecd + pop_dens + urban + age_dep +
      male_edu + temp + rainfall_1000 + earthquake + drought"
)

fit_panel <- function(outcome) {
  plm(update.formula(predictors, as.formula(paste(outcome, "~ ."))),
      index = c("iso", "year"),
      effect = "twoways",
      model = "within",
      data = dat)
}

# Short column headings keep the four confidence intervals on one page; the
# full outcome definitions are given in the note below the table.
outcomes <- c(matmor = "Maternal",
              un5mor = "Under-5",
              infmor = "Infant",
              neomor = "Neonatal")

models <- lapply(names(outcomes), fit_panel)
names(models) <- names(outcomes)

### ---------------------------------------------------------------
### 2. Country-clustered robust standard errors
### ---------------------------------------------------------------

clustered <- lapply(models, function(m) {
  coeftest(m, vcov = vcovHC(m, type = "HC1", cluster = "group"))
})

### ---------------------------------------------------------------
### 3. Build the table
### ---------------------------------------------------------------

coef_names <- c(
  armedconflict_lag1 = "Armed conflict in the previous year",
  log_gdp = "GDP per capita (log)",
  oecd = "OECD member",
  pop_dens = "Population density",
  urban = "Urban residence",
  age_dep = "Age dependency ratio",
  male_edu = "Male education",
  temp = "Temperature",
  rainfall_1000 = "Rainfall",
  earthquake = "Earthquake",
  drought = "Drought"
)

caption <- paste(
  "The association between armed conflict and maternal and child mortality,",
  "binary conflict specification."
)

note <- paste(
  "Outcomes are the maternal mortality ratio per 100,000 live births and the",
  "under-5, infant and neonatal mortality rates per 1,000 live births.",
  "Each column is one panel regression with country and year fixed effects.",
  "Cells are the adjusted beta coefficient and its 95 percent confidence",
  "interval, computed from standard errors clustered by country.",
  "Coefficients are the absolute change in the outcome for a one unit change",
  "in the predictor. Armed conflict is lagged by one year. GDP per capita is",
  "logged, so its coefficient is the change in the outcome for an e-fold",
  "increase in GDP; a 10 percent increase is about a tenth of that.",
  "Year dummies are fitted but not shown."
)

latex_body <- texreg(
  models,
  override.se = lapply(clustered, function(x) x[, "Std. Error"]),
  override.pvalues = lapply(clustered, function(x) x[, "Pr(>|t|)"]),
  ci.force = TRUE,
  ci.test = 0,
  custom.model.names = unname(outcomes),
  custom.coef.map = as.list(coef_names),
  caption = caption,
  caption.above = TRUE,
  booktabs = TRUE,
  use.packages = FALSE,
  digits = 2,
  float.pos = "h",
  label = "tab:table02"
)

tex <- c(
  "\\documentclass[11pt]{article}",
  "\\usepackage[margin=0.7in]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{dcolumn}",
  "\\usepackage{caption}",
  "\\captionsetup{labelfont=bf, skip=6pt}",
  "\\pagestyle{empty}",
  "\\begin{document}",
  # the file is table02, so the table should be numbered 2
  "\\setcounter{table}{1}",
  "\\footnotesize",
  latex_body,
  "\\vspace{1ex}",
  "\\footnotesize",
  "\\noindent Note. ", note,
  "\\end{document}"
)

tex_dir <- here("outputs", "tables")
tex_file <- file.path(tex_dir, "table02_regression.tex")
writeLines(tex, tex_file)

tinytex::latexmk(tex_file, engine = "pdflatex")
file.remove(list.files(tex_dir, pattern = "table02_regression\\.(tex|aux|log)$",
                       full.names = TRUE))

### ---------------------------------------------------------------
### 4. Why GDP is logged
### ---------------------------------------------------------------

# Fitting the maternal mortality model both ways shows what the log does. On
# the raw scale the coefficient is positive, which is the result reported in
# the paper and reads as though getting richer raises maternal mortality. On
# the log scale it is negative, which is the direction the literature would
# predict. The raw scale forces every extra thousand dollars to have the same
# effect whether a country moves from 1 to 2 thousand or from 100 to 101,
# which is implausible, and the few very rich country-years dominate the fit.

compare_gdp <- function(gdp_term) {
  form <- as.formula(paste(
    "matmor ~ armedconflict_lag1 +", gdp_term,
    "+ oecd + pop_dens + urban + age_dep + male_edu + temp + rainfall_1000 +
     earthquake + drought"
  ))
  m <- plm(form, index = c("iso", "year"), effect = "twoways",
           model = "within", data = dat)
  ct <- coeftest(m, vcov = vcovHC(m, type = "HC1", cluster = "group"))
  data.frame(
    specification = gdp_term,
    conflict = sprintf("%.2f (%.2f, %.2f)", ct[1, 1],
                       ct[1, 1] - 1.96 * ct[1, 2], ct[1, 1] + 1.96 * ct[1, 2]),
    gdp = sprintf("%.2f (%.2f, %.2f)", ct[2, 1],
                  ct[2, 1] - 1.96 * ct[2, 2], ct[2, 1] + 1.96 * ct[2, 2])
  )
}

print(rbind(compare_gdp("gdp_1000"), compare_gdp("log_gdp")), row.names = FALSE)
