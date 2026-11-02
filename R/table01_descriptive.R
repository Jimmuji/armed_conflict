### CHL5233 Week 6 assignment 1
### Table 1 for the armed conflict paper, primary analyses, binary exposure
###
### Design decisions, following "Who is in this study, anyway?"
###   Unit of observation: country-year. The primary models regress each
###     outcome on armed conflict lagged by one year, so the analytic sample is
###     the country-years where the lagged exposure is observed. That is 2001
###     to 2019, which drops the 186 year 2000 rows.
###   Columns: the exposure, not the outcome. Three columns: no armed conflict
###     in the previous year, armed conflict in the previous year, and overall.
###   Rows: the four primary outcomes plus all ten covariates that the models
###     adjust for. The published Table 1 reports only outcomes, which leaves
###     the reader unable to judge how comparable the two groups are.
###   Missing data: shown as a count and percent for every variable.
###
### Output: outputs/tables/table01_descriptive.pdf
###         outputs/tables/table01_descriptive.docx

library(dplyr)
library(table1)
library(flextable)
library(here)

dat <- read.csv(here("data", "processed", "final_data.csv"))

analytic <- dat |>
  filter(!is.na(armedconflict_lag1)) |>
  mutate(
    conflict = factor(armedconflict_lag1,
                      levels = c(0, 1),
                      labels = c("No armed conflict", "Armed conflict")),
    oecd = factor(oecd, levels = c(0, 1), labels = c("No", "Yes")),
    earthquake = factor(earthquake, levels = c(0, 1), labels = c("No", "Yes")),
    drought = factor(drought, levels = c(0, 1), labels = c("No", "Yes"))
  )

# Units are written into the labels rather than set with units(), because
# table1 wraps the units in an HTML span that does not survive the conversion
# to LaTeX.
label(analytic$matmor) <- "Maternal mortality ratio, per 100,000 live births"
label(analytic$un5mor) <- "Under-5 mortality rate, per 1,000 live births"
label(analytic$infmor) <- "Infant mortality rate, per 1,000 live births"
label(analytic$neomor) <- "Neonatal mortality rate, per 1,000 live births"
label(analytic$gdp_1000) <- "GDP per capita, thousands of US dollars"
label(analytic$oecd) <- "OECD member"
label(analytic$pop_dens) <- "Population density"
label(analytic$urban) <- "Urban residence"
label(analytic$age_dep) <- "Age dependency ratio"
label(analytic$male_edu) <- "Male education, years"
label(analytic$temp) <- "Mean temperature, degrees Celsius"
label(analytic$rainfall_1000) <- "Mean rainfall, thousands of mm"
label(analytic$earthquake) <- "Earthquake that year"
label(analytic$drought) <- "Drought that year"

# All four outcomes are strongly right skewed, with standard deviations larger
# than their means, so the median and interquartile range are reported next to
# the mean rather than instead of it.
render_continuous <- function(x, ...) {
  with(stats::setNames(as.list(c(mean(x, na.rm = TRUE),
                                 sd(x, na.rm = TRUE),
                                 quantile(x, c(0.25, 0.5, 0.75), na.rm = TRUE))),
                       c("m", "s", "q1", "q2", "q3")),
       c("",
         "Mean (SD)" = sprintf("%s (%s)", round_pad(m, 1), round_pad(s, 1)),
         "Median [IQR]" = sprintf("%s [%s, %s]", round_pad(q2, 1),
                                  round_pad(q1, 1), round_pad(q3, 1))))
}

render_categorical <- function(x, ...) {
  c("", sapply(stats::na.omit(table1::stats.default(x)),
               function(y) with(y, sprintf("%d (%s%%)", FREQ, round_pad(PCT, 1)))))
}

render_missing <- function(x, ...) {
  n <- sum(is.na(x))
  c("Missing" = sprintf("%d (%s%%)", n, round_pad(100 * n / length(x), 1)))
}

# table1 only prints a missing row for variables that actually have missing
# values. The assignment asks for the count on every variable, so the three
# renderers are combined into one and passed as `render`, which overrides that
# behaviour and always appends the row.
render_all <- function(x, ...) {
  if (is.factor(x) || is.logical(x) || is.character(x)) {
    c(render_categorical(x, ...), render_missing(x, ...))
  } else {
    c(render_continuous(x, ...), render_missing(x, ...))
  }
}

caption <- paste(
  "Characteristics of the 186 countries contributing to the primary analyses,",
  "by armed conflict status in the previous year."
)

footnote <- paste(
  "Note.",
  "The unit of observation is the country-year.",
  "Armed conflict is defined as 25 or more battle-related deaths in a",
  "country-year and is lagged by one year, so the analytic sample runs from",
  "2001 to 2019 and each country contributes up to 19 observations.",
  "Countries move between the two columns as their conflict status changes,",
  "so the columns are not mutually exclusive groups of countries.",
  "Missing is reported for every variable because the denominator differs by",
  "row; maternal mortality in particular is unavailable for 2018 and 2019,",
  "which is why it is missing for about 12 percent of country-years.",
  "Covariates are reported in the units of the supplied covariates file."
)

make_table <- function(...) {
  table1(
    ~ matmor + un5mor + infmor + neomor +
      gdp_1000 + oecd + pop_dens + urban + age_dep + male_edu +
      temp + rainfall_1000 + earthquake + drought | conflict,
    data = analytic,
    overall = "Overall",
    render = render_all,
    ...
  )
}

### Word output -----------------------------------------------------------

# flextable wraps the caption and footnote across lines, so they can be part
# of the table object here.
t1flex(make_table(caption = caption, footnote = footnote)) |>
  set_table_properties(layout = "autofit") |>
  font(fontname = "Calibri", part = "all") |>
  fontsize(size = 9, part = "all") |>
  save_as_docx(path = here("outputs", "tables", "table01_descriptive.docx"))

### PDF output ------------------------------------------------------------

# In LaTeX the caption and footnote are set outside the table. Passing them to
# table1() puts them inside a table cell, which LaTeX will not break across
# lines, and the table then runs off the page.
latex_body <- t1kable(make_table(caption = caption), format = "latex",
                      booktabs = TRUE, longtable = TRUE)

tex <- c(
  "\\documentclass[11pt]{article}",
  "\\usepackage[margin=1in]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{caption}",
  "\\usepackage{array}",
  "\\usepackage{makecell}",
  "\\usepackage{multirow}",
  "\\usepackage{xcolor}",
  "\\usepackage{colortbl}",
  "\\usepackage{longtable}",
  "\\captionsetup{labelfont=bf, skip=6pt}",
  "\\pagestyle{empty}",
  "\\begin{document}",
  "\\small",
  latex_body,
  "\\vspace{1ex}",
  "\\footnotesize",
  "\\noindent", footnote,
  "\\end{document}"
)

tex_dir <- here("outputs", "tables")
tex_file <- file.path(tex_dir, "table01_descriptive.tex")
writeLines(tex, tex_file)

tinytex::latexmk(tex_file, engine = "pdflatex")
file.remove(list.files(tex_dir, pattern = "table01_descriptive\\.(tex|aux|log)$",
                       full.names = TRUE))
