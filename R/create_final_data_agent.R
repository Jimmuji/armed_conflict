### CHL5233 Week 5 in-class assignment
### Independent version of the analytical data set, written by the coding agent
### Built with base R only, so that it does not reuse any of the code in
### create_final_data.R. The output is compared against the week 3 version
### in reports/compare_final_data.qmd.

raw_path <- file.path(here::here(), "data", "raw")

### ---------------------------------------------------------------
### 1. World Bank mortality data
### ---------------------------------------------------------------

wb_long <- function(filename, varname) {
  wide <- read.csv(file.path(raw_path, filename), header = TRUE)
  year_cols <- grep("^X[0-9]{4}$", names(wide), value = TRUE)
  long <- reshape(wide[c("iso", year_cols)],
                  direction = "long",
                  varying = year_cols,
                  v.names = varname,
                  timevar = "year",
                  times = as.numeric(sub("^X", "", year_cols)),
                  idvar = "iso")
  long <- long[order(long$iso, long$year), c("iso", "year", varname)]
  row.names(long) <- NULL
  long
}

wbfiles <- c(matmor = "maternal_mortality.csv",
             infmor = "infant_mortality.csv",
             neomor = "neonatal_mortality.csv",
             un5mor = "under5_mortality.csv")

wblist <- mapply(wb_long, wbfiles, names(wbfiles), SIMPLIFY = FALSE)

wbdata <- Reduce(function(x, y) merge(x, y, by = c("iso", "year"), all = TRUE),
                 wblist)

### ---------------------------------------------------------------
### 2. Disaster data
### ---------------------------------------------------------------

dis_raw <- read.csv(file.path(raw_path, "disaster.csv"), header = TRUE,
                    check.names = TRUE)

dis <- dis_raw[dis_raw$Year >= 2000 & dis_raw$Year <= 2019 &
                 dis_raw$Disaster.Type %in% c("Earthquake", "Drought"),
               c("ISO", "Year", "Disaster.Type")]
names(dis) <- c("iso", "year", "type")
dis <- unique(dis)

disaster <- data.frame(
  iso = dis$iso,
  year = dis$year,
  earthquake = as.integer(dis$type == "Earthquake"),
  drought = as.integer(dis$type == "Drought")
)
disaster <- aggregate(cbind(earthquake, drought) ~ iso + year,
                      data = disaster, FUN = max)

### ---------------------------------------------------------------
### 3. Conflict data
### ---------------------------------------------------------------

con_raw <- read.csv(file.path(raw_path, "conflict.csv"), header = TRUE)

deaths <- aggregate(best ~ iso + year, data = con_raw, FUN = sum,
                    na.rm = TRUE, na.action = na.pass)
conflict <- data.frame(iso = deaths$iso,
                       year = deaths$year,
                       armedconflict = as.integer(deaths$best >= 25))

### ---------------------------------------------------------------
### 4. Covariates
### ---------------------------------------------------------------

covariates <- read.csv(file.path(raw_path, "covariates.csv"), header = TRUE)

### ---------------------------------------------------------------
### 5. Merge everything
### ---------------------------------------------------------------

final <- merge(wbdata, conflict, by = c("iso", "year"), all.x = TRUE)
final <- merge(final, disaster, by = c("iso", "year"), all.x = TRUE)
final <- merge(final, covariates, by = c("iso", "year"), all.x = TRUE)

# a country-year that does not appear in the conflict or disaster file had no
# event that year, which is a zero rather than a missing value
for (v in c("armedconflict", "earthquake", "drought")) {
  final[[v]][is.na(final[[v]])] <- 0
}

final <- final[order(final$iso, final$year), ]
row.names(final) <- NULL

# the exposure enters the models lagged by one year
final$armedconflict_lag1 <- unlist(lapply(split(final$armedconflict, final$iso),
                                          function(x) c(NA, head(x, -1))),
                                   use.names = FALSE)

write.csv(final,
          file.path(here::here(), "data", "processed", "final_data_agent.csv"),
          row.names = FALSE)
