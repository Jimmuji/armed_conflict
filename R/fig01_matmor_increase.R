### CHL5233 Week 6 assignment 2
### Trend in maternal mortality for the countries where it rose, 2000 to 2017
###
### The window ends in 2017 because the World Bank maternal mortality series
### has no values for 2018 or 2019: every one of the 186 countries is missing
### both years, so 2017 is the last year in which a country can be compared
### with its own value in 2000.
###
### Output: outputs/figures/fig01_matmor_increase.png

library(dplyr)
library(ggplot2)
library(ggrepel)
library(here)

dat <- read.csv(here("data", "processed", "final_data.csv"))

countries <- read.table(here("data", "raw", "countries.txt"),
                        header = TRUE, stringsAsFactors = FALSE) |>
  rename(iso = ISO, country = country_name)

### Select the countries where maternal mortality was higher in 2017 than in
### 2000. diff_matmor is each year's value minus the country's year 2000 value,
### so the selection is the sign of diff_matmor in 2017.

trends <- dat |>
  select(iso, year, matmor) |>
  filter(year < 2018) |>
  arrange(iso, year) |>
  group_by(iso) |>
  filter(!any(is.na(matmor))) |>
  mutate(diff_matmor = matmor - matmor[1L]) |>
  ungroup()

n_complete <- n_distinct(trends$iso)

endpoints <- trends |> filter(year == 2017)
n_down <- sum(endpoints$diff_matmor < 0)
n_same <- sum(endpoints$diff_matmor == 0)

increased <- endpoints |>
  filter(diff_matmor > 0) |>
  pull(iso)

plot_data <- trends |>
  filter(iso %in% increased) |>
  left_join(countries, by = "iso") |>
  mutate(country = reorder(country, -matmor))

labels <- plot_data |>
  filter(year == max(year))

### The countries span two orders of magnitude, from 10 to 480 deaths per
### 100,000 live births, so a linear axis would flatten every line except
### Haiti's. The y axis is logged and the lines are labelled directly, which
### keeps the colours as an aid to tracking a line rather than the only way to
### identify it.

fig <- ggplot(plot_data, aes(x = year, y = matmor,
                             colour = country, group = country)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.1) +
  geom_text_repel(data = labels, aes(label = country),
                  nudge_x = 0.6, hjust = 0, direction = "y",
                  segment.size = 0.3, segment.alpha = 0.5,
                  size = 3, min.segment.length = 0, seed = 1) +
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015, 2017),
                     limits = c(2000, 2022.5)) +
  scale_y_log10(breaks = c(10, 20, 50, 100, 200, 500)) +
  scale_colour_viridis_d(option = "turbo", end = 0.92) +
  labs(
    title = paste0("Maternal mortality rose in ", length(increased),
                   " of ", n_complete,
                   " countries between 2000 and 2017"),
    subtitle = paste0("Of the rest, ", n_down, " declined and ", n_same,
                      " were unchanged.",
                      " Note the logarithmic vertical axis."),
    x = NULL,
    y = "Maternal deaths per 100,000 live births",
    caption = paste("Source: World Bank maternal mortality estimates.",
                    "The series ends in 2017; 2018 and 2019 are missing for",
                    "all countries.")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(colour = "grey30", size = 9.5),
    plot.caption = element_text(colour = "grey40", size = 8, hjust = 0),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )

ggsave(here("outputs", "figures", "fig01_matmor_increase.png"),
       fig, width = 9, height = 6, dpi = 300, bg = "white")
