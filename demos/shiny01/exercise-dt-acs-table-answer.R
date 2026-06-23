# Exercise Answer Key: Build a Filterable ACS Table with DT
# DSPG 2026 | Virginia Tech
#
# This is the complete answer key.
# For the student version with blanks, see: exercise-dt-acs-table.R

library(DT)
library(tidycensus)
library(tidyverse)

# ── 1. Pull data from ACS ────────────────────────────────────────────────────

va_data <- get_acs(
  geography = "county",
  state     = "VA",
  variables = c(
    median_income = "B19013_001",
    pct_college   = "B15003_022",
    pov_count     = "B17001_002",
    pov_total     = "B17001_001"
  ),
  year   = 2022,
  output = "wide"
) |>
  mutate(
    pov_rate    = pov_countE / pov_totalE,
    county_name = str_remove(NAME, ", Virginia")
  ) |>
  select(county_name, median_incomeE, pct_collegeE, pov_rate)

# ── 2. Rename columns for display ────────────────────────────────────────────

va_display <- va_data |>
  rename(
    County           = county_name,
    `Median Income`  = median_incomeE,
    `College Grads`  = pct_collegeE,
    `Poverty Rate`   = pov_rate
  )

# ── 3. Build the DT table ────────────────────────────────────────────────────

va_display |>
  datatable(
    filter     = "top",           # per-column filter boxes
    rownames   = FALSE,           # hide row numbers
    caption    = "Virginia County Profile — ACS 5-Year Estimates 2022",
    extensions = "Buttons",       # enables CSV/Excel export
    options    = list(
      pageLength = 15,
      scrollX    = TRUE,
      dom        = "Bfrtip",
      buttons    = c("csv", "excel")
    )
  ) |>
  formatCurrency(
    columns  = "Median Income",
    currency = "$",
    digits   = 0
  ) |>
  formatPercentage(
    columns = "Poverty Rate",
    digits  = 1
  ) |>
  formatRound(
    columns = "College Grads",
    digits  = 0
  )


# ── 4. Bonus: filter before display ──────────────────────────────────────────

va_display |>
  filter(`Poverty Rate` > 0.15) |>
  arrange(desc(`Poverty Rate`)) |>
  datatable(
    rownames = FALSE,
    caption  = "High-Poverty Virginia Counties (> 15%)",
    options  = list(pageLength = 20)
  ) |>
  formatPercentage("Poverty Rate", digits = 1)


# ── 5. Bonus: substitute your own project data ────────────────────────────────

# my_project_data |>
#   rename(
#     `My Column` = raw_column_name
#   ) |>
#   datatable(
#     filter = "top",
#     rownames = FALSE,
#     options = list(pageLength = 15, scrollX = TRUE)
#   )
