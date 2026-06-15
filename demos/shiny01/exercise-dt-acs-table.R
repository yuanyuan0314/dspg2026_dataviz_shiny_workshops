# Exercise: Build a Filterable ACS Table with DT
# DSPG 2026 | Virginia Tech
#
# Goal: Pull Virginia county data from the ACS API and display it
#       as a sortable, filterable, downloadable DT table.
#
# Fill in every _____ to complete the exercise.
# Run the whole script in RStudio — the table will open in the Viewer pane.

library(DT)
library(tidycensus)
library(tidyverse)

# ── 1. Pull data from ACS ────────────────────────────────────────────────────
# get_acs() returns one row per variable per geography.
# output = "wide" pivots to one row per county.

va_data <- get_acs(
  geography = "county",
  state     = "VA",
  variables = c(
    median_income = "B19013_001",   # median household income
    pct_college   = "B15003_022",   # bachelor's degree holders (25+)
    pov_count     = "B17001_002",   # people below poverty line
    pov_total     = "B17001_001"    # denominator for poverty rate
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
# Fill in the blanks below.

va_display |>
  datatable(
    filter   = _____,        # "top" = per-column filter boxes
    rownames = _____,        # FALSE = hide row numbers
    caption  = "Virginia County Profile — ACS 5-Year Estimates 2022",
    extensions = _____,      # "Buttons" enables CSV/Excel export
    options  = list(
      pageLength = _____,    # how many rows per page? try 15
      scrollX    = TRUE,
      dom        = "Bfrtip", # show Buttons, filter, records, table, info, pager
      buttons    = c(_____, _____)  # export to "csv" and "excel"
    )
  ) |>
  formatCurrency(
    columns  = _____,        # column name to format as currency
    currency = "$",
    digits   = 0
  ) |>
  formatPercentage(
    columns = _____,         # column name to format as percentage
    digits  = 1
  ) |>
  formatRound(
    columns = _____,         # column name to round to 0 decimal places
    digits  = 0
  )


# ── 4. Bonus: filter before display ──────────────────────────────────────────
# Show only counties with poverty rate above 15%

va_display |>
  filter(`Poverty Rate` > _____) |>   # fill in the threshold
  arrange(desc(`Poverty Rate`)) |>
  datatable(
    rownames = FALSE,
    caption  = "High-Poverty Virginia Counties (> 15%)",
    options  = list(pageLength = 20)
  ) |>
  formatPercentage("Poverty Rate", digits = 1)


# ── 5. Bonus: substitute your own project data ────────────────────────────────
# Replace va_data with your project dataset and rebuild the table.
# Tip: any data frame works — just rename the columns for display first.

# my_project_data |>
#   rename(
#     `My Column` = raw_column_name
#   ) |>
#   datatable(
#     filter = "top",
#     rownames = FALSE,
#     options = list(pageLength = 15, scrollX = TRUE)
#   )
