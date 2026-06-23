# Demo: Interactive Tables with DT
# DSPG 2026 | Virginia Tech
#
# DT wraps the JavaScript DataTables library — instant sort, search,
# filter, and export with zero extra code.
#
# HOW TO RUN: Select each section and press Ctrl+Enter.
# Do NOT use Source / Run All — each widget must be printed one at a time.

library(tidyverse)
library(DT)
library(nycflights13)
library(scales)

# ── Dataset ───────────────────────────────────────────────────────────────────
flights_sub <- flights |>
  slice_sample(n = 500) |>
  select(carrier, origin, dest, dep_delay, arr_delay, air_time, distance) |>
  rename(
    Carrier    = carrier,
    Origin     = origin,
    Dest       = dest,
    `Dep Delay` = dep_delay,
    `Arr Delay` = arr_delay,
    `Air Time`  = air_time,
    Distance    = distance
  )


# ── Section 1: Basic table — sort and search out of the box ──────────────────
datatable(flights_sub)


# ── Section 2: Per-column filters + export buttons ───────────────────────────
datatable(
  flights_sub,
  filter     = "top",        # filter box above each column
  rownames   = FALSE,
  extensions = "Buttons",
  caption    = "Sample of 500 NYC Flights (2013)",
  options    = list(
    pageLength = 10,
    scrollX    = TRUE,
    dom        = "Bfrtip",   # Buttons, filter, records, table, info, pager
    buttons    = c("csv", "excel")
  )
)


# ── Section 3: Number formatting ──────────────────────────────────────────────
# formatCurrency, formatRound, formatStyle — applied after datatable()

datatable(
  flights_sub,
  filter   = "top",
  rownames = FALSE,
  options  = list(pageLength = 10, scrollX = TRUE)
) |>
  formatRound(columns = c("Dep Delay", "Arr Delay"), digits = 0) |>
  formatRound(columns = "Air Time", digits = 0) |>
  formatStyle(
    columns    = "Dep Delay",
    background = styleInterval(
      cuts   = c(0, 30),
      values = c("#d4edda", "#fff3cd", "#f8d7da")  # green / yellow / red
    )
  )


# ── Section 4: Highlight rows by condition ────────────────────────────────────
# Color entire row red when arrival delay > 60 minutes

datatable(
  flights_sub,
  rownames = FALSE,
  options  = list(pageLength = 10, scrollX = TRUE)
) |>
  formatStyle(
    columns         = "Arr Delay",
    target          = "row",
    backgroundColor = styleInterval(60, c("white", "#f8d7da"))
  )


# ── Section 5: Substitute your own project data ───────────────────────────────
# Any data frame works — rename columns for display first.

# my_data |>
#   rename(
#     `Column Label` = raw_column_name
#   ) |>
#   datatable(
#     filter     = "top",
#     rownames   = FALSE,
#     extensions = "Buttons",
#     options    = list(
#       pageLength = 15,
#       scrollX    = TRUE,
#       dom        = "Bfrtip",
#       buttons    = c("csv", "excel")
#     )
#   ) |>
#   formatCurrency("income_col", currency = "$", digits = 0) |>
#   formatPercentage("rate_col", digits = 1)
