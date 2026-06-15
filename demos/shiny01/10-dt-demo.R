library(tidyverse)
library(DT)
library(nycflights13)

# Demo: DT â€” sortable, searchable, exportable tables.
#
# HOW TO RUN: Select the lines for each section and press Ctrl+Enter.
# Do NOT use Source / Run All â€” each widget must be printed one at a time.
#

# â”€â”€ 1. Basic datatable â€” instant sort + search â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
print(
  flights |>
    slice_sample(n = 500) |>
    select(year, month, day, carrier, origin, dest,
           dep_delay, arr_delay, air_time) |>
    datatable()
)


# â”€â”€ 2. With column filters, pagination, and export buttons â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
print(
  flights |>
    slice_sample(n = 500) |>
    select(carrier, origin, dest, dep_delay, arr_delay, air_time) |>
    datatable(
      filter     = "top",
      rownames   = FALSE,
      extensions = "Buttons",
      options    = list(
        pageLength = 10,
        scrollX    = TRUE,
        dom        = "Bfrtip",
        buttons    = c("csv", "excel")
      ),
      caption = "Sample of 500 NYC Flights (2013)"
    )
)


# â”€â”€ 3. With number formatting â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€
# Replace my_county_data with your own data frame
# my_county_data |>
#   datatable(
#     filter     = "top",
#     extensions = "Buttons",
#     options    = list(dom = "Bfrtip", buttons = c("csv", "excel"),
#                       pageLength = 15)
#   ) |>
#   formatCurrency("median_income", currency = "$", digits = 0) |>
#   formatPercentage("poverty_rate", digits = 1)
