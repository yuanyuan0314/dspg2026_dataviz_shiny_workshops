library(tidyverse)
library(plotly)
library(palmerpenguins)

# Demo: plotly -- ggplotly() and native plot_ly().
#
# HOW TO RUN: Select the lines for each section and press Ctrl+Enter.
# Do NOT use Source / Run All -- plotly widgets must be printed one at a time
# or only the last one will appear in the Viewer pane.
#

# ── Section 1: ggplotly() -- wrap any ggplot in one line ─────────────────────
p <- penguins |>
  drop_na() |>
  ggplot(aes(
    x      = bill_length_mm,
    y      = body_mass_g,
    color  = species,
    shape  = species,
    text   = paste0(
      "Species: ", species, "<br>",
      "Bill: ",    bill_length_mm, " mm<br>",
      "Mass: ",    body_mass_g,    " g<br>",
      "Island: ",  island
    )
  )) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_manual(values = c("#861F41", "#E5751F", "#2D2D2D")) +
  labs(
    title  = "Penguin Bill Length vs Body Mass",
    x      = "Bill Length (mm)",
    y      = "Body Mass (g)",
    color  = "Species",
    shape  = "Species"
  ) +
  theme_minimal(base_size = 13)

print(ggplotly(p, tooltip = "text"))   # hover shows custom tooltip


# ── Section 2: native plot_ly() -- bar chart with full control ────────────────
bar_chart <- penguins |>
  drop_na() |>
  group_by(species) |>
  summarise(mean_mass = mean(body_mass_g)) |>
  plot_ly(
    x      = ~species,
    y      = ~mean_mass,
    type   = "bar",
    color  = ~species,
    colors = c("#861F41", "#E5751F", "#2D2D2D")
  ) |>
  layout(
    title      = "Average Body Mass by Species",
    xaxis      = list(title = "Species"),
    yaxis      = list(title = "Mean Body Mass (g)"),
    showlegend = FALSE
  )

print(bar_chart)
