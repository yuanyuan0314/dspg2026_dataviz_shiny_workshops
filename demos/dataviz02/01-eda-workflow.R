library(tidyverse)
library(palmerpenguins)
library(nycflights13)
library(skimr)
library(GGally)
library(corrplot)
library(scales)

# Demo: 6-step EDA workflow.
# Run sections with Ctrl+Enter — each step builds on the last.

# ---- Step 1: Summary statistics with skimr ----------------------------
penguins |> skim()

# Drill into missingness
penguins |>
  summarise(across(everything(), ~ mean(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") |>
  filter(pct_missing > 0) |>
  arrange(desc(pct_missing))

# ---- Step 2: Spot impossible values -----------------------------------
penguins |>
  filter(body_mass_g < 0 | bill_length_mm > 200)

# ---- Step 3: Hidden missing — complete() reveals implicit gaps --------
stocks <- tibble(
  year  = c(2020, 2020, 2020, 2020, 2021, 2021, 2021),
  qtr   = c(   1,    2,    3,    4,    2,    3,    4),
  price = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
stocks |> complete(year, qtr)

# ---- Step 4: Anomalies — boxplot + jitter ----------------------------
penguins |>
  ggplot(aes(species, body_mass_g, fill = species)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(aes(color = species), width = 0.15, alpha = 0.4, size = 1.2) +
  scale_fill_manual(values  = c("#E69F00", "#56B4E9", "#009E73")) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Body Mass by Species — Spot Outliers",
       x = NULL, y = "Body Mass (g)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# ---- Step 5: Distribution shape — histogram + density ----------------

# Histogram
penguins |>
  ggplot(aes(bill_length_mm)) +
  geom_histogram(binwidth = 2, fill = "#861F41", color = "white") +
  labs(title = "Distribution of Bill Length", x = "Bill Length (mm)", y = "Count") +
  theme_minimal(base_size = 13)

# Overlapping densities by species
penguins |>
  ggplot(aes(bill_length_mm, fill = species)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  labs(title = "Bill Length by Species", x = "Bill Length (mm)", y = "Density") +
  theme_minimal(base_size = 13)

# ---- Step 6: Correlations -------------------------------------------

# Scatterplot with trend line
set.seed(42)
flights |>
  slice_sample(n = 5000) |>
  ggplot(aes(dep_delay, arr_delay)) +
  geom_point(alpha = 0.2, color = "#861F41") +
  geom_smooth(method = "lm", color = "#E5751F", se = TRUE) +
  labs(title = "Departure Delay vs Arrival Delay",
       x = "Departure Delay (min)", y = "Arrival Delay (min)") +
  theme_minimal(base_size = 13)

# Scatterplot matrix — GGally
set.seed(42)
flights |>
  slice_sample(n = 2000) |>
  select(dep_delay, arr_delay, air_time) |>
  drop_na() |>
  ggpairs(
    lower = list(continuous = wrap("points", alpha = 0.2, color = "#861F41")),
    upper = list(continuous = wrap("cor", size = 4)),
    diag  = list(continuous = wrap("densityDiag", fill = "#E5751F", alpha = 0.5))
  ) +
  theme_minimal(base_size = 13)

# Correlation heatmap — corrplot
flights_num <- flights |>
  select(dep_delay, arr_delay, air_time, distance) |>
  drop_na()

corrplot(
  cor(flights_num),
  method      = "color",
  type        = "upper",
  col         = colorRampPalette(c("#861F41", "white", "#E5751F"))(200),
  tl.col      = "#2D2D2D",
  addCoef.col = "black",
  number.cex  = 0.8
)
