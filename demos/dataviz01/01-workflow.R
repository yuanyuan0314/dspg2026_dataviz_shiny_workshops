library(tidyverse)
library(tidycensus)
library(ggrepel)
library(scales)

# Demo: 7-step ggplot2 refinement workflow using ACS state data.
# Requires a Census API key — register free at https://api.census.gov/data/key_signup.html
# then run once: tidycensus::census_api_key("YOUR_KEY", install = TRUE)

state_data <- get_acs(
  geography = "state",
  variables = c(
    median_income = "B19013_001",
    pop_25plus    = "B15003_001",
    ba            = "B15003_022",
    ma            = "B15003_023",
    prof          = "B15003_024",
    phd           = "B15003_025"
  ),
  year   = 2022,
  output = "wide"
) |>
  mutate(pct_college = (baE + maE + profE + phdE) / pop_25plusE * 100) |>
  left_join(
    tibble(
      NAME   = state.name,
      region = recode(as.character(state.region), "North Central" = "Midwest")
    ),
    by = "NAME"
  ) |>
  filter(!is.na(region)) |>
  select(NAME, region, median_income = median_incomeE, pct_college)

state_outliers <- state_data |>
  filter(NAME %in% c("Massachusetts", "Maryland", "Colorado",
                     "Mississippi", "West Virginia", "Utah",
                     "North Carolina"))

# ---- Step 1: Skeleton --------------------------------------------------
# Get the relationship visible before adding anything.
ggplot(state_data, aes(x = pct_college, y = median_income, color = region)) +
  geom_point(size = 2.5)

# ---- Step 2: Labels ----------------------------------------------------
ggplot(state_data, aes(x = pct_college, y = median_income, color = region)) +
  geom_point(size = 2.5) +
  labs(
    title   = "Education Attainment and Household Income by State, 2022",
    x       = "Bachelor's Degree or Higher (%)",
    y       = "Median Household Income",
    color   = "Census Region",
    caption = "Source: ACS 5-Year Estimates 2022"
  )

# ---- Step 3: Color palette (viridis_d = colorblind-safe) ---------------
ggplot(state_data, aes(x = pct_college, y = median_income, color = region)) +
  geom_point(size = 2.5) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE,
              color = "gray40", linewidth = 0.8) +
  scale_color_viridis_d() +
  labs(
    title   = "Education Attainment and Household Income by State, 2022",
    x       = "Bachelor's Degree or Higher (%)",
    y       = "Median Household Income",
    color   = "Census Region",
    caption = "Source: ACS 5-Year Estimates 2022"
  )

# ---- Step 4: Theme -----------------------------------------------------
ggplot(state_data, aes(x = pct_college, y = median_income, color = region)) +
  geom_point(size = 2.5) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE,
              color = "gray40", linewidth = 0.8) +
  scale_color_viridis_d() +
  labs(
    title   = "Education Attainment and Household Income by State, 2022",
    x       = "Bachelor's Degree or Higher (%)",
    y       = "Median Household Income",
    color   = "Census Region",
    caption = "Source: ACS 5-Year Estimates 2022"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right", panel.grid.minor = element_blank())

# ---- Step 5: Axis scale ------------------------------------------------
# coord_cartesian clips without dropping data — trend line still uses all points.
ggplot(state_data, aes(x = pct_college, y = median_income, color = region)) +
  geom_point(size = 2.5) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE,
              color = "gray40", linewidth = 0.8) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = label_dollar(scale = 1e-3, suffix = "k")) +
  coord_cartesian(xlim = c(20, NA), ylim = c(40000, 100000)) +
  labs(
    title   = "Education Attainment and Household Income by State, 2022",
    x       = "Bachelor's Degree or Higher (%)",
    y       = "Median Household Income",
    color   = "Census Region",
    caption = "Source: ACS 5-Year Estimates 2022"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right", panel.grid.minor = element_blank())

# ---- Step 6: Annotations -----------------------------------------------
# geom_label_repel() — labeled outlier states, no overlap.
final_plot <- ggplot(state_data, aes(x = pct_college, y = median_income, color = region)) +
  geom_point(size = 2.5) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE,
              color = "gray40", linewidth = 0.8) +
  geom_label_repel(data = state_outliers, aes(label = NAME),
                   size = 2.8, box.padding = 0.4, show.legend = FALSE) +
  scale_color_viridis_d() +
  scale_y_continuous(labels = label_dollar(scale = 1e-3, suffix = "k")) +
  coord_cartesian(xlim = c(20, NA), ylim = c(40000, 100000)) +
  labs(
    title   = "Education Attainment and Household Income by State, 2022",
    x       = "Bachelor's Degree or Higher (%)",
    y       = "Median Household Income",
    color   = "Census Region",
    caption = "Source: ACS 5-Year Estimates 2022"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right", panel.grid.minor = element_blank())

print(final_plot)

# ---- Step 7: Save ------------------------------------------------------
# ggsave("figures/state_income_education_2022.png",
#        plot = final_plot, width = 10, height = 5.5, dpi = 300)
