library(tidyverse)
library(patchwork)
library(palmerpenguins)

# Demo: patchwork multi-panel layouts using palmerpenguins.
# Run sections with Ctrl+Enter to see each layout pattern.

penguins_c <- penguins |> drop_na()

p_scatter <- ggplot(penguins_c, aes(flipper_length_mm, body_mass_g, color = species)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  labs(x = "Flipper (mm)", y = "Mass (g)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

p_bar <- penguins_c |>
  count(species) |>
  ggplot(aes(species, n, fill = species)) +
  geom_col(width = 0.6) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  labs(x = NULL, y = "Count") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

p_box <- ggplot(penguins_c, aes(species, bill_length_mm, fill = species)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  labs(x = NULL, y = "Bill (mm)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# ---- Side by side: | ---------------------------------------------------
p_scatter | p_bar

# ---- Stacked: / --------------------------------------------------------
p_scatter / p_bar

# ---- Compound: (A | B) / C --------------------------------------------
(p_scatter | p_bar) / p_box

# ---- Unequal widths + panel labels -------------------------------------
(p_scatter | (p_bar / p_box)) +
  plot_layout(widths = c(2, 1)) +
  plot_annotation(
    title      = "Palmer Penguins: three-panel summary",
    caption    = "Source: palmerpenguins package",
    tag_levels = "A"
  )
