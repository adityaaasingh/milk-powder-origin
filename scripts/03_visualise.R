# scripts/03_visualise.R
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(ggfortify)
})

# -------------------------
# 0) Settings
# -------------------------
TOP_N <- 10
TOP_K_IMPORTANCE_PLOT <- 15

# -------------------------
# 1) Folders
# -------------------------
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# -------------------------
# 2) Load processed data + selected elements
# -------------------------
df <- read.csv("data/processed/trace_elements_processed.csv", check.names = FALSE)
df$Region <- as.factor(df$Region)

selected_path <- paste0("outputs/tables/selected_top_", TOP_N, "_elements.csv")
stopifnot(file.exists(selected_path))
selected <- read.csv(selected_path)$selected

# Importance table created in 02_model_rf.R
imp_path <- "outputs/tables/rf_full_importance.csv"
stopifnot(file.exists(imp_path))
imp_df <- read.csv(imp_path)

# -------------------------
# 3) Importance plot (Top K by MDA)
# -------------------------
imp_top <- imp_df %>%
  slice_head(n = TOP_K_IMPORTANCE_PLOT) %>%
  mutate(element = factor(element, levels = rev(element)))

p_imp <- ggplot(imp_top, aes(x = element, y = MDA)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste0("Top ", TOP_K_IMPORTANCE_PLOT, " Random Forest Predictors (MDA)"),
    x = "Element",
    y = "Mean Decrease Accuracy"
  ) +
  theme_minimal()

ggsave(
  filename = "outputs/figures/rf_importance_top15_mda.png",
  plot = p_imp,
  width = 9, height = 6, dpi = 300
)

# -------------------------
# 4) PCA plot (selected elements)
# -------------------------
pca <- prcomp(df[, selected], scale. = TRUE)

p_pca <- autoplot(
  pca,
  data = df,
  colour = "Region",
  frame = TRUE
) +
  labs(title = paste0("PCA of Top ", TOP_N, " Elements (by MDA)"))

ggsave(
  filename = paste0("outputs/figures/pca_top_", TOP_N, ".png"),
  plot = p_pca,
  width = 9, height = 6, dpi = 300
)

# -------------------------
# 5) Faceted boxplots (selected elements)
# -------------------------
df_long <- df %>%
  select(Region, all_of(selected)) %>%
  pivot_longer(
    cols = -Region,
    names_to = "Element",
    values_to = "Concentration"
  )

p_box <- ggplot(df_long, aes(x = Region, y = Concentration, fill = Region)) +
  geom_boxplot(outlier.size = 0.6) +
  facet_wrap(~ Element, scales = "free_y", ncol = 4) +
  labs(
    title = paste0("Elemental Concentrations by Region (Top ", TOP_N, " Elements)"),
    y = "Concentration (ppm)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )

ggsave(
  filename = paste0("outputs/figures/boxplots_top_", TOP_N, ".png"),
  plot = p_box,
  width = 12, height = 8, dpi = 300
)

message("Saved figures to outputs/figures/")