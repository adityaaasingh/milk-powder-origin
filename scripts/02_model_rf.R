# scripts/02_model_rf.R
suppressPackageStartupMessages({
  library(dplyr)
  library(randomForest)
  library(caret)
})

# -------------------------
# 0) Settings
# -------------------------
TOP_N <- 10
NTREE <- 1000
SEED  <- 123

# -------------------------
# 1) Folders
# -------------------------
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/models", recursive = TRUE, showWarnings = FALSE)

# -------------------------
# 2) Load processed data
# -------------------------
df <- read.csv("data/processed/trace_elements_processed.csv", check.names = FALSE)

stopifnot("Region" %in% names(df))
df$Region <- as.factor(df$Region)

# Identify predictor columns: all columns except Region, keep numeric only
predictor_cols <- setdiff(names(df), "Region")
predictor_cols <- predictor_cols[sapply(df[predictor_cols], is.numeric)]

stopifnot(length(predictor_cols) > 0)

rf_df <- df[, c("Region", predictor_cols)]

# -------------------------
# 3) Full Random Forest (all predictors)
# -------------------------
set.seed(SEED)
rf_full <- randomForest(
  Region ~ .,
  data = rf_df,
  importance = TRUE,
  ntree = NTREE
)

# Save confusion matrix
write.csv(rf_full$confusion, "outputs/tables/rf_full_confusion.csv")

# Importance table
imp <- importance(rf_full)
imp_df <- data.frame(
  element = rownames(imp),
  MDA = imp[, "MeanDecreaseAccuracy"],
  MDG = imp[, "MeanDecreaseGini"],
  row.names = NULL
) %>%
  arrange(desc(MDA))

write.csv(imp_df, "outputs/tables/rf_full_importance.csv", row.names = FALSE)

# -------------------------
# 4) Select Top N by MDA
# -------------------------
selected <- head(imp_df$element, TOP_N)

write.csv(
  data.frame(selected = selected),
  paste0("outputs/tables/selected_top_", TOP_N, "_elements.csv"),
  row.names = FALSE
)

# -------------------------
# 5) Reduced Random Forest (Top N)
# -------------------------
rf_reduced_df <- df[, c("Region", selected)]
rf_reduced_df$Region <- as.factor(rf_reduced_df$Region)

set.seed(SEED)
rf_reduced <- randomForest(
  Region ~ .,
  data = rf_reduced_df,
  importance = TRUE,
  ntree = NTREE
)

write.csv(
  rf_reduced$confusion,
  paste0("outputs/tables/rf_reduced_top_", TOP_N, "_confusion.csv")
)

# -------------------------
# 6) Cross-validated RF (caret) using Top N
# -------------------------
ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5
)

set.seed(SEED)
cv_model <- train(
  Region ~ .,
  data = rf_reduced_df,
  method = "rf",
  trControl = ctrl
)

# Save CV results table
write.csv(
  cv_model$results,
  paste0("outputs/tables/cv_results_top_", TOP_N, ".csv"),
  row.names = FALSE
)

# Save model objects
saveRDS(rf_full,    "outputs/models/rf_full.rds")
saveRDS(rf_reduced, paste0("outputs/models/rf_reduced_top_", TOP_N, ".rds"))
saveRDS(cv_model,   paste0("outputs/models/cv_model_top_", TOP_N, ".rds"))

# Print summary to console
print(rf_full)
print(rf_reduced)
print(cv_model)
