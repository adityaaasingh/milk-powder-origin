# =========================
# 1) Libraries
# =========================
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
})

# =========================
# 2) Make folders
# =========================
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# =========================
# 3) Load dataset
# =========================
raw_path <- "data/raw/Trace_elemental_data.xls"
stopifnot(file.exists(raw_path))

df <- readxl::read_xls(raw_path)

# =========================
# 4) LOQ table (ppm)
# =========================
loq_tbl <- data.frame(
  element = c("Li","Be","B","Al","Sc","Ti","V","Cr","Mn","Co","Ni","Ga","Ge","As","Se","Rb","Y",
              "Zr","Nb","Mo","Cd","In","Sn","Sb","Te","Cs","Ba","La","Ce","Pr","Nd","Sm","Eu",
              "Gd","Tb","Dy","Ho","Er","Tm","Yb","Lu","Hf","Ta","W","Hg","Pb","Bi","Th","U",
              "Na","Mg","Al","P","S","K","Ca","Mn","Fe","Zn","Cu"),
  loq_ppm = c(0.040,0.019,0.926,0.216,0.072,0.232,0.548,0.182,0.063,0.002,0.062,0.003,0.069,
              0.142,0.673,0.015,0.000,0.010,0.006,0.012,0.004,0.006,0.161,0.004,0.012,0.002,
              0.006,0.001,0.002,0.000,0.004,0.000,0.000,0.001,0.001,0.001,0.001,0.002,0.001,
              0.002,0.000,0.001,0.002,0.016,0.018,0.003,0.003,0.003,0.001,0.038,0.002,0.031,
              0.009,0.015,0.519,0.098,0.017,0.069,0.002,0.015)
)

# If there are duplicate elements in loq_tbl (Al, Mn), keep first occurrence
loq_tbl <- loq_tbl %>%
  group_by(element) %>%
  summarise(loq_ppm = first(loq_ppm), .groups = "drop")

loq_map <- setNames(loq_tbl$loq_ppm, loq_tbl$element)

# =========================
# 5) Identify element columns that exist in df
# =========================
element_cols <- intersect(names(df), names(loq_map))
stopifnot(length(element_cols) > 0)

# =========================
# 6) Replace "<LOQ" with LOQ/2 in each element column
# =========================
for (el in element_cols) {
  x <- df[[el]]
  
  # Only replace if it's character (because "<LOQ" is text)
  if (is.character(x)) {
    is_loq <- str_detect(x, regex("^\\s*<\\s*LOQ\\s*$", ignore_case = TRUE))
    x[is_loq] <- loq_map[[el]] / 2
  }
  
  # Convert to numeric
  df[[el]] <- suppressWarnings(as.numeric(x))
}

# Optional: ensure Region is factor if present
if ("Region" %in% names(df)) df$Region <- as.factor(df$Region)

# =========================
# 7) Sanity checks
# =========================
# Confirm no "<LOQ" strings remain in element columns
still_char <- any(sapply(df[element_cols], is.character))
message("Any element columns still character? ", still_char)

# Count NAs introduced (helps catch weird strings)
na_counts <- sapply(df[element_cols], function(x) sum(is.na(x)))
na_counts <- sort(na_counts, decreasing = TRUE)
write.csv(data.frame(element = names(na_counts), na_count = as.integer(na_counts)),
          "outputs/tables/na_counts_after_loq.csv", row.names = FALSE)

# =========================
# 8) Save processed dataset
# =========================
write.csv(df, "data/processed/trace_elements_processed.csv", row.names = FALSE)
message("Saved: data/processed/trace_elements_processed.csv")