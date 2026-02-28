# scripts/utils.R
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(here)
})

ensure_dirs <- function(paths) {
  for (p in paths) {
    if (!dir.exists(p)) dir.create(p, recursive = TRUE)
  }
}

# Build LOQ map; handle duplicates (Al, Mn appear twice in your table)
make_loq_map <- function(loq_tbl) {
  # keep first occurrence per element (explicit + reproducible)
  loq_tbl_clean <- loq_tbl %>%
    group_by(element) %>%
    summarise(loq_ppm = first(loq_ppm), .groups = "drop")
  
  dupes <- loq_tbl$element[duplicated(loq_tbl$element)]
  if (length(dupes) > 0) {
    message("Duplicate elements found in LOQ table (kept first): ",
            paste(unique(dupes), collapse = ", "))
  }
  
  setNames(loq_tbl_clean$loq_ppm, loq_tbl_clean$element)
}

# Replace <LOQ (or "< LOQ") with LOQ/2 in element columns found in loq_map
replace_loq_half <- function(df, loq_map, region_col = "Region") {
  element_cols <- intersect(names(df), names(loq_map))
  
  df[element_cols] <- lapply(element_cols, function(el) {
    x <- df[[el]]
    
    # Only try to replace if column might be character
    if (is.character(x)) {
      is_loq <- str_detect(x, regex("^\\s*<\\s*LOQ\\s*$", ignore_case = TRUE))
      x[is_loq] <- loq_map[[el]] / 2
    }
    
    suppressWarnings(as.numeric(x))
  })
  
  # Ensure Region is a factor if present
  if (region_col %in% names(df)) {
    df[[region_col]] <- as.factor(df[[region_col]])
  }
  
  list(df = df, element_cols = element_cols)
}

# Convenience: save ggplot
save_plot <- function(p, filename, width = 10, height = 6, dpi = 300) {
  ggplot2::ggsave(filename, plot = p, width = width, height = height, dpi = dpi)
}

# Simple check: any character columns among element cols?
any_element_char <- function(df, element_cols) {
  any(sapply(df[element_cols], is.character))
}
