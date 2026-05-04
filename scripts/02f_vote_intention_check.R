suppressPackageStartupMessages(library(haven))

WAVE_FILES <- list(
  `28` = "data/ZA10117_w28_sA_v2-0-0.dta",
  `29` = "data/ZA10118_w29_sA_v1-0-0.dta",
  `30` = "data/ZA10119_w30_sA_v1-0-0.dta",
  `31` = "data/ZA10120_w31_sA_v1-0-0.dta",
  `32` = "data/ZA10121_w32_sA_v1-0-0.dta",
  `33` = "data/ZA10122_w33_sA_v1-0-1.dta"
)

MISSING_CODES <- c(-99:-91, -75:-71)

recode_na <- function(x) { x[x %in% MISSING_CODES] <- NA; x }

cat(sprintf("%-6s  %-10s  %-10s  %-10s  %-10s  %-12s  %-12s  %s\n",
            "Wave", "N_total", "A_only", "B_only", "Both", "Neither(NA)", "plain_190b", "Notes"))
cat(strrep("-", 100), "\n")

for (wn in names(WAVE_FILES)) {
  df <- read_dta(WAVE_FILES[[wn]])
  n  <- nrow(df)

  va_raw <- if (paste0("kp", wn, "_190ba") %in% names(df)) as.numeric(df[[paste0("kp", wn, "_190ba")]]) else rep(NA_real_, n)
  vb_raw <- if (paste0("kp", wn, "_190bb") %in% names(df)) as.numeric(df[[paste0("kp", wn, "_190bb")]]) else rep(NA_real_, n)

  va <- recode_na(va_raw)
  vb <- recode_na(vb_raw)

  a_only  <- sum(!is.na(va) &  is.na(vb))
  b_only  <- sum( is.na(va) & !is.na(vb))
  both    <- sum(!is.na(va) & !is.na(vb))
  neither <- sum( is.na(va) &  is.na(vb))

  # Check plain _190b (no suffix)
  plain_var  <- paste0("kp", wn, "_190b")
  plain_exists <- plain_var %in% names(df)
  plain_str  <- if (plain_exists) {
    n_valid <- sum(!is.na(recode_na(as.numeric(df[[plain_var]]))))
    paste0("YES (", n_valid, " valid)")
  } else "no"

  # Sanity: raw values in _190ba for first few rows to catch unexpected codes
  raw_sample <- paste(head(va_raw[!is.na(va_raw) & !va_raw %in% MISSING_CODES], 5), collapse = " ")

  cat(sprintf("%-6s  %-10d  %-10d  %-10d  %-10d  %-12d  %-12s  sample_A: %s\n",
              wn, n, a_only, b_only, both, neither, plain_str, raw_sample))
}
