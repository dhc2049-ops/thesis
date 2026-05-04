suppressPackageStartupMessages(library(haven))
w28 <- read_dta("data/ZA10117_w28_sA_v2-0-0.dta")

targets <- c("kp28_2880dc", "kp28_2880aj", "kp28_2880de", "kp28_2880x", "kp28_1250")

for (v in targets) {
  if (v %in% names(w28)) {
    x <- w28[[v]]
    cat(sprintf("FOUND   %-18s  label: %s\n", v, attr(x, "label")))
    cat(sprintf("        values: %s\n\n", paste(head(as.numeric(x), 10), collapse = " ")))
  } else {
    # Find nearest match by stripping the wave number prefix
    suffix  <- sub("^kp28_", "", v)
    pattern <- paste0("kp28_", suffix)
    # Try prefix match in case of slight suffix variation
    close   <- grep(paste0("^kp28_", substr(suffix, 1, 6)), names(w28), value = TRUE)
    cat(sprintf("MISSING %-18s  closest matches: %s\n\n", v,
                if (length(close) > 0) paste(close, collapse = ", ") else "none"))
  }
}
