suppressPackageStartupMessages(library(haven))
w28 <- read_dta("data/ZA10117_w28_sA_v2-0-0.dta")
hits <- grep("_221", names(w28), value = TRUE)
if (length(hits) == 0) {
  cat("No variable matching *_221* found in Wave 28.\n")
} else {
  for (v in hits) {
    cat(sprintf("%-20s  %s\n", v, attr(w28[[v]], "label")))
    cat(sprintf("  First values: %s\n", paste(head(as.numeric(w28[[v]]), 10), collapse = " ")))
  }
}
