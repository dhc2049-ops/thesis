suppressPackageStartupMessages(library(haven))

FILES <- list(
  "Profile A5" = "data/ZA7961_wa5_sA_v1-0-0.dta",
  "Wave 21"    = "data/ZA6838_w21_sA_v6-0-0.dta",
  "Wave 10"    = "data/ZA6838_w10_sA_v6-0-0.dta"
)

KEYWORDS <- paste(
  "erwerb", "beruf", "arbeit", "t.+tig", "besch.+ftigt",
  "einkommen", "gehalt", "haushaltseinkommen", "nettoeinkommen",
  sep = "|"
)

for (label in names(FILES)) {
  df  <- read_dta(FILES[[label]])
  nms <- names(df)
  labs <- vapply(nms, function(v) {
    l <- attr(df[[v]], "label")
    if (is.null(l)) "" else as.character(l)
  }, character(1))

  hits <- which(grepl(KEYWORDS, labs, ignore.case = TRUE))

  cat(strrep("═", 65), "\n")
  cat(label, "\n")
  cat(strrep("═", 65), "\n")

  if (length(hits) == 0) {
    cat("  No matches found.\n\n")
    next
  }

  for (i in hits) {
    v    <- nms[i]
    vals <- suppressWarnings(head(as.numeric(df[[v]]), 5))
    cat(sprintf("  %-28s  %s\n", v, labs[i]))
    cat(sprintf("  %28s  values: %s\n", "", paste(vals, collapse = " ")))
  }
  cat("\n")
}
