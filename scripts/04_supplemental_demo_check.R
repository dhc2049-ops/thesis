suppressPackageStartupMessages(library(haven))

SUPP_FILES <- list(
  "Wave 10" = "data/ZA6838_w10_sA_v6-0-0.dta",
  "Wave 21" = "data/ZA6838_w21_sA_v6-0-0.dta"
)

DEMO_PATTERNS <- list(
  east_west  = c("ostwest", "2100"),
  birth_year = c("2290", "geburt"),
  gender     = c("2280", "geschl"),
  education  = c("2320", "schul"),
  employment = c("2340", "erwerb"),
  income     = c("2591", "eink"),
  party_id   = c("2090")
)

# Load merged panel to get lfdn values missing demographics
panel <- readRDS("data/panel_merged.rds")
missing_demo_lfdn <- unique(panel$lfdn[is.na(panel$eastwest)])
cat(sprintf("Respondents in panel_merged with NA ostwest: %d unique lfdn values\n\n",
            length(missing_demo_lfdn)))

for (label in names(SUPP_FILES)) {
  path <- SUPP_FILES[[label]]
  df   <- read_dta(path)
  nms  <- names(df)

  cat(strrep("═", 65), "\n")
  cat(sprintf("%s  |  file: %s\n", label, basename(path)))
  cat(sprintf("Respondents: %d rows\n", nrow(df)))

  # ID variable
  id_vars <- grep("^lfdn$", nms, ignore.case = TRUE, value = TRUE)
  has_lfdn <- length(id_vars) > 0
  cat(sprintf("lfdn present: %s\n", if (has_lfdn) "YES" else "NO"))

  cat("\nDemographic variable search:\n")
  found_any <- FALSE
  for (concept in names(DEMO_PATTERNS)) {
    pats  <- DEMO_PATTERNS[[concept]]
    regex <- paste(pats, collapse = "|")
    hits  <- grep(regex, nms, ignore.case = TRUE, value = TRUE)
    if (length(hits) == 0) {
      cat(sprintf("  %-14s -> NOT FOUND\n", concept))
    } else {
      found_any <- TRUE
      for (v in hits) {
        lbl  <- attr(df[[v]], "label")
        vals <- head(as.numeric(df[[v]]), 5)
        cat(sprintf("  %-14s -> %-30s [%s]\n              values: %s\n",
                    concept, v,
                    if (is.null(lbl)) "" else lbl,
                    paste(vals, collapse = " ")))
      }
    }
  }

  # Coverage check
  if (has_lfdn) {
    file_lfdn   <- as.numeric(df$lfdn)
    overlap     <- sum(missing_demo_lfdn %in% file_lfdn)
    pct         <- 100 * overlap / length(missing_demo_lfdn)
    cat(sprintf("\nCoverage: %d of %d panel respondents missing demographics are in this file (%.1f%%)\n",
                overlap, length(missing_demo_lfdn), pct))
  }
  cat("\n")
}
