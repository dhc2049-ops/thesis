library(haven)

w28     <- read_dta("data/ZA10117_w28_sA_v2-0-0.dta")
profile <- read_dta("data/ZA7961_wa5_sA_v1-0-0.dta")

# Extract variable labels into a data frame
var_labels <- function(df) {
  nms  <- names(df)
  labs <- vapply(nms, function(v) {
    lbl <- attr(df[[v]], "label")
    if (is.null(lbl)) "" else as.character(lbl)
  }, character(1))
  data.frame(variable = nms, label = labs, stringsAsFactors = FALSE)
}

w28_meta  <- var_labels(w28)
prof_meta <- var_labels(profile)

search <- function(meta, pattern, title) {
  hits <- meta[grepl(pattern, meta$label, ignore.case = TRUE) |
               grepl(pattern, meta$variable, ignore.case = TRUE), ]
  cat(sprintf("\n--- %s ---\n", title))
  if (nrow(hits) == 0) {
    cat("  (no matches)\n")
  } else {
    for (i in seq_len(nrow(hits)))
      cat(sprintf("  %-25s  %s\n", hits$variable[i], hits$label[i]))
  }
  invisible(hits)
}

# ── Wave 28: find vote intention ──────────────────────────────────────────────
cat("══════════════════════════════════════════════════════════\n")
cat("WAVE 28 — searching for vote intention variable\n")
cat("══════════════════════════════════════════════════════════\n")

search(w28_meta, "zweitstimme",          "Zweitstimme")
search(w28_meta, "stimme|wahlabsicht|vote intention|191|190|192|193",
                                         "vote / Wahlabsicht / nearby codes")
search(w28_meta, "wen.*w.+hlen|w.+hlen.*partei|w.+hlen sie",
                                         "wen würden Sie wählen")

# Also dump all vars near kp28_170 – kp28_210 to see what's there
cat("\n--- Wave 28 variables kp28_170 through kp28_220 (window around vote) ---\n")
window <- w28_meta[grepl("^kp28_(1[789]\\d|2[012]\\d)$", w28_meta$variable), ]
for (i in seq_len(nrow(window)))
  cat(sprintf("  %-25s  %s\n", window$variable[i], window$label[i]))

# ── Profile A5: find demographics ────────────────────────────────────────────
cat("\n══════════════════════════════════════════════════════════\n")
cat("PROFILE A5 — demographic variable search\n")
cat("══════════════════════════════════════════════════════════\n")

search(prof_meta, "alter|geboren|birth|age|jahrgang",  "Age / birth year")
search(prof_meta, "geschlecht|gender|sex|mann|frau",   "Gender")
search(prof_meta, "bildung|schul|abschluss|ausbildung|educ", "Education")
search(prof_meta, "erwerb|beruf|arbeit|employ|t.+tig", "Employment")
search(prof_meta, "einkommen|income|haushalt.*einko",  "Income")
search(prof_meta, "parteiident|pid|partei.*neig|neig.*partei|party id", "Party ID")
search(prof_meta, "ostwest|ost|west|region|bundesland", "East/West / Region")

# Show all Profile A5 variables (full dump to a text file for review)
cat("\n══════════════════════════════════════════════════════════\n")
cat("Full variable list written to: output/profile_a5_vars.txt\n")
cat("Full variable list written to: output/wave28_vars.txt\n")
cat("══════════════════════════════════════════════════════════\n")

dir.create("output", showWarnings = FALSE)
write.csv(prof_meta, "output/profile_a5_vars.csv", row.names = FALSE)
write.csv(w28_meta,  "output/wave28_vars.csv",     row.names = FALSE)

cat("Done.\n")
