suppressPackageStartupMessages(library(haven))
prof <- read_dta("data/ZA7961_wa5_sA_v1-0-0.dta")

vars <- c(
  "ostwest", "kpa5_2290s", "kpa5_2280",
  "kpa5_2320", "kpa5_2330", "kpa5_2340",
  "kpa5_2591", "kpa5_2090a", "kpa5_2090b", "kpa5_2100"
)

for (v in vars) {
  if (!v %in% names(prof)) { cat(v, ": NOT FOUND\n\n"); next }
  x    <- prof[[v]]
  lbl  <- attr(x, "label")
  vals <- head(as.numeric(x), 10)
  vl   <- attr(x, "labels")
  vl_str <- if (!is.null(vl))
    paste(paste(vl, names(vl), sep = "="), collapse = ", ")
  else ""
  cat(sprintf("%s [%s]\n  values: %s\n  value labels: %s\n\n",
              v, lbl,
              paste(vals, collapse = " "),
              substr(vl_str, 1, 130)))
}
