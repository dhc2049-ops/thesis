library(haven)
library(dplyr)

# File paths
wave_files <- list(
  `28` = "data/ZA10117_w28_sA_v2-0-0.dta",
  `29` = "data/ZA10118_w29_sA_v1-0-0.dta",
  `30` = "data/ZA10119_w30_sA_v1-0-0.dta",
  `31` = "data/ZA10120_w31_sA_v1-0-0.dta",
  `32` = "data/ZA10121_w32_sA_v1-0-0.dta",
  `33` = "data/ZA10122_w33_sA_v1-0-1.dta"
)
profile_file <- "data/ZA7961_wa5_sA_v1-0-0.dta"

cat("==========================================================\n")
cat("STEP 1: Load waves and check respondent ID variable\n")
cat("==========================================================\n\n")

waves <- lapply(wave_files, read_dta)
profile <- read_dta(profile_file)

# Check ID variable in each wave
id_check <- function(df, label) {
  candidates <- grep("^lfdn$|^id$|^respondent|^resp_id|^person_id", names(df),
                     ignore.case = TRUE, value = TRUE)
  cat(sprintf("  %s: n=%d rows, ID candidates: %s\n",
              label, nrow(df),
              if (length(candidates) > 0) paste(candidates, collapse=", ") else "NONE FOUND"))
  # Also show first 5 column names to help spot the ID
  cat(sprintf("    First 10 vars: %s\n", paste(names(df)[1:min(10, ncol(df))], collapse=", ")))
}

cat("Wave ID variable check:\n")
for (wn in names(waves)) id_check(waves[[wn]], paste("Wave", wn))

cat("\nProfile Wave A5 ID variable check:\n")
id_check(profile, "Profile Wave A5")

cat("\n==========================================================\n")
cat("STEP 2: Profile Wave A5 — find demographic variables\n")
cat("==========================================================\n\n")

# Search for demographic variable patterns in profile wave
all_vars <- names(profile)

demo_patterns <- list(
  east_west      = c("ost", "east", "west", "region", "bundesland", "_200", "_210"),
  age            = c("alter", "age", "geburt", "birth", "_3000", "_aage"),
  gender         = c("geschlecht", "gender", "sex", "_2690", "_sex"),
  education      = c("bildung", "schul", "educ", "abschluss", "_2710", "_2720"),
  employment     = c("erwerb", "beruf", "employ", "arbeit", "_2760", "_2770"),
  income         = c("einkommen", "income", "_2740", "_2750"),
  party_id       = c("parteiident", "party_id", "pid", "kpx_", "_2300", "_2310", "_2330")
)

cat("Searching for demographic variables in Profile Wave A5:\n\n")
found_demo <- list()
for (concept in names(demo_patterns)) {
  pattern <- paste(demo_patterns[[concept]], collapse="|")
  matches <- grep(pattern, all_vars, ignore.case = TRUE, value = TRUE)
  found_demo[[concept]] <- matches
  cat(sprintf("  %-15s -> %s\n", concept,
              if (length(matches) > 0) paste(matches, collapse=", ") else "NOT FOUND"))
}

# Show first values for found variables
cat("\nFirst values for found demographic variables:\n")
for (concept in names(found_demo)) {
  vars <- found_demo[[concept]]
  if (length(vars) == 0) next
  for (v in vars[1:min(3, length(vars))]) {
    vals <- head(as.numeric(profile[[v]]), 10)
    cat(sprintf("  %s: %s\n", v, paste(vals, collapse=" ")))
  }
}

cat("\n==========================================================\n")
cat("STEP 3: Wave 28 — check for key variables from CODEBOOK\n")
cat("==========================================================\n\n")

w28 <- waves[["28"]]
w28_vars <- names(w28)

key_patterns <- list(
  vote_intention    = "kp28_190b",
  thermometer       = "kp28_430",   # prefix search
  left_right        = "kp28_1500",
  redistribution    = "kp28_1090",
  immigration       = "kp28_1130",
  climate           = "kp28_1290",
  democracy_sat     = "kp28_020",
  pol_interest      = "kp28_010"
)

cat("Checking CODEBOOK key variables in Wave 28:\n\n")
results <- list()
for (label in names(key_patterns)) {
  pat <- key_patterns[[label]]
  # For thermometer use prefix match, others exact
  if (label == "thermometer") {
    matches <- grep(paste0("^", pat), w28_vars, value = TRUE)
  } else {
    matches <- w28_vars[w28_vars == pat]
  }
  status <- if (length(matches) > 0) "FOUND" else "MISSING"
  results[[label]] <- list(status = status, vars = matches)
  cat(sprintf("  %-20s [%s] %s\n", label, status,
              if (length(matches) > 0) paste(matches, collapse=", ") else ""))
}

# Show the thermometer variable names in full
therm_vars <- results$thermometer$vars
if (length(therm_vars) > 0) {
  cat(sprintf("\n  Thermometer variables (kp28_430*): %s\n",
              paste(therm_vars, collapse=", ")))
}

cat("\n==========================================================\n")
cat("STEP 4: Summary report across all waves\n")
cat("==========================================================\n\n")

cat(sprintf("%-10s %-12s %-10s %s\n", "Wave", "File", "N_rows", "ID_var"))
cat(strrep("-", 60), "\n")

for (wn in names(waves)) {
  df <- waves[[wn]]
  id_var <- if ("lfdn" %in% names(df)) "lfdn" else
            grep("^lfdn$|^id$", names(df), ignore.case=TRUE, value=TRUE)[1]
  cat(sprintf("%-10s %-12s %-10d %s\n",
              paste("Wave", wn),
              wave_files[[wn]],
              nrow(df),
              if (!is.na(id_var)) id_var else "?"))
}

id_var_prof <- if ("lfdn" %in% names(profile)) "lfdn" else
               grep("^lfdn$|^id$", names(profile), ignore.case=TRUE, value=TRUE)[1]
cat(sprintf("%-10s %-12s %-10d %s\n",
            "Profile A5",
            profile_file,
            nrow(profile),
            if (!is.na(id_var_prof)) id_var_prof else "?"))

cat("\n==========================================================\n")
cat("STEP 5: Check key variables across ALL waves\n")
cat("==========================================================\n\n")

key_exact <- c("_190b", "_1500", "_1090", "_1130", "_1290", "_020", "_010")

wave_nums <- names(waves)
header <- sprintf("%-18s", "Variable")
for (wn in wave_nums) header <- paste0(header, sprintf(" %-8s", paste0("W", wn)))
cat(header, "\n")
cat(strrep("-", 18 + 9 * length(wave_nums)), "\n")

for (suf in key_exact) {
  row_str <- sprintf("%-18s", suf)
  for (wn in wave_nums) {
    varname <- paste0("kp", wn, suf)
    found <- varname %in% names(waves[[wn]])
    row_str <- paste0(row_str, sprintf(" %-8s", if (found) "OK" else "MISSING"))
  }
  cat(row_str, "\n")
}

# Thermometer separately
row_str <- sprintf("%-18s", "_430* (therm)")
for (wn in wave_nums) {
  pat <- paste0("^kp", wn, "_430")
  n_found <- sum(grepl(pat, names(waves[[wn]])))
  row_str <- paste0(row_str, sprintf(" %-8s", if (n_found > 0) paste0("OK(", n_found, ")") else "MISSING"))
}
cat(row_str, "\n")

cat("\nDone.\n")
