suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

# ── Constants ──────────────────────────────────────────────────────────────────

WAVE_FILES <- list(
  `28` = "data/ZA10117_w28_sA_v2-0-0.dta",
  `29` = "data/ZA10118_w29_sA_v1-0-0.dta",
  `30` = "data/ZA10119_w30_sA_v1-0-0.dta",
  `31` = "data/ZA10120_w31_sA_v1-0-0.dta",
  `32` = "data/ZA10121_w32_sA_v1-0-0.dta",
  `33` = "data/ZA10122_w33_sA_v1-0-1.dta"
)
PROFILE_FILE <- "data/ZA7961_wa5_sA_v1-0-0.dta"
WAVE21_FILE  <- "data/ZA6838_w21_sA_v6-0-0.dta"
WAVE10_FILE  <- "data/ZA6838_w10_sA_v6-0-0.dta"

# Standard GLES non-response / split / out-of-scope codes.
# Includes -86:-81 (not eligible, will not vote, undecided, etc.)
# Does NOT include 808 (= "no party identification" — kept as meaningful category)
MISSING_CODES <- c(-99:-91, -86:-81, -75:-71)

DEMO_VARS_REPORT <- c("eastwest", "birth_year", "gender", "education",
                       "pid", "employment", "income")

# ── Helpers ────────────────────────────────────────────────────────────────────

get_var <- function(df, varname) {
  if (varname %in% names(df)) as.numeric(df[[varname]])
  else rep(NA_real_, nrow(df))
}

recode_na <- function(x) {
  x[x %in% MISSING_CODES] <- NA
  x
}

# Coalesce two split-experiment party ID columns; preserve 808 (no PID)
coalesce_pid <- function(a, b) {
  dplyr::coalesce(recode_na(as.numeric(a)), recode_na(as.numeric(b)))
}

# ── Process one wave ───────────────────────────────────────────────────────────

process_wave <- function(df, wave_num) {
  w  <- as.character(wave_num)
  gv <- function(suffix) recode_na(get_var(df, paste0("kp", w, "_", suffix)))

  # _190ba = simplified party list; _190bb = extended list (minor parties).
  # Both are shown to all respondents; they differ only when _190ba = 801 (andere).
  vote_a <- gv("190ba")
  vote_b <- gv("190bb")

  data.frame(
    lfdn             = as.numeric(df$lfdn),
    wave             = wave_num,
    vote_intention   = dplyr::coalesce(vote_a, vote_b),
    afd_therm        = gv("430i"),
    cdu_therm        = gv("430a"),
    csu_therm        = gv("430b"),
    spd_therm        = gv("430c"),
    fdp_therm        = gv("430d"),
    gru_therm        = gv("430e"),
    lin_therm        = gv("430f"),
    bsw_therm        = gv("430m"),
    lr_self          = gv("1500"),      # rotating — only waves 28, 30, 33
    redist           = gv("1090"),
    immigration      = gv("1130"),
    climate          = gv("1290"),
    dir_right        = gv("2880dc"),    # valence: Germany going in right direction
    min_wage         = gv("2880aj"),
    buergergeld_cut  = gv("2880de"),
    refugee_cap      = gv("2880x"),
    eu_integ         = gv("1250"),
    afd_consider     = gv("221i"),
    dem_sat          = gv("020"),
    pol_interest     = gv("010"),
    stringsAsFactors = FALSE
  )
}

# ── Capture before-state ───────────────────────────────────────────────────────

cat("Reading existing panel_merged.rds for before/after comparison...\n")
old <- readRDS("data/panel_merged.rds")
before_na <- sapply(DEMO_VARS_REPORT, function(v) {
  if (!v %in% names(old)) NA_real_
  else 100 * mean(is.na(old[[v]]))
})
rm(old)

# ── Load and process all waves ─────────────────────────────────────────────────

cat("Processing panel waves...\n")
wave_list <- lapply(names(WAVE_FILES), function(wn) {
  cat(sprintf("  Wave %s ... ", wn))
  out <- process_wave(read_dta(WAVE_FILES[[wn]]), as.integer(wn))
  cat(sprintf("%d rows\n", nrow(out)))
  out
})
panel <- do.call(rbind, wave_list)
cat(sprintf("Combined: %d rows\n\n", nrow(panel)))

# ── Build demographic tables ───────────────────────────────────────────────────

cat("Building demo table: Profile A5...\n")
a5 <- read_dta(PROFILE_FILE)
demo_a5 <- data.frame(
  lfdn         = as.numeric(a5$lfdn),
  eastwest     = recode_na(as.numeric(a5$ostwest)),
  birth_year   = recode_na(as.numeric(a5$kpa5_2290s)),
  gender       = recode_na(as.numeric(a5$kpa5_2280)),
  education    = recode_na(as.numeric(a5$kpa5_2320)),
  employment   = recode_na(as.numeric(a5$kpa5_2340)),
  income       = recode_na(as.numeric(a5$kpa5_2591)),
  pid          = coalesce_pid(a5$kpa5_2090a, a5$kpa5_2090b),
  pid_strength = recode_na(as.numeric(a5$kpa5_2100)),
  stringsAsFactors = FALSE
) |> distinct(lfdn, .keep_all = TRUE)
cat(sprintf("  %d unique respondents\n", nrow(demo_a5)))

cat("Building demo table: Wave 21...\n")
w21 <- read_dta(WAVE21_FILE)
demo_w21 <- data.frame(
  lfdn       = as.numeric(w21$lfdn),
  eastwest   = recode_na(as.numeric(w21$ostwest)),
  birth_year = recode_na(as.numeric(w21$kpx_2290s)),
  gender     = recode_na(as.numeric(w21$kpx_2280)),
  education  = recode_na(as.numeric(w21$kp21_2320)),
  pid        = coalesce_pid(w21$kp21_2090a, w21$kp21_2090b),
  stringsAsFactors = FALSE
) |> distinct(lfdn, .keep_all = TRUE)
cat(sprintf("  %d unique respondents\n", nrow(demo_w21)))

cat("Building demo table: Wave 10...\n")
w10 <- read_dta(WAVE10_FILE)
demo_w10 <- data.frame(
  lfdn       = as.numeric(w10$lfdn),
  eastwest   = recode_na(as.numeric(w10$ostwest)),
  birth_year = recode_na(as.numeric(w10$kpx_2290s)),
  gender     = recode_na(as.numeric(w10$kpx_2280)),
  pid        = coalesce_pid(w10$kp10_2090a, w10$kp10_2090b),
  stringsAsFactors = FALSE
) |> distinct(lfdn, .keep_all = TRUE)
cat(sprintf("  %d unique respondents\n\n", nrow(demo_w10)))

# ── Join all three sources and coalesce ────────────────────────────────────────

cat("Joining and coalescing demographics (A5 → W21 → W10)...\n")

merged <- panel |>
  left_join(rename_with(demo_a5,  \(x) paste0(x, "_a5"),  -lfdn), by = "lfdn") |>
  left_join(rename_with(demo_w21, \(x) paste0(x, "_w21"), -lfdn), by = "lfdn") |>
  left_join(rename_with(demo_w10, \(x) paste0(x, "_w10"), -lfdn), by = "lfdn") |>
  mutate(
    eastwest     = coalesce(eastwest_a5,   eastwest_w21,   eastwest_w10),
    birth_year   = coalesce(birth_year_a5, birth_year_w21, birth_year_w10),
    gender       = coalesce(gender_a5,     gender_w21,     gender_w10),
    education    = coalesce(education_a5,  education_w21),  # not in W10
    employment   = employment_a5,                            # A5 only
    income       = income_a5,                                # A5 only
    pid          = coalesce(pid_a5,        pid_w21,        pid_w10),
    pid_strength = pid_strength_a5,                          # A5 only
    age          = 2025L - as.integer(birth_year)
  ) |>
  select(-ends_with("_a5"), -ends_with("_w21"), -ends_with("_w10"))

cat(sprintf("Merged: %d rows × %d columns\n\n", nrow(merged), ncol(merged)))

# ── Save ───────────────────────────────────────────────────────────────────────

saveRDS(merged, "data/panel_merged.rds")

# ── Before / after NA report ───────────────────────────────────────────────────

cat("══════════════════════════════════════════════════════════\n")
cat("BEFORE / AFTER NA RATES (demographic variables)\n")
cat("══════════════════════════════════════════════════════════\n")
cat(sprintf("%-14s  %8s  %8s  %9s\n", "Variable", "Before", "After", "Change"))
cat(strrep("-", 46), "\n")

n <- nrow(merged)
for (v in DEMO_VARS_REPORT) {
  after  <- 100 * mean(is.na(merged[[v]]))
  before <- before_na[[v]]
  cat(sprintf("%-14s  %7.1f%%  %7.1f%%  %+8.1f%%\n", v, before, after, after - before))
}

n_none <- sum(merged$pid == 808, na.rm = TRUE)
n_pid  <- sum(!is.na(merged$pid))
cat(sprintf("\npid = 808 (no party ID): %d rows (%.1f%% of non-NA pid responses)\n",
            n_none, 100 * n_none / n_pid))
cat(sprintf("pid = NA  (no source)  : %d rows (%.1f%% of total)\n",
            sum(is.na(merged$pid)), 100 * mean(is.na(merged$pid))))
