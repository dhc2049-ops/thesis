suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

dir.create("output", showWarnings = FALSE)

# ── Load data ──────────────────────────────────────────────────────────────────
df <- readRDS("data/panel_merged.rds")

# ── Party lookup ───────────────────────────────────────────────────────────────
PARTY_CODES <- c(`1` = "CDU/CSU", `4` = "SPD", `5` = "FDP",
                 `6` = "Grüne",   `7` = "Linke", `322` = "AfD",
                 `392` = "BSW",   `801` = "Andere")
LEFT_WING   <- c(4, 6, 7, 392)   # SPD, Grüne, Linke, BSW
PARTY_ORDER <- c("CDU/CSU","SPD","FDP","Grüne","Linke","AfD","BSW","Andere","NA/Undecided")

df <- df |>
  mutate(
    party_label = dplyr::recode(as.character(vote_intention),
                                !!!PARTY_CODES, .default = NA_character_),
    party_label = factor(ifelse(is.na(party_label), "NA/Undecided", party_label),
                         levels = PARTY_ORDER),
    left_wing   = vote_intention %in% LEFT_WING,
    lr_bin      = case_when(
      lr_self >= 1 & lr_self <= 4  ~ "Left (1-4)",
      lr_self >= 5 & lr_self <= 7  ~ "Centre (5-7)",
      lr_self >= 8 & lr_self <= 11 ~ "Right (8-11)",
      TRUE ~ NA_character_
    ),
    lr_bin = factor(lr_bin, levels = c("Left (1-4)", "Centre (5-7)", "Right (8-11)"))
  )

sep <- function() cat(strrep("─", 65), "\n")

# ══════════════════════════════════════════════════════════════════
# 1. VOTE INTENTION DISTRIBUTION BY WAVE
# ══════════════════════════════════════════════════════════════════
cat("══════════════════════════════════════════════════════════════════\n")
cat("1. VOTE INTENTION DISTRIBUTION BY WAVE\n")
cat("   (Note: Wave 31 has no vote intention data)\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

vote_table <- df |>
  group_by(wave, party_label) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(wave) |>
  mutate(pct = 100 * n / sum(n)) |>
  ungroup()

for (wv in sort(unique(df$wave))) {
  wv_total <- sum(df$wave == wv)
  cat(sprintf("Wave %d  (N = %d)\n", wv, wv_total))
  wv_dat <- vote_table |> filter(wave == wv) |> arrange(party_label)
  for (i in seq_len(nrow(wv_dat))) {
    cat(sprintf("  %-14s : %5d  (%5.1f%%)\n",
                wv_dat$party_label[i], wv_dat$n[i], wv_dat$pct[i]))
  }
  cat("\n")
}

# ══════════════════════════════════════════════════════════════════
# 2. LEFT-WING SUBSAMPLE SIZE BY WAVE
# ══════════════════════════════════════════════════════════════════
cat("══════════════════════════════════════════════════════════════════\n")
cat("2. LEFT-WING SUBSAMPLE (SPD + Grüne + Linke + BSW) BY WAVE\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

lw_summary <- df |>
  group_by(wave) |>
  summarise(
    n_wave       = n(),
    n_lw         = sum(left_wing, na.rm = TRUE),
    n_spd        = sum(vote_intention == 4,   na.rm = TRUE),
    n_gru        = sum(vote_intention == 6,   na.rm = TRUE),
    n_lin        = sum(vote_intention == 7,   na.rm = TRUE),
    n_bsw        = sum(vote_intention == 392, na.rm = TRUE),
    pct_lw       = 100 * n_lw / n_wave,
    .groups = "drop"
  )

cat(sprintf("%-6s  %7s  %7s  %7s  %7s  %7s  %7s  %7s\n",
            "Wave", "N_wave", "N_lw", "SPD", "Grüne", "Linke", "BSW", "%_lw"))
cat(strrep("-", 65), "\n")
for (i in seq_len(nrow(lw_summary))) {
  r <- lw_summary[i, ]
  cat(sprintf("%-6d  %7d  %7d  %7d  %7d  %7d  %7d  %6.1f%%\n",
              r$wave, r$n_wave, r$n_lw,
              r$n_spd, r$n_gru, r$n_lin, r$n_bsw, r$pct_lw))
}
cat(sprintf("%-6s  %7d  %7d  %7d  %7d  %7d  %7d  %6.1f%%\n",
            "TOTAL",
            sum(lw_summary$n_wave), sum(lw_summary$n_lw),
            sum(lw_summary$n_spd),  sum(lw_summary$n_gru),
            sum(lw_summary$n_lin),  sum(lw_summary$n_bsw),
            100 * sum(lw_summary$n_lw) / sum(lw_summary$n_wave)))
cat("\n")

# ══════════════════════════════════════════════════════════════════
# 3. AfD THERMOMETER DISTRIBUTION
# ══════════════════════════════════════════════════════════════════
cat("══════════════════════════════════════════════════════════════════\n")
cat("3. AfD THERMOMETER (afd_therm, -5 to +5)\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

therm_stats <- function(x, label) {
  x <- x[!is.na(x)]
  cat(sprintf("  %s (N = %d)\n", label, length(x)))
  cat(sprintf("    Mean: %.2f  |  Median: %.1f  |  SD: %.2f\n",
              mean(x), median(x), sd(x)))
  cat(sprintf("    ≤ -3 (strong dislike): %d (%.1f%%)\n",
              sum(x <= -3), 100 * mean(x <= -3)))
  cat(sprintf("    ≥ +3 (strong like)   : %d (%.1f%%)\n\n",
              sum(x >= 3), 100 * mean(x >= 3)))
}

therm_stats(df$afd_therm,                          "Full sample")
therm_stats(df$afd_therm[df$left_wing],            "Left-wing voters only")

# Histograms
plot_dat <- bind_rows(
  df |> filter(!is.na(afd_therm)) |>
    mutate(group = "Full sample"),
  df |> filter(!is.na(afd_therm), left_wing) |>
    mutate(group = "Left-wing voters")
) |>
  mutate(group = factor(group, levels = c("Full sample", "Left-wing voters")))

p_therm <- ggplot(plot_dat, aes(x = afd_therm)) +
  geom_histogram(binwidth = 1, fill = "#2166ac", colour = "white", alpha = 0.85) +
  facet_wrap(~ group, scales = "free_y") +
  scale_x_continuous(breaks = -5:5) +
  labs(title = "AfD feeling thermometer distribution",
       x = "AfD thermometer (-5 = strong dislike, +5 = strong like)",
       y = "Count") +
  theme_bw(base_size = 11) +
  theme(strip.background = element_rect(fill = "#dce9f5"))

ggsave("output/03a_afd_therm_hist.png", p_therm, width = 9, height = 4, dpi = 150)
cat("  Plot saved: output/03a_afd_therm_hist.png\n\n")

# ══════════════════════════════════════════════════════════════════
# 4. POLICY VARIABLE DISTRIBUTIONS — LEFT-WING VOTERS ONLY
# ══════════════════════════════════════════════════════════════════
cat("══════════════════════════════════════════════════════════════════\n")
cat("4. POLICY DISTRIBUTIONS — LEFT-WING VOTERS ONLY\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

lw <- df |> filter(left_wing)

policy_meta <- list(
  redist     = list(var = "redist",     label = "Redistribution (1=tax cuts … 7=more welfare)", lo = "Tax cuts", hi = "More welfare"),
  immigration= list(var = "immigration",label = "Immigration (1=easier … 7=harder)",           lo = "Easier",   hi = "Harder"),
  climate    = list(var = "climate",    label = "Climate (1=climate priority … 7=growth)",      lo = "Climate",  hi = "Growth")
)

for (pm in policy_meta) {
  x <- lw[[pm$var]]
  x <- x[!is.na(x)]
  cat(sprintf("  %s\n", pm$label))
  cat(sprintf("    N=%d  Mean=%.2f  SD=%.2f  Range=[%d-%d]\n",
              length(x), mean(x), sd(x), min(x), max(x)))
  freq <- table(x)
  freq_pct <- round(100 * prop.table(freq), 1)
  cat("    Frequency: ")
  cat(paste(sprintf("%d:%d(%.0f%%)", as.integer(names(freq)), as.integer(freq), freq_pct),
            collapse = "  "))
  cat("\n\n")
}

# Faceted policy histogram
policy_long <- lw |>
  select(redist, immigration, climate) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  filter(!is.na(value)) |>
  mutate(variable = recode(variable,
    redist      = "Redistribution\n(1=tax cuts, 7=welfare)",
    immigration = "Immigration\n(1=easier, 7=harder)",
    climate     = "Climate\n(1=climate, 7=growth)"
  ))

p_policy <- ggplot(policy_long, aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "#4dac26", colour = "white", alpha = 0.85) +
  facet_wrap(~ variable) +
  scale_x_continuous(breaks = 1:7) +
  labs(title = "Policy position distributions — left-wing voters",
       x = "Scale value", y = "Count") +
  theme_bw(base_size = 11) +
  theme(strip.background = element_rect(fill = "#e4f3dc"))

ggsave("output/03b_policy_hist.png", p_policy, width = 10, height = 4, dpi = 150)
cat("  Plot saved: output/03b_policy_hist.png\n\n")

# ══════════════════════════════════════════════════════════════════
# 5. LEFT-RIGHT SELF-PLACEMENT — LEFT-WING VOTERS (W28, W30, W33)
# ══════════════════════════════════════════════════════════════════
cat("══════════════════════════════════════════════════════════════════\n")
cat("5. LEFT-RIGHT SELF-PLACEMENT — LEFT-WING VOTERS (waves 28, 30, 33)\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

lw_lr <- lw |> filter(wave %in% c(28, 30, 33), !is.na(lr_self))

cat(sprintf("  N = %d  (left-wing voters in waves 28, 30, 33 with valid lr_self)\n", nrow(lw_lr)))
cat(sprintf("  Mean: %.2f  |  Median: %.1f  |  SD: %.2f\n",
            mean(lw_lr$lr_self), median(lw_lr$lr_self), sd(lw_lr$lr_self)))

lr_freq <- table(lw_lr$lr_self)
cat("  Frequency by scale point:\n  ")
cat(paste(sprintf("%2d:%d", as.integer(names(lr_freq)), as.integer(lr_freq)), collapse = "  "))
cat("\n\n")

lr_bin_tab <- lw_lr |>
  count(lr_bin) |>
  mutate(pct = 100 * n / sum(n))
cat("  By LR bin:\n")
for (i in seq_len(nrow(lr_bin_tab)))
  cat(sprintf("    %-15s : %d (%.1f%%)\n", lr_bin_tab$lr_bin[i], lr_bin_tab$n[i], lr_bin_tab$pct[i]))
cat("\n")

p_lr <- ggplot(lw_lr, aes(x = lr_self)) +
  geom_histogram(binwidth = 1, fill = "#d01c8b", colour = "white", alpha = 0.85) +
  facet_wrap(~ wave, labeller = label_both) +
  scale_x_continuous(breaks = 1:11) +
  labs(title = "Left-right self-placement — left-wing voters (waves 28, 30, 33)",
       x = "Left-right scale (1=left, 11=right)", y = "Count") +
  theme_bw(base_size = 11) +
  theme(strip.background = element_rect(fill = "#f9e2f2"))

ggsave("output/03c_lr_hist.png", p_lr, width = 10, height = 4, dpi = 150)
cat("  Plot saved: output/03c_lr_hist.png\n\n")

# ══════════════════════════════════════════════════════════════════
# 6. CROSS-TAB: AfD THERM × LR BIN — LEFT-WING VOTERS
# ══════════════════════════════════════════════════════════════════
cat("══════════════════════════════════════════════════════════════════\n")
cat("6. AfD THERMOMETER BY LR BIN — LEFT-WING VOTERS (W28, W30, W33)\n")
cat("   Reactive voter candidates: left-wing vote + centrist/right LR + afd_therm ≤ -3\n")
cat("══════════════════════════════════════════════════════════════════\n\n")

crosstab <- lw_lr |>
  filter(!is.na(lr_bin), !is.na(afd_therm)) |>
  group_by(lr_bin) |>
  summarise(
    n              = n(),
    mean_afd_therm = round(mean(afd_therm), 2),
    median_afd     = median(afd_therm),
    sd_afd         = round(sd(afd_therm), 2),
    pct_strong_neg = round(100 * mean(afd_therm <= -3), 1),  # reactive candidates
    pct_neutral_pos= round(100 * mean(afd_therm >= 0), 1),
    .groups = "drop"
  )

cat(sprintf("%-15s  %6s  %10s  %8s  %6s  %12s  %12s\n",
            "LR bin", "N", "Mean therm", "Median", "SD", "≤-3 (%)", "≥0 (%)"))
cat(strrep("-", 75), "\n")
for (i in seq_len(nrow(crosstab))) {
  r <- crosstab[i, ]
  cat(sprintf("%-15s  %6d  %10.2f  %8.1f  %6.2f  %11.1f%%  %11.1f%%\n",
              r$lr_bin, r$n, r$mean_afd_therm, r$median_afd, r$sd_afd,
              r$pct_strong_neg, r$pct_neutral_pos))
}

# Detailed breakdown: afd_therm distribution by lr_bin (for centrist+right lw voters)
cat("\n  AfD thermometer frequency within LR bins (left-wing voters):\n")
for (bin in levels(lw_lr$lr_bin)) {
  vals <- lw_lr |> filter(lr_bin == bin, !is.na(afd_therm)) |> pull(afd_therm)
  if (length(vals) == 0) next
  freq <- table(factor(vals, levels = -5:5))
  cat(sprintf("\n  %s (N=%d):\n  ", bin, length(vals)))
  cat(paste(sprintf("%+d:%d", as.integer(names(freq)), as.integer(freq)), collapse = "  "))
  cat("\n")
}

# Boxplot: afd_therm by lr_bin
p_cross <- lw_lr |>
  filter(!is.na(lr_bin), !is.na(afd_therm)) |>
  ggplot(aes(x = lr_bin, y = afd_therm, fill = lr_bin)) +
  geom_boxplot(outlier.size = 0.8, alpha = 0.8) +
  geom_hline(yintercept = -3, linetype = "dashed", colour = "firebrick", linewidth = 0.7) +
  annotate("text", x = 0.6, y = -3.3, label = "≤ -3 threshold", colour = "firebrick",
           hjust = 0, size = 3) +
  scale_fill_manual(values = c("Left (1-4)" = "#4dac26",
                                "Centre (5-7)" = "#f4a582",
                                "Right (8-11)" = "#d7191c")) +
  scale_y_continuous(breaks = -5:5) +
  labs(title = "AfD thermometer by LR self-placement — left-wing voters",
       subtitle = "Dashed line = -3 threshold for reactive voter classification",
       x = "Left-right self-placement bin", y = "AfD thermometer",
       fill = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position = "none")

ggsave("output/03d_afd_therm_by_lr.png", p_cross, width = 7, height = 5, dpi = 150)
cat("\n  Plot saved: output/03d_afd_therm_by_lr.png\n")
