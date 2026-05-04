suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

dir.create("output", showWarnings = FALSE)

# ── Constants ──────────────────────────────────────────────────────────────────
LEFT_WING    <- c(4L, 6L, 7L, 392L)          # SPD, Grüne, Linke, BSW
PARTY_LABELS <- c(`4`="SPD",`6`="Gruene",`7`="Linke",`392`="BSW")

# Approximate party positions on 1-7 policy scales
# Sources: party manifesto positions / expert surveys
PARTY_POS <- data.frame(
  vote_intention = c(4L,  6L,  7L,  392L),
  pos_redist     = c(5,   5,   6,   5),     # 1=tax cuts, 7=more welfare
  pos_immigration= c(4,   2,   2,   6),     # 1=easier, 7=harder
  pos_climate    = c(3,   1,   2,   4),     # 1=climate priority, 7=growth
  stringsAsFactors = FALSE
)

sep <- function(title) {
  cat("══════════════════════════════════════════════════════════════════\n")
  cat(title, "\n")
  cat("══════════════════════════════════════════════════════════════════\n\n")
}

# ── Load data ──────────────────────────────────────────────────────────────────
df <- readRDS("data/panel_merged.rds")

# ══════════════════════════════════════════════════════════════════
# STEP 1: LEFT-WING SUBSAMPLE
# ══════════════════════════════════════════════════════════════════
sep("STEP 1: LEFT-WING SUBSAMPLE")

lw <- df |>
  filter(
    vote_intention %in% LEFT_WING,
    wave != 31,
    !is.na(redist), !is.na(immigration), !is.na(climate)
  ) |>
  mutate(
    party = factor(PARTY_LABELS[as.character(vote_intention)],
                   levels = c("SPD", "Gruene", "Linke", "BSW"))
  )

cat(sprintf("Total observations (policy vars complete): %d\n", nrow(lw)))
cat(sprintf("Unique respondents: %d\n", length(unique(lw$lfdn))))
cat(sprintf("Waves included: %s\n\n", paste(sort(unique(lw$wave)), collapse=", ")))

n_party <- lw |> count(party)
for (i in seq_len(nrow(n_party)))
  cat(sprintf("  %-8s : %d (%.1f%%)\n", n_party$party[i], n_party$n[i], 100*n_party$n[i]/nrow(lw)))

# ══════════════════════════════════════════════════════════════════
# STEP 2: POLICY DISTANCE
# ══════════════════════════════════════════════════════════════════
sep("\nSTEP 2: POLICY DISTANCE (Euclidean, 3-variable)")

lw <- lw |>
  left_join(PARTY_POS, by = "vote_intention") |>
  mutate(
    dist_redist     = redist      - pos_redist,
    dist_immigration= immigration - pos_immigration,
    dist_climate    = climate     - pos_climate,
    policy_distance = sqrt(dist_redist^2 + dist_immigration^2 + dist_climate^2)
  )

cat("Overall policy distance:\n")
cat(sprintf("  Mean=%.2f  Median=%.2f  SD=%.2f  Range=[%.2f, %.2f]\n\n",
            mean(lw$policy_distance), median(lw$policy_distance),
            sd(lw$policy_distance), min(lw$policy_distance), max(lw$policy_distance)))

cat("Policy distance by party (mean ± SD):\n")
lw |>
  group_by(party) |>
  summarise(mean=round(mean(policy_distance),2), sd=round(sd(policy_distance),2),
            n=n(), .groups="drop") |>
  with(for (i in seq_along(party))
    cat(sprintf("  %-8s : %.2f ± %.2f  (N=%d)\n", party[i], mean[i], sd[i], n[i])))

cat("\nMean signed distances from party position (negative = more left/pro-climate/etc.):\n")
cat(sprintf("  Redistribution : %.2f\n", mean(lw$dist_redist,      na.rm=TRUE)))
cat(sprintf("  Immigration    : %.2f\n", mean(lw$dist_immigration, na.rm=TRUE)))
cat(sprintf("  Climate        : %.2f\n", mean(lw$dist_climate,     na.rm=TRUE)))

# ══════════════════════════════════════════════════════════════════
# STEP 3: CLASSIFY REACTIVE VOTERS
# ══════════════════════════════════════════════════════════════════
sep("\nSTEP 3: CLASSIFICATION")

# Within-wave median split (as specified)
lw <- lw |>
  group_by(wave) |>
  mutate(
    wave_median = median(policy_distance),
    reactive_voter = as.integer(policy_distance > wave_median)
  ) |>
  ungroup()

# Global median for reference / robustness
global_med <- median(lw$policy_distance)
lw <- lw |> mutate(reactive_global = as.integer(policy_distance > global_med))

cat(sprintf("Global median policy distance: %.2f\n\n", global_med))
cat("Within-wave medians and resulting classification:\n")
cat(sprintf("  %-6s  %8s  %6s  %10s\n", "Wave", "Median", "N", "% Reactive"))
cat(strrep("-", 38), "\n")
lw |>
  group_by(wave) |>
  summarise(med=first(wave_median), n=n(), pct=100*mean(reactive_voter), .groups="drop") |>
  with(for (i in seq_along(wave))
    cat(sprintf("  %-6d  %8.2f  %6d  %9.1f%%\n", wave[i], med[i], n[i], pct[i])))

cat(sprintf("\n  Note: within-wave median split forces ~50%% reactive per wave.\n"))
cat(sprintf("  Use reactive_global (global median=%.2f) for time trend analysis.\n\n",
            global_med))

# ══════════════════════════════════════════════════════════════════
# STEP 4: DESCRIPTIVE COMPARISON
# ══════════════════════════════════════════════════════════════════
sep("\nSTEP 4: REACTIVE vs AGENDA-DRIVEN COMPARISON")

compare <- function(label, reactive, agenda, fmt="%.2f") {
  cat(sprintf("  %-40s  reactive=%-10s  agenda=%-10s\n",
              label,
              sprintf(fmt, reactive),
              sprintf(fmt, agenda)))
}

rv1 <- lw |> filter(reactive_voter == 1)
rv0 <- lw |> filter(reactive_voter == 0)

cat(sprintf("N reactive=1: %d  |  N reactive=0: %d\n\n", nrow(rv1), nrow(rv0)))

# Policy positions
cat("--- Policy positions ---\n")
compare("Redistribution (1-7)",   mean(rv1$redist,      na.rm=TRUE), mean(rv0$redist,      na.rm=TRUE))
compare("Immigration (1-7)",      mean(rv1$immigration, na.rm=TRUE), mean(rv0$immigration, na.rm=TRUE))
compare("Climate (1-7)",          mean(rv1$climate,     na.rm=TRUE), mean(rv0$climate,     na.rm=TRUE))
compare("Policy distance (Eucl)", mean(rv1$policy_distance),        mean(rv0$policy_distance))

# AfD thermometer and LR
cat("\n--- Core attitudes ---\n")
compare("AfD therm (-5 to +5)",   mean(rv1$afd_therm, na.rm=TRUE), mean(rv0$afd_therm, na.rm=TRUE))
lw_lr <- lw |> filter(wave %in% c(28,30,33))
rv1_lr <- lw_lr |> filter(reactive_voter==1)
rv0_lr <- lw_lr |> filter(reactive_voter==0)
compare("LR self-placement (1-11) [W28,30,33]",
        mean(rv1_lr$lr_self, na.rm=TRUE), mean(rv0_lr$lr_self, na.rm=TRUE))
compare("Democracy satisfaction (1-5)",
        mean(rv1$dem_sat,      na.rm=TRUE), mean(rv0$dem_sat,      na.rm=TRUE))
compare("Political interest (1-5)",
        mean(rv1$pol_interest, na.rm=TRUE), mean(rv0$pol_interest, na.rm=TRUE))

# Demographics
cat("\n--- Demographics (from merged profile data) ---\n")
compare("% East Germany",     100*mean(rv1$eastwest==1, na.rm=TRUE), 100*mean(rv0$eastwest==1, na.rm=TRUE), "%.1f%%")
compare("Mean age",           mean(rv1$age, na.rm=TRUE), mean(rv0$age, na.rm=TRUE))
compare("% Female",           100*mean(rv1$gender==2, na.rm=TRUE), 100*mean(rv0$gender==2, na.rm=TRUE), "%.1f%%")
compare("Mean education (1-5)",mean(rv1$education, na.rm=TRUE), mean(rv0$education, na.rm=TRUE))
cat(sprintf("  N with demographic data: reactive=%d (%.0f%%), agenda=%d (%.0f%%)\n",
            sum(!is.na(rv1$age)), 100*mean(!is.na(rv1$age)),
            sum(!is.na(rv0$age)), 100*mean(!is.na(rv0$age))))

# Party distribution
cat("\n--- Party distribution ---\n")
cat(sprintf("  %-8s  %8s  %8s  %10s  %12s\n", "Party", "N_react", "N_agenda", "%_reactive", "(global)"))
cat(strrep("-", 56), "\n")
lw |>
  group_by(party) |>
  summarise(
    n_r = sum(reactive_voter),
    n_a = sum(1-reactive_voter),
    pct = 100*mean(reactive_voter),
    pct_g = 100*mean(reactive_global),
    .groups="drop"
  ) |>
  with(for (i in seq_along(party))
    cat(sprintf("  %-8s  %8d  %8d  %9.1f%%  %10.1f%%\n",
                party[i], n_r[i], n_a[i], pct[i], pct_g[i])))

# ══════════════════════════════════════════════════════════════════
# STEP 5: TIME TREND
# ══════════════════════════════════════════════════════════════════
sep("\nSTEP 5: TIME TREND BY WAVE")

trend <- lw |>
  group_by(wave) |>
  summarise(
    n                 = n(),
    mean_dist         = mean(policy_distance),
    sd_dist           = sd(policy_distance),
    pct_reactive_wave = 100*mean(reactive_voter),    # within-wave split
    pct_reactive_glob = 100*mean(reactive_global),   # global split
    mean_afd_therm    = mean(afd_therm, na.rm=TRUE),
    .groups="drop"
  )

cat(sprintf("%-6s  %6s  %10s  %8s  %14s  %14s\n",
            "Wave","N","Mean_dist","SD_dist","% React(wave)","% React(glob)"))
cat(strrep("-", 65), "\n")
for (i in seq_len(nrow(trend)))
  cat(sprintf("%-6d  %6d  %10.2f  %8.2f  %13.1f%%  %13.1f%%\n",
              trend$wave[i], trend$n[i], trend$mean_dist[i], trend$sd_dist[i],
              trend$pct_reactive_wave[i], trend$pct_reactive_glob[i]))

# ══════════════════════════════════════════════════════════════════
# PLOTS
# ══════════════════════════════════════════════════════════════════

# 4a: Policy distance histogram by reactive status
p4a <- lw |>
  mutate(Type = ifelse(reactive_voter==1, "Reactive (high distance)", "Agenda-driven (low distance)")) |>
  ggplot(aes(x=policy_distance, fill=Type)) +
  geom_histogram(binwidth=0.5, colour="white", alpha=0.8, position="identity") +
  geom_vline(data = lw |> group_by(wave) |> summarise(med=first(wave_median)),
             aes(xintercept=med), linetype="dashed", colour="grey30") +
  facet_wrap(~wave, labeller=label_both, nrow=2) +
  scale_fill_manual(values=c("Reactive (high distance)"="#d7191c",
                              "Agenda-driven (low distance)"="#2c7bb6")) +
  labs(title="Policy distance distribution by wave",
       subtitle="Dashed line = within-wave median threshold",
       x="Euclidean policy distance from chosen party", y="Count", fill=NULL) +
  theme_bw(base_size=10) +
  theme(legend.position="bottom", strip.background=element_rect(fill="#eeeeee"))
ggsave("output/04a_policy_dist_hist.png", p4a, width=10, height=6, dpi=150)

# 4b: Policy distance by party (boxplot)
p4b <- lw |>
  ggplot(aes(x=party, y=policy_distance, fill=party)) +
  geom_boxplot(outlier.size=0.6, alpha=0.8) +
  geom_hline(yintercept=global_med, linetype="dashed", colour="grey30") +
  annotate("text", x=0.6, y=global_med+0.15, label=sprintf("Global median=%.2f", global_med),
           hjust=0, size=3, colour="grey30") +
  scale_fill_manual(values=c("SPD"="#e63946","Gruene"="#2d6a4f","Linke"="#7b2d8b","BSW"="#f4a261")) +
  labs(title="Policy distance from party position by party",
       x=NULL, y="Euclidean policy distance", fill=NULL) +
  theme_bw(base_size=11) + theme(legend.position="none")
ggsave("output/04b_dist_by_party.png", p4b, width=6, height=4, dpi=150)

# 4c: Time trend — mean policy distance + % reactive (global)
p4c <- ggplot(trend, aes(x=wave)) +
  geom_line(aes(y=mean_dist, colour="Mean policy distance"), linewidth=1) +
  geom_point(aes(y=mean_dist, colour="Mean policy distance"), size=2.5) +
  geom_line(aes(y=pct_reactive_glob/10, colour="% Reactive (global, ÷10)"),
            linetype="dashed", linewidth=1) +
  geom_point(aes(y=pct_reactive_glob/10, colour="% Reactive (global, ÷10)"), size=2.5) +
  scale_y_continuous(
    name="Mean policy distance",
    sec.axis=sec_axis(~.*10, name="% Reactive voters (global median)")
  ) +
  scale_colour_manual(values=c("Mean policy distance"="#2166ac",
                                "% Reactive (global, ÷10)"="#d7191c")) +
  scale_x_continuous(breaks=c(28,29,30,32,33)) +
  labs(title="Time trend: policy distance and reactive voter share",
       x="Wave", colour=NULL) +
  theme_bw(base_size=11) +
  theme(legend.position="bottom", axis.title.y.right=element_text(colour="#d7191c"))
ggsave("output/04c_time_trend.png", p4c, width=7, height=4, dpi=150)

# 4d: Policy position comparison reactive vs agenda-driven
policy_comp <- lw |>
  mutate(Type=ifelse(reactive_voter==1,"Reactive","Agenda-driven")) |>
  group_by(Type) |>
  summarise(
    Redistribution = mean(redist,      na.rm=TRUE),
    Immigration    = mean(immigration, na.rm=TRUE),
    Climate        = mean(climate,     na.rm=TRUE),
    .groups="drop"
  ) |>
  pivot_longer(-Type, names_to="Variable", values_to="Mean")

p4d <- ggplot(policy_comp, aes(x=Variable, y=Mean, fill=Type)) +
  geom_col(position="dodge", alpha=0.85, colour="white") +
  geom_hline(yintercept=4, linetype="dotted", colour="grey50") +
  scale_fill_manual(values=c("Reactive"="#d7191c","Agenda-driven"="#2c7bb6")) +
  scale_y_continuous(breaks=1:7, limits=c(0,7)) +
  labs(title="Mean policy positions: reactive vs agenda-driven",
       subtitle="Scale 1-7; dotted line = midpoint (4)",
       x=NULL, y="Mean scale value", fill=NULL) +
  theme_bw(base_size=11) +
  theme(legend.position="bottom")
ggsave("output/04d_policy_comparison.png", p4d, width=6, height=4, dpi=150)

# 4e: Scatter — policy distance vs AfD thermometer (coloured by LR bin, W28/30/33 only)
p4e <- lw |>
  filter(wave %in% c(28,30,33), !is.na(afd_therm), !is.na(lr_self)) |>
  mutate(lr_bin = case_when(
    lr_self <= 4  ~ "Left (1-4)",
    lr_self <= 7  ~ "Centre (5-7)",
    TRUE          ~ "Right (8-11)"
  ) |> factor(levels=c("Left (1-4)","Centre (5-7)","Right (8-11)"))) |>
  ggplot(aes(x=policy_distance, y=afd_therm, colour=lr_bin)) +
  geom_point(alpha=0.25, size=0.8) +
  geom_smooth(method="lm", se=TRUE, linewidth=1) +
  geom_hline(yintercept=-3, linetype="dashed", colour="grey40") +
  geom_vline(xintercept=global_med, linetype="dashed", colour="grey40") +
  scale_colour_manual(values=c("Left (1-4)"="#4dac26","Centre (5-7)"="#f4a582","Right (8-11)"="#d01c8b")) +
  scale_y_continuous(breaks=-5:5) +
  annotate("text", x=global_med+0.1, y=4.8, label="Global median", size=3, colour="grey40", hjust=0) +
  annotate("text", x=0.1, y=-2.8, label="≤-3 threshold", size=3, colour="grey40") +
  facet_wrap(~lr_bin) +
  labs(title="Policy distance vs AfD thermometer — left-wing voters (W28, 30, 33)",
       x="Policy distance from chosen party", y="AfD thermometer (-5 to +5)",
       colour="LR bin") +
  theme_bw(base_size=10) + theme(legend.position="none",
                                  strip.background=element_rect(fill="#eeeeee"))
ggsave("output/04e_dist_vs_therm.png", p4e, width=9, height=4, dpi=150)

cat("\nPlots saved:\n")
for (f in list.files("output", pattern="^04", full.names=TRUE)) cat(" ", f, "\n")

# ══════════════════════════════════════════════════════════════════
# SAVE
# ══════════════════════════════════════════════════════════════════
analysis_vars <- c("lfdn","wave","party","vote_intention","policy_distance",
                   "reactive_voter","reactive_global","wave_median",
                   "dist_redist","dist_immigration","dist_climate",
                   "pos_redist","pos_immigration","pos_climate",
                   "afd_therm","lr_self","redist","immigration","climate",
                   "dir_right","min_wage","buergergeld_cut",
                   "afd_consider","dem_sat","pol_interest",
                   "pid","pid_strength","eastwest","birth_year","age",
                   "gender","education","employment","income")

saveRDS(lw |> select(all_of(intersect(analysis_vars, names(lw)))),
        "data/panel_analysis.rds")
cat(sprintf("\nSaved: data/panel_analysis.rds  (%d rows × %d cols)\n",
            nrow(lw), length(intersect(analysis_vars, names(lw)))))
