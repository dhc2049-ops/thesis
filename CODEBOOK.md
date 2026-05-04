# GLES Panel Codebook — Reactive Voting Project

## Project Context

Research question: Does the rise of the AfD produce "reactive voters" who choose left-wing parties not out of policy agreement but as the most reliable counter to the radical right?

Data: GLES Panel, Waves 28–33 (Dec 2024 – Nov 2025), supplemented by baseline waves for demographics and party ID.

---

## 1. Vote Intention (Zweitstimme)

| Variable | Waves | Description | Scale | Missing |
|----------|-------|-------------|-------|---------|
| kp{wave}_190ba | 28–33 | Beabsichtigte Stimmabgabe: Zweitstimme (Version A) | SPD, CDU/CSU, Grüne, FDP, AfD, Die Linke, BSW, andere, weiß nicht | -98 = weiß nicht |
| kp{wave}_190bb | 28–33 | Beabsichtigte Stimmabgabe: Zweitstimme (Version B) | SPD, CDU/CSU, Grüne, FDP, AfD, Die Linke, BSW, andere, weiß nicht | -98 = weiß nicht |

**Note:** The codebook originally listed `_190b` but the actual variable names are `_190ba` and `_190bb` — a split survey experiment. Respondents receive either version A or B. When merging, combine with `coalesce(kp{wave}_190ba, kp{wave}_190bb)` after handling missing codes.

## 2. Party Feeling Thermometer (Skalometer)

| Variable | Waves | Description | Scale | Missing |
|----------|-------|-------------|-------|---------|
| kp{wave}_430* | 28–33 | "Was halten Sie ganz allgemein von den verschiedenen Parteien?" | -5 to +5 | -71 = nie gehört |

Suffix–party mapping (confirmed from Wave 28 variable labels):

| Variable | Party |
|----------|-------|
| kp{wave}_430a | CDU |
| kp{wave}_430b | CSU |
| kp{wave}_430c | SPD |
| kp{wave}_430d | FDP |
| kp{wave}_430e | Grüne |
| kp{wave}_430f | Die Linke |
| kp{wave}_430i | **AfD** |
| kp{wave}_430m | BSW |

**Key for this project:** `kp{wave}_430i` is the AfD Skalometer — the primary measure of anti-AfD sentiment. Low (negative) scores = strong dislike.

## 3. Left–Right Self-Placement

| Variable | Waves | Description | Scale | Missing |
|----------|-------|-------------|-------|---------|
| kp{wave}_1500 | 28–33 | "Wo würden Sie sich selbst einordnen?" | 1 (links) – 11 (rechts) | -98 = weiß nicht |

## 4. Policy Positions

### Redistribution / Welfare

| Variable | Waves | Description | Scale |
|----------|-------|-------------|-------|
| kp{wave}_1090 | 28–33 | Niedrigere Steuern vs. mehr Sozialleistungen | 1–7 |
| kp{wave}_2880dc | 28–33 | Alles in allem entwickelt sich Deutschland in die richtige Richtung (valence — NOT income inequality as originally noted) | 1–5 (agreement) |
| kp{wave}_2880aj | 28–33 | Mindestlohn sollte erhöht werden | 1–5 |
| kp{wave}_2880de | 28–33 | Bürgergeld sollte reduziert werden | 1–5 |

### Immigration

| Variable | Waves | Description | Scale |
|----------|-------|-------------|-------|
| kp{wave}_1130 | 28–33 | Zuwanderung erleichtern oder erschweren | 1–7 |
| kp{wave}_2880x | 28–33 | Obergrenze für Flüchtlinge | 1–5 |

### Climate

| Variable | Waves | Description | Scale |
|----------|-------|-------------|-------|
| kp{wave}_1290 | 28–33 | Klimaschutz vs. Wirtschaftswachstum | 1–7 |

### EU Integration

| Variable | Waves | Description | Scale |
|----------|-------|-------------|-------|
| kp{wave}_1250 | 28–33 | Europäische Einigung vorantreiben oder zu weit gegangen | 1–7 |

## 5. Voting Motivation

**No direct "why did you vote for this party" question exists in Waves 28–33.**

Closest proxies:

| Variable | Description |
|----------|-------------|
| kp{wave}_221* | Consideration set — one variable per party, same suffixes as thermometer (e.g. `_221i` = AfD). Scale: 1–4 (1 = definitely consider … 4 = definitely not); -97 = n/a |
| kp{wave}_1100 | Issue importance ranking |

Implication: Reactive voting must be identified indirectly via policy distance × AfD sentiment.

## 6. Party Identification

**Not available in Waves 28–33.** Collected in Profile Wave A5 (ZA7961). Merge key: `lfdn`.

| Variable | File | Description |
|----------|------|-------------|
| kpa5_2090a | Profile A5 | Parteiidentifikation (Version A) — party codes |
| kpa5_2090b | Profile A5 | Parteiidentifikation (Version B) — party codes |
| kpa5_2100  | Profile A5 | Parteiidentifikation, Stärke (strength 1–5) |
| kpa5_2095a | Profile A5 | Multiple Parteiidentifikation (Version A) |
| kpa5_2095b | Profile A5 | Multiple Parteiidentifikation (Version B) |
| kpa5_2101  | Profile A5 | Multiple Parteiidentifikation, Stärke |

**Note:** `_2090a`/`_2090b` are a split experiment; combine with `coalesce()`. Value 808 = no party ID. Party codes follow standard GLES scheme (1=CDU/CSU, 4=SPD, 5=FDP, etc.).

## 7. Satisfaction with Democracy

| Variable | Waves | Description | Scale |
|----------|-------|-------------|-------|
| kp{wave}_020 | 28–33 | Zufriedenheit mit der Demokratie in Deutschland | 1–5 |

## 8. Political Interest

| Variable | Waves | Description | Scale |
|----------|-------|-------------|-------|
| kp{wave}_010 | 28–33 | Wie stark interessieren Sie sich für Politik? | 1–5 |
| kp{wave}_390 | 28–33 | Interesse am Wahlkampf | 1–5 |

## 9. Demographics

**Not repeated in Waves 28–33.** Must be merged from baseline waves.

Available in current waves:

| Variable | Description |
|----------|-------------|
| kp{wave}_780 | Own economic situation |
| kp{wave}_820 | National economy assessment |

Needed from Profile Wave A5 (ZA7961). Merge key: `lfdn`. N = 9,934.

| Variable | Description | Notes |
|----------|-------------|-------|
| `ostwest` | Ost/West | 1 = East (incl. Berlin), 0 = West |
| `kpa5_2290s` | Geburtsjahr (birth year) | Raw year, e.g. 1964. Compute age = survey year − birth year. |
| `kpa5_2280` | Geschlecht (gender) | 1 = male, 2 = female |
| `kpa5_2320` | Schulabschluss (school degree) | Ordinal, see GLES codebook for levels |
| `kpa5_2330` | Berufliche Bildung (vocational training) | Separate from school degree |
| `kpa5_2340` | Erwerbstätigkeit (employment status) | 1 = full-time, 2 = part-time, 10 = retired, etc. |
| `kpa5_2591` | Nettoeinkommen HH (household net income, categorical) | Ordinal income brackets |

---

## Operationalization Notes

### Defining "Reactive Voter"

A left-wing voter (Die Linke, SPD, Grüne, or BSW in `kp{wave}_190ba`/`_190bb`) is classified as a reactive voter if:

1. **High policy distance** from their chosen party: their left-right self-placement (kp{wave}_1500) or issue positions diverge substantially from the party's typical position.
2. **Strong anti-AfD sentiment**: AfD Skalometer score is strongly negative (e.g., -3 or below).

A voter is classified as agenda-driven if:
1. **Low policy distance** from their chosen party.
2. Anti-AfD sentiment is not the dominant feature of their political profile.

### Alternative operationalization (LCA)

Run Latent Class Analysis on: vote intention, left-right placement, AfD Skalometer, policy positions. Let the model identify voter types from data rather than imposing thresholds.

---

## Data Processing Checklist

- [x] Download Waves 28–33 data files
- [x] Download Profile Wave A5 (ZA7961) for demographics and party ID
- [x] Identify respondent ID variable for merging — **`lfdn`** in all files
- [x] Identify vote intention variable — **`kp{wave}_190ba`/`_190bb`** (split experiment, not `_190b`)
- [x] Identify demographic variables in Profile A5 — see Section 9
- [x] Identify party ID variables in Profile A5 — **`kpa5_2090a`/`_2090b`**
- [x] Identify exact AfD Skalometer variable suffix — **`kp{wave}_430i`**
- [x] Verify consideration set variable — **`kp{wave}_221*`** present, one per party, scale 1–4
- [ ] Note: `kp{wave}_1500` (left-right) only present in waves 28, 30, 33 — not a rotating question asked every wave
