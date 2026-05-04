suppressPackageStartupMessages(library(haven))

MISSING_CODES <- c(-99:-91, -75:-71)
recode_na <- function(x) { x[x %in% MISSING_CODES] <- NA; x }

# Wave 28: compare _190ba vs _190bb for respondents where both are non-NA
w28 <- read_dta("data/ZA10117_w28_sA_v2-0-0.dta")
va  <- recode_na(as.numeric(w28$kp28_190ba))
vb  <- recode_na(as.numeric(w28$kp28_190bb))

both <- !is.na(va) & !is.na(vb)
cat(sprintf("Wave 28 respondents with both non-NA: %d\n", sum(both)))
cat(sprintf("  Identical (va == vb)  : %d\n", sum(both & va == vb, na.rm = TRUE)))
cat(sprintf("  Different (va != vb)  : %d\n", sum(both & va != vb, na.rm = TRUE)))

# Show value labels of _190ba
vl <- attr(w28$kp28_190ba, "labels")
cat("\nValue labels for kp28_190ba:\n")
for (i in seq_along(vl)) cat(sprintf("  %4d = %s\n", vl[i], names(vl)[i]))

cat("\nValue labels for kp28_190bb:\n")
vl2 <- attr(w28$kp28_190bb, "labels")
for (i in seq_along(vl2)) cat(sprintf("  %4d = %s\n", vl2[i], names(vl2)[i]))

# Distribution of _190ba values (post-recode)
cat("\nDistribution of kp28_190ba (valid responses):\n")
print(sort(table(va), decreasing = TRUE))

# Check Wave 31 - what vote-related vars exist?
cat("\n\nWave 31 - variables matching *190* or *vote*:\n")
w31 <- read_dta("data/ZA10120_w31_sA_v1-0-0.dta")
hits <- grep("190|wahlabsicht|zweit", names(w31), ignore.case = TRUE, value = TRUE)
for (v in hits) cat(sprintf("  %-25s  %s\n", v, attr(w31[[v]], "label")))
