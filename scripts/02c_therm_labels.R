suppressPackageStartupMessages(library(haven))
w28 <- read_dta("data/ZA10117_w28_sA_v2-0-0.dta")
therm <- grep("^kp28_430", names(w28), value = TRUE)
for (v in therm) cat(sprintf("  %-15s  %s\n", v, attr(w28[[v]], "label")))
