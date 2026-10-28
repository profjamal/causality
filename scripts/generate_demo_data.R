# Generate the synthetic cohort used in the book practicals.
# The values are fictional and must not be used for clinical decisions.

set.seed(8311)
n_clinics <- 30
n_per_clinic <- 100
n <- n_clinics * n_per_clinic

clinic_frame <- data.frame(
  clinic_id = seq_len(n_clinics),
  clinic_support = rnorm(n_clinics, 0, 1),
  clinic_deprivation = rnorm(n_clinics, 0, 1)
)

demo <- merge(
  data.frame(
    id = seq_len(n),
    clinic_id = rep(seq_len(n_clinics), each = n_per_clinic)
  ),
  clinic_frame,
  by = "clinic_id",
  sort = FALSE
)
demo <- demo[order(demo$id), ]

demo$age <- round(pmin(65, pmax(30, rnorm(n, 47, 8))), 1)
demo$sex <- factor(rbinom(n, 1, 0.46), labels = c("female", "male"))
# Internal centred log-income variable; not a measured or exported SES scale.
# Exponentiating later creates positive annual RM/person values.
log_income <- round(
  rnorm(n, -0.35 * demo$clinic_deprivation, 1), 2
)
demo$asbestos_exposure <- rbinom(
  n, 1,
  plogis(-1.7 + 0.45 * (demo$sex == "male") -
           0.25 * log_income)
)

# Included only so the true data-generating process is documented.
# It is deliberately omitted from the main adjusted analyses.
demo$unmeasured_health_risk <- rbinom(
  n, 1, plogis(-0.8 - 0.3 * log_income)
)

nicotine_dependence <- rnorm(
  n,
  0.25 * demo$clinic_deprivation -
    0.25 * log_income +
    0.35 * demo$unmeasured_health_risk,
  1
)
demo$cigarettes_per_day <- round(pmax(1, 16 + 5 * nicotine_dependence + rnorm(n, 0, 3)))
demo$morning_urgency <- round(pmin(10, pmax(0, 5 + 2 * nicotine_dependence + rnorm(n))), 1)
demo$withdrawal_score <- round(pmin(20, pmax(0, 9 + 3 * nicotine_dependence + rnorm(n, 0, 2))), 1)
demo$pack_yr <- round(pmax(
  0,
  18 + 0.35 * (demo$age - 30) + 6 * nicotine_dependence + rnorm(n, 0, 6)
), 1)

demo$cessation <- rbinom(
  n, 1,
  plogis(-0.1 + 0.65 * demo$clinic_support +
           0.25 * log_income -
           0.7 * nicotine_dependence -
           0.25 * demo$unmeasured_health_risk)
)
# cessation = 1 means seven-day abstinence achieved at the baseline visit.
# It does NOT mean guaranteed sustained abstinence: relapse can occur later.
# At months 1-12, simulate the reported average cigarettes/day for that month.
# A monthly value of zero represents no smoking throughout that month.
# These are fictional monthly reports, not a validated questionnaire or biomarker.
smoking <- 1 - demo$cessation
monthly <- matrix(0, nrow = n, ncol = 12)
for (month in 1:12) {
  relapse <- rbinom(n, 1, plogis(-2.8 + 0.45 * nicotine_dependence))
  quit <- rbinom(n, 1, plogis(-4.2 - 0.30 * nicotine_dependence))
  smoking <- ifelse(smoking == 0, relapse, 1 - quit)
  intensity <- pmax(1, demo$cigarettes_per_day + rnorm(n, 0, 2.5))
  monthly[, month] <- ifelse(smoking == 1, round(intensity, 1), 0)
}
colnames(monthly) <- sprintf("cigs_m%02d", 1:12)
demo <- cbind(demo, as.data.frame(monthly))
# Equal-month mean of the 12 monthly average daily consumption values.
# Retain the existing column name for compatibility; its unit is now cigarettes/day.
demo$later_tobacco_exposure <- round(rowMeans(monthly), 1)
# Cancer is generated AFTER the first-year measurement window, through year 20.

demo$lung_cancer_complete <- rbinom(
  n, 1,
  plogis(-4.1 - 0.20 * demo$cessation +
           0.095 * demo$later_tobacco_exposure +
           0.035 * demo$pack_yr +
           0.50 * demo$asbestos_exposure -
           0.30 * demo$cessation * demo$asbestos_exposure +
           0.018 * (demo$age - 45) +
           0.55 * demo$unmeasured_health_risk)
)

demo$followup_status <- rbinom(
  n, 1,
  plogis(1.8 + 0.35 * demo$cessation -
           0.025 * demo$pack_yr -
           0.50 * demo$unmeasured_health_risk)
)
demo$lung_cancer <- demo$lung_cancer_complete
demo$lung_cancer[demo$followup_status == 0] <- NA

# Annual household-income teaching protocol (synthetic, not Malaysian norms).
# An individual's latent log-income sets annual income per person.
# A separate seed for household size preserves all earlier smoking/cancer draws.
demo$income_pc <- round(30000 * exp(0.5 * log_income))
set.seed(8312)
demo$hh_size <- sample(1:6, n, replace = TRUE,
                       prob = c(0.10, 0.20, 0.25, 0.25, 0.15, 0.05))
demo$hh_income <- demo$income_pc * demo$hh_size
# Students can verify income_pc = annual household income / household members.
# Put the income variables together near the other baseline characteristics.
base_names <- names(demo)[!names(demo) %in% c("income_pc", "hh_income", "hh_size")]
pos <- match("sex", base_names)
demo <- demo[c(base_names[1:pos], "hh_income", "hh_size", "income_pc",
               base_names[(pos + 1):length(base_names)])]

dir.create("data", showWarnings = FALSE)
write.csv(demo, "data/smoking_cohort_demo.csv", row.names = FALSE, na = "")

