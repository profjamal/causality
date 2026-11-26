# STAT 8311 | Sessions 9 and 10: Missing Data

# 1. Introduction and principle
# Causal question: Among people aged 30–65 with a smoking history in the teaching clinic
# cohort, would baseline cessation, compared with continued smoking, reduce lung cancer
# after year 1 through year 20?
# Analysis question: What can we conclude about that effect when some cancer
# outcomes are missing, under the assumptions of each handling method?
# PICO-T (I/E is the exposure; no cessation programme is assigned).
# P: People aged 30–65 with a smoking history in the teaching clinic cohort.
# I/E: Baseline cessation (seven-day abstinence at the baseline visit).
# C: Continued smoking at baseline.
# O: Lung cancer.
# T: Cancer after year 1 through year 20.
#
# Missing cancer outcomes can reduce precision and create selection bias.
# MCAR, MAR and MNAR describe missingness assumptions; data alone cannot prove MAR.
# Compare complete cases, observation weighting and multiple imputation.

# 2. Setup: folders and packages
# Folders: data/; output/tables/; output/figures/.
getwd()
dir.create("data")
dir.create("output")
dir.create("output/tables")
dir.create("output/figures")
# Install once if needed:
# install.packages(c("dplyr", "labelled", "ggplot2", "gtsummary", "flextable", "mice",
#                    "writexl", "survey"))
library(dplyr)
library(labelled)
library(ggplot2)
library(gtsummary)
library(flextable)
library(mice)
library(writexl)
library(survey)

# 3. Prepare the data
# 3a. Import the cohort; then select variables and set their coding.
url <- paste0("https://raw.githubusercontent.com/profjamal/causality/main/",
              "data/smoking_cohort_demo.csv")
dat_ori <- read.csv(url)
# Offline: dat_ori <- read.csv("data/smoking_cohort.csv")
# lung_ca: 0 = no cancer; 1 = cancer; NA = outcome not observed.
# income_pc: annual household income / members, RM/person/year.
# clinic identifies participants sharing a clinic.
dat <- dat_ori %>%
  dplyr::select(stop_smoke = cessation, lung_ca = lung_cancer,
                age, sex, pack_yr, income_pc,
                asbestos = asbestos_exposure, clinic = clinic_id) %>%
  mutate(
    stop_smoke = factor(stop_smoke, levels = c(0, 1),
                        labels = c("Continued smoking", "Stopped smoking")),
    lung_ca = factor(lung_ca, levels = c(0, 1), labels = c("No", "Yes")),
    sex = factor(sex, levels = c("female", "male"), labels = c("Female", "Male")),
    asbestos = factor(asbestos, levels = c(0, 1), labels = c("No", "Yes")),
    clinic = factor(clinic)
  )
var_label(dat) <- list(
  stop_smoke = "Baseline smoking status",
  lung_ca = "Later lung cancer",
  age = "Age (years)",
  sex = "Sex",
  pack_yr = "Baseline pack-years",
  income_pc = "Annual income (RM per person)",
  asbestos = "Previous asbestos exposure"
)

# Do not select lung_cancer_complete or unmeasured_health_risk.
# Researchers would not have these hidden values in an actual study.

# 4. Statistical plan
# Describe missingness, examine observation probabilities and fit the same
# adjusted cancer model using complete cases, observation weights and imputation.
# Inspect imputation diagnostics before comparing ORs and CIs across methods.

# 5. Analysis and presentable tables
# 5a. Count missing outcomes and compare observed with missing groups.
# Perform descriptive checks before choosing a method.
# Descriptive check 1: count missing values in every selected variable.
nrow(dat)
colSums(is.na(dat))
md.pattern(dat, plot = FALSE)
# In this supplied cohort, only lung_ca is incomplete.
# If other analysis variables are missing, this exercise needs adaptation.
stopifnot(!anyNA(dat[c("stop_smoke", "age", "sex", "pack_yr", "income_pc",
                     "asbestos", "clinic")]))

# Descriptive check 2: count observed and missing cancer outcomes.
dat$obs <- ifelse(is.na(dat$lung_ca), 0, 1)
# obs: 1 = observed; 0 = missing. It is not the cancer outcome.
table(dat$obs)
round(100 * mean(dat$obs == 0), 1)
# Missing outcomes are not zero and must not be recoded as no cancer.
dat$status <- factor(dat$obs, levels = c(1, 0),
                     labels = c("Observed", "Missing"))

# Descriptive check 3: compare people with and without observed outcomes.
# Income is right-skewed, so use median (Q1, Q3).
desc <- tbl_summary(
  dat, by = status, include = c(stop_smoke, age, sex, pack_yr, income_pc),
  statistic = list(all_continuous() ~ "{mean} ({sd})",
                   income_pc ~ "{median} ({p25}, {p75})")
)
desc
miss <- dat %>%
  group_by(stop_smoke) %>%
  summarise(n = n(), missing = sum(obs == 0),
            missing_pct = 100 * mean(obs == 0), .groups = "drop")
miss
p1 <- ggplot(miss, aes(x = stop_smoke, y = missing_pct)) +
  geom_col(fill = "steelblue") +
  labs(x = "Baseline smoking status", y = "Cancer outcomes missing (%)") +
  theme_minimal()
p1
# Consider the mechanism and causal implications
# MCAR: missingness is unrelated to observed and unseen information.
# MAR: after using observed information, missingness does not depend on
# the unseen outcome. This is a working assumption, not a test result.
# MNAR: missingness still depends on the unseen outcome after conditioning
# on the observed information used to explain it.
# Differences in desc/miss can challenge simple MCAR explanations.
# These checks cannot distinguish MAR from MNAR or prove a mechanism.
# The generator uses a hidden health-risk cause of cancer AND observation.
# Our measured variables therefore do not guarantee MAR in this cohort.
# Selecting observed cases can change the population and induce selection bias.
# Discuss: what reasons for missing outcomes would you collect in a real study?

# 5b. Method 1: fit the outcome model in complete cases
dat_cc <- dat %>% filter(obs == 1)
nrow(dat_cc)
# Compare nrow(dat) and nrow(dat_cc): how many outcomes are unavailable?
# ids = ~clinic keeps people from the same clinic together for uncertainty.
# weights = ~1 gives equal weight to each observed participant.
des.dat_cc <- svydesign(ids = ~clinic, weights = ~1, data = dat_cc)
fit.dat_cc <- svyglm(lung_ca ~ stop_smoke + age + sex + pack_yr + income_pc,
                 family = quasibinomial(), design = des.dat_cc)
# Complete cases are easy to analyse but may be selected and less precise.
# Complete-case validity does not always require MCAR; it depends on the model
# and selection process. Do not assume it is unbiased here.

# 5c. Method 2: model observation, calculate weights and fit the weighted model
# Model observation using information available for ALL participants.
# Do not include lung_ca, which is missing for the very people being modelled.
fit.obs <- glm(obs ~ stop_smoke + age + sex + pack_yr + income_pc + clinic,
               family = binomial(), data = dat)
dat$pr_obs <- predict(fit.obs, type = "response")
summary(dat$pr_obs)
dat$wt <- 1 / dat$pr_obs
dat_cc <- dat %>% filter(obs == 1)
summary(dat_cc$wt)
# People less likely to have an observed outcome receive greater weight.
# Very low observation probabilities and large weights can cause instability.
des.wt <- svydesign(ids = ~clinic, weights = ~wt, data = dat_cc)
fit.wt <- svyglm(lung_ca ~ stop_smoke + age + sex + pack_yr + income_pc,
                 family = quasibinomial(), design = des.wt)
# This uses observation weights, not exposure propensity weights.
# quasibinomial avoids a fractional-weight binomial warning.
# svyglm supplies uncertainty allowing for clinic clustering.

# 5d. Method 3: impute, fit the same model in each dataset and pool
# Include the analysis variables and measured auxiliary predictors.
# clinic is a factor, allowing different imputation intercepts by clinic.
# Exclude obs/status: obs=0 identifies exactly the people whose outcome is NA.
# Select imputation inputs from the prepared dat.
dat_mi <- dat %>%
  dplyr::select(lung_ca, stop_smoke, age, sex, pack_yr, income_pc,
                asbestos, clinic)
# mice chooses logistic imputation for this binary factor; other fields are complete.
imp <- mice(dat_mi, m = 20, maxit = 10, seed = 8310, printFlag = FALSE)
imp$method
imp$loggedEvents
# Inspect any logged events; they may indicate collinearity or fitting problems.
# Fit the same clinic-aware model in each completed dataset.
# The loop repeats these commands for imputations 1 to 20.
fits <- list()
for (i in 1:20) {
  dat_imp <- complete(imp, i)
  des.mi <- svydesign(ids = ~clinic, weights = ~1, data = dat_imp)
  fits[[i]] <- svyglm(
    lung_ca ~ stop_smoke + age + sex + pack_yr + income_pc,
    family = quasibinomial(), design = des.mi
  )
}
fit.mi <- as.mira(fits)
pooled <- pool(fit.mi)
summary(pooled, conf.int = TRUE, exponentiate = TRUE)
# Fit the SAME outcome model in all 20 datasets.
# Pool model coefficients and uncertainty, not the completed datasets or ORs.
# Twenty imputations are for class. Check stability and adequacy in real work.

# 5e. Inspect imputation behaviour and display the three estimates
# Use mice's own diagnostic plot; no separate helper script is needed.
p2 <- plot(imp, layout = c(2, 1))
p2
# Look for chains mixing without persistent drift or separation.
# These are diagnostic summaries of factor codes, not cancer-risk estimates.
# A short trace and no logged events do not prove a valid imputation model or MAR.
# Only one variable is incomplete here, so there is no multivariable feedback loop.

# Show cessation only; all models still include the same baseline covariates.
t1 <- tbl_regression(fit.dat_cc, exponentiate = TRUE, include = stop_smoke, show_single_row = stop_smoke,
  label = list(stop_smoke ~ "Cessation vs continued smoking"))
t2 <- tbl_regression(fit.wt, exponentiate = TRUE, include = stop_smoke, show_single_row = stop_smoke,
  label = list(stop_smoke ~ "Cessation vs continued smoking"))
t3 <- tbl_regression(fit.mi, exponentiate = TRUE, include = stop_smoke, show_single_row = stop_smoke,
  label = list(stop_smoke ~ "Cessation vs continued smoking"),
  tidy_fun = pool_and_tidy_mice)
tab <- tbl_stack(list(t1, t2, t3), group_header = c(
  "Complete cases", "Observation weighted", "Multiple imputation"))
tab
# Read each OR and 95% CI. An OR is not a risk ratio.
# Similar results do not prove MAR or adequate confounding control.
# Do not choose the method giving the smallest p-value or desired result.

# 6. Interpret the results
# Compare effect size, CI and participants used, not just statistical significance.
# CI includes 1: uncertainty about the association, not proof of no association.
# Agreement between methods does not prove MAR or remove unmeasured bias.

# 7. Subsequent analysis decisions
# Review low observation probabilities, extreme weights and imputation warnings.
# Choose a method using the design, missingness reasons and plausible assumptions.
# If MAR is doubtful, acknowledge that limitation and plan a justified sensitivity
# analysis in real work; no optional advanced analysis is required here.

# 8. Write the report
# Methods: Describe the missing variable, number/percentage missing and reasons.
# Explain the working missingness assumption and the information supporting it.
# State the analysis, observation and imputation predictors; report diagnostics.
# Results: Missing outcomes = __ of __ (__%).
# Complete-case OR = __ (95% CI __ to __).
# Weighted OR = __ (95% CI __ to __).
# MI OR = __ (95% CI __ to __).
# Conclusion (answer the question): Accounting for missing outcomes using __,
# cessation was associated with __ cancer odds compared with continued smoking.
# The results were [consistent with a protective effect / inconclusive], under __
# assumptions. Explain how missingness affects confidence in this causal answer.
# MI may retain more information with useful predictors and plausible MAR.
# Weighting may be useful when observation can be modelled with adequate overlap.
# Neither method guarantees removal of bias from unobserved causes.
# Consider plausible MNAR assumptions in a real study; no extra analysis is
# required for this practical. Never use hidden simulation truth to fill NAs.

# 9. Save the figures and tables
ft1 <- as_flex_table(desc)
ft2 <- as_flex_table(tab) %>%
  set_table_properties(layout = "autofit", width = 1)
save_as_docx("Characteristics by outcome observation" = ft1,
             "Three approaches to missing cancer outcomes" = ft2,
             path = "output/tables/s09_s10_results.docx")
write_xlsx(list(models = as_tibble(tab), missingness = miss),
           "output/tables/s09_s10_results.xlsx")
capture.output(summary(fit.obs), file = "output/s09_s10_observation_model.txt")
capture.output(summary(fit.dat_cc), file = "output/s09_s10_complete_case.txt")
capture.output(summary(fit.wt), file = "output/s09_s10_weighted.txt")
capture.output(summary(imp), file = "output/s09_s10_imputation.txt")
capture.output(summary(pooled, conf.int = TRUE, exponentiate = TRUE),
               file = "output/s09_s10_pooled.txt")

ggsave("output/figures/s09_s10_missing.png", p1,
       width = 6, height = 4, dpi = 300)


ggsave("output/figures/s09_s10_imputation_trace.png",
       grid::grid.grabExpr(print(p2), width = 7, height = 4),
       width = 7, height = 4, dpi = 300, bg = "white")

# 10. Summary
# End by answering the opening question, supported by the estimates and their CIs.
# Describe missingness, justify assumptions, inspect diagnostics and compare methods.
# Missing outcomes are not zero; no method automatically removes missing-data bias.
