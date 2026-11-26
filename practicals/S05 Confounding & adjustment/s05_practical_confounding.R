# STAT 8311 | Session 5: Confounding and Adjustment

# 1. Introduction and principle
# Causal question: Among people aged 30–65 with a smoking history in the teaching clinic
# cohort, would baseline cessation, compared with continued smoking, reduce lung cancer
# after year 1 through year 20?
# Analysis question: What does adjustment for baseline common causes change
# about the estimated cessation–cancer association?
# PICO-T (I/E is the exposure; no cessation programme is assigned).
# P: People aged 30–65 with a smoking history in the teaching clinic cohort.
# I/E: Baseline cessation (seven-day abstinence at the baseline visit).
# C: Continued smoking at baseline.
# O: Lung cancer.
# T: Cancer after year 1 through year 20.
#
# Compare cessation with continued smoking for later lung cancer.
# Confounders are common causes; choose adjustment from causal reasoning.
# A change in an OR after adjustment does not by itself prove confounding.

# 2. Setup: folders and packages
# Folders: data/; output/tables/; output/figures/.
getwd()
dir.create("data")
dir.create("output")
dir.create("output/tables")
dir.create("output/figures")
# Install once if needed:
# install.packages(c("dplyr", "ggplot2", "dagitty", "ggdag", "gtsummary",
#                    "survey", "flextable", "writexl"))
library(dplyr)
library(ggplot2)
library(flextable)
library(dagitty)
library(ggdag)
library(gtsummary)
library(survey)

# 3. Prepare the data
# 3a. Import the cohort; then select variables and set their coding.
url <- paste0(
  "https://raw.githubusercontent.com/profjamal/causality/main/",
  "data/smoking_cohort_demo.csv"
)
dat_ori <- read.csv(url)
# Offline: dat_ori <- read.csv("data/smoking_cohort.csv")
head(dat_ori, 10)

# 3b. Select and label the analysis variables
# stop_smoke: seven-day abstinence at baseline (1) vs smoking (0).
# Later relapse or quitting can occur.
# lung_ca: cancer after year 1 through year 20 (0 = no, 1 = yes).
# pack_yr = previous pack-years; income_pc = annual income per capita (RM/person/year).
# Use the complete synthetic outcome to focus this lesson on adjustment.
# Missing outcomes and complete-case selection are covered separately.
dat <- dat_ori %>%
  dplyr::select(stop_smoke = cessation, lung_ca = lung_cancer_complete,
                age, sex, pack_yr, income_pc, clinic = clinic_id) %>%
  mutate(
    stop_smoke = factor(stop_smoke, levels = c(0, 1),
                        labels = c("Continued smoking", "Stopped smoking")),
    lung_ca = factor(lung_ca, levels = c(0, 1), labels = c("No", "Yes")),
    sex = factor(sex, levels = c("female", "male"), labels = c("Female", "Male"))
  )
attr(dat$stop_smoke, "label") <- "Baseline smoking status"
attr(dat$lung_ca, "label") <- "Later lung cancer"
attr(dat$age, "label") <- "Age (years)"
attr(dat$sex, "label") <- "Sex"
attr(dat$pack_yr, "label") <- "Baseline pack-years"
attr(dat$income_pc, "label") <- "Annual income (RM per person)"

# pack_yr = cumulative smoking before baseline, in pack-years.
# income_pc = annual household income / household members, RM/person/year.
nrow(dat)
colSums(is.na(dat))

# 4. Statistical plan and causal structure
# Describe both groups, then compare crude and adjusted models on the same people.
# Identify the adjustment set
# How is baseline cessation associated with later lung-cancer risk?
# C groups age, sex, previous pack-years and annual income per capita.
# These are possible baseline common causes, selected BEFORE the analysis.
dag <- dagitty('dag {
  stop_smoke [exposure, pos="0,0"]
  lung_ca [outcome, pos="4,0"]
  C [pos="2,2"]
  C -> stop_smoke
  C -> lung_ca
  stop_smoke -> lung_ca
}')
plot(dag)
adjustmentSets(dag)
# The backdoor path stop_smoke <- C -> lung_ca requires adjustment for C.
# Do not automatically adjust for variables occurring after exposure;
# some may be mediators. Mediation is examined in Session 7.
# A collider has two arrows pointing into it, e.g. stop_smoke -> S <- lung_ca.
# Selecting or adjusting for S may create bias; do not control every variable.
# This is a simplified working DAG, not proof that all confounders are measured.

# 5. Analysis and presentable tables
# 5a. Descriptive check 1: count cancer outcomes in each smoking group.
table(dat$stop_smoke, dat$lung_ca)
# Counts alone do not compare risk when group sizes differ.
round(100 * prop.table(table(dat$stop_smoke, dat$lung_ca), margin = 1), 1)
# Row percentages: read the Yes (cancer) column within each smoking group.

# 5b. Descriptive check 2: compare baseline factors between the smoking groups.
# Income is right-skewed, so use median (Q1, Q3).
desc <- tbl_summary(
  dat, by = stop_smoke, include = c(age, sex, pack_yr, income_pc),
  statistic = list(all_continuous() ~ "{mean} ({sd})",
                   income_pc ~ "{median} ({p25}, {p75})"),
  label = list(age ~ "Age (years)", sex ~ "Sex",
               pack_yr ~ "Previous pack-years",
               income_pc ~ "Income per capita (RM/person/year)")
)
desc
# Describe the differences. Do not select confounders using table p-values.
# A common cause may need adjustment even when its group difference is small.

# 5c. Descriptive check 3: view previous smoking history in both groups.
p1 <- ggplot(dat, aes(x = stop_smoke, y = pack_yr)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Baseline smoking status", y = "Previous pack-years") +
  theme_minimal()
p1
# Overlapping boxes do not establish comparability on all confounders.

# 5d. Fit TWO outcome models on the same participants
# Identify clinics once; use the same participants in both models.
des <- svydesign(ids = ~clinic, weights = ~1, data = dat)
# Model 1: estimate the crude association.
fit0 <- svyglm(lung_ca ~ stop_smoke, family = quasibinomial(), design = des)
# Model 2: estimate the association after baseline adjustment.
fit1 <- svyglm(lung_ca ~ stop_smoke + age + sex + pack_yr + income_pc,
            family = quasibinomial(), design = des)
summary(fit1)
# Both outcomes are binary, so we use logistic regression.
# The printed coefficients are log odds; the next table shows odds ratios.
# The adjustment variables follow the working DAG, not stepwise selection.
# Keep adjustment focused on the justified baseline variables.

# 5e. Present crude and adjusted ORs using gtsummary
# svyglm accounts for participants sharing clinics.
# gtsummary reads the ORs and clinic-adjusted CIs directly from each model.
t0 <- tbl_regression(fit0, exponentiate = TRUE, include = stop_smoke, show_single_row = stop_smoke,
                     label = list(stop_smoke ~ "Cessation vs continued smoking"))
t1 <- tbl_regression(fit1, exponentiate = TRUE, include = stop_smoke, show_single_row = stop_smoke,
                     label = list(stop_smoke ~ "Cessation vs continued smoking"))
# Combine each OR and CI, then place the models side by side.
t0 <- t0 %>%
  modify_column_merge(pattern = "{estimate} ({conf.low}, {conf.high})") %>%
  modify_header(estimate = "**OR (95% CI)**")
t1 <- t1 %>%
  modify_column_merge(pattern = "{estimate} ({conf.low}, {conf.high})") %>%
  modify_header(estimate = "**OR (95% CI)**")
tab <- tbl_merge(list(t0, t1),
                 tab_spanner = c("**Crude**", "**Adjusted**")) %>%
  modify_header(label = "**Exposure**")
tab
# Read each OR and its 95% CI. Null = 1; OR < 1 means lower ODDS of cancer.
# The adjusted OR accounts for age, sex, previous pack-years and annual income per capita.
# It is a conditional OR, not a risk ratio or a standardized population effect.
# ORs can change after adjustment because of non-collapsibility as well as
# confounding. Do not use a percentage-change rule as proof of confounding.
# Both summary(fit1) and the table use clinic-adjusted uncertainty.

# 6. Interpret the results
# Compare the crude and adjusted OR, their CIs and the covariates included.
# CI excludes 1: evidence of an association under the fitted model.
# CI includes 1: the direction and size remain uncertain; not proof of no effect.
# Keep an association's size separate from its statistical significance.

# 7. Subsequent analysis decisions
# Retain justified confounders even when their own p-values are large.
# Check whether both smoking groups cover comparable baseline covariate values.
# Do not remove variables solely to obtain a desired p-value.

# 8. Write the report
# Methods: We selected baseline age, sex, previous pack-years and annual income per capita using
# a working causal DAG. We fitted crude and adjusted logistic models to the
# same complete synthetic cohort, with CIs allowing for clinic clustering.
# Results: Crude OR = __ (95% CI __ to __).
# Adjusted OR = __ (95% CI __ to __).
# Conclusion (answer the question): After baseline adjustment, cessation was
# associated with __ cancer odds compared with continued smoking.
# This is [consistent with a protective effect / inconclusive about benefit],
# under the causal assumptions; adjustment alone does not establish causation.
# Do not say that adjustment or a small p-value proves causation.
# Remaining confounding, selection, measurement, overlap and model fit matter.
# The synthetic data include omitted causes: this is a teaching adjustment set.
# Clinic-clustered CIs address dependence, not confounding bias.

# 9. Save the figures and tables
# gtsummary already supplies the table labels, estimates and CIs.
# as_flex_table() converts the table; save_as_docx() saves it to Word.
ft1 <- as_flex_table(desc)
ft2 <- as_flex_table(tab) %>%
  set_table_properties(layout = "autofit", width = 1)
save_as_docx("Baseline characteristics" = ft1,
             "Crude and adjusted cessation associations" = ft2,
             path = "output/tables/s05_results.docx")
# Save the displayed regression table to Excel too.
writexl::write_xlsx(as_tibble(tab), "output/tables/s05_results.xlsx")
capture.output(summary(fit0), file = "output/s05_crude_model.txt")
capture.output(summary(fit1), file = "output/s05_adjusted_model.txt")
# Open output/tables/s05_results.docx. Rerunning updates the Word tables.
# The displayed regression table is also saved in output/tables/s05_results.xlsx.

ggsave("output/figures/s05_pack_years.png", p1,
       width = 6, height = 4, dpi = 300)

ggsave("output/figures/s05_dag.png",
       plot = ggdag(dag, node = FALSE, text = FALSE) +
         geom_dag_label(aes(label = name), fill = "white", size = 4) +
         theme_dag() +
         scale_x_continuous(expand = expansion(mult = 0.2)) +
         scale_y_continuous(expand = expansion(mult = 0.15)),
       width = 7, height = 5, dpi = 300)

# 10. Summary
# End by answering the opening question, supported by the estimates and their CIs.
# Justify adjustment before modelling; compare estimates using the same sample.
# Report the adjusted association with uncertainty and remaining limitations.
