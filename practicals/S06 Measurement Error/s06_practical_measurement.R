# STAT 8311 | Session 6: Measurement Error and Information Bias

# SETUP

# Setup 1. Set the working directory and create folders
# Edit and run this line for your own practical folder:
# setwd("~/STAT8311/S06 Measurement Error")
getwd()
dir.create("data")
dir.create("output")
dir.create("output/tables")
dir.create("output/figures")

# Setup 2. Install and load packages
# Install once if needed:
# install.packages(c("dplyr", "labelled", "ggplot2", "dagitty", "ggdag", "gtsummary",
#                    "survey", "flextable", "writexl"))
library(dplyr)
library(labelled)
library(ggplot2)
library(flextable)
library(dagitty)
library(ggdag)
library(gtsummary)
library(survey)

# Setup 3. Import the raw data: dat_ori
url <- paste0(
  "https://raw.githubusercontent.com/profjamal/causality/main/",
  "data/smoking_cohort_demo.csv"
)
dat_ori <- read.csv(url)
# Offline: dat_ori <- read.csv("data/smoking_cohort.csv")
head(dat_ori, 10)

# Setup 4. Select and rename variables: dat
# stop_smoke: seven-day abstinence at baseline (1) vs smoking (0).
# Later relapse or quitting is possible.
# lung_ca: cancer after year 1 through year 20 (0 = no, 1 = yes).
# Use complete synthetic cancer outcomes, as in Session 5.
dat <- dat_ori %>%
  dplyr::select(stop_smoke = cessation, lung_ca = lung_cancer_complete,
                age, sex, pack_yr, income_pc, clinic = clinic_id)

# Setup 5. Set variable labels, values and factor levels: dat
dat <- dat %>%
  mutate(
    stop_smoke = factor(stop_smoke, levels = c(0, 1),
                        labels = c("Continued smoking", "Stopped smoking")),
    lung_ca = factor(lung_ca, levels = c(0, 1), labels = c("No", "Yes")),
    sex = factor(sex, levels = c("female", "male"), labels = c("Female", "Male"))
  )
var_label(dat) <- list(
  stop_smoke = "Baseline smoking status",
  lung_ca = "Later lung cancer",
  age = "Age (years)",
  sex = "Sex",
  pack_yr = "Baseline pack-years",
  income_pc = "Annual income (RM per person)"
)

# pack_yr = cumulative smoking before baseline, in pack-years.
# income_pc = annual household income / household members, RM/person/year.


head(dat, 10)


# ANALYSIS

# Analysis 1. Question and PICO-T
# Causal question: Among people aged 30–65 with a smoking history in the teaching clinic
# cohort, would baseline cessation, compared with continued smoking, reduce lung cancer
# after year 1 through year 20?
# Analysis question: How does inaccurate recording of cessation change
# our estimate of that relationship?
# PICO-T (I/E is the exposure; no cessation programme is assigned).
# P: People aged 30–65 with a smoking history in the teaching clinic cohort.
# I/E: Baseline cessation (seven-day abstinence at the baseline visit).
# C: Continued smoking at baseline.
# O: Lung cancer.
# T: Cancer after year 1 through year 20.
#
# Examine how inaccurate cessation recording changes the estimated association.
# Sensitivity and specificity describe accuracy; narrow CIs do not remove bias.

# Analysis 2. DAG and adjustment plan
# Create a recorded measure using stated accuracy assumptions, check it,
# then compare equally adjusted models using original and recorded exposure.
# Identify the measurement structure
# How does inaccurate recording of cessation change its association with cancer?
# C groups the same baseline factors used in Session 5.
# smoke_rec is a MEASUREMENT of stop_smoke, not a mediator.
dag <- dagitty('dag {
  stop_smoke [exposure, pos="0,0"]
  lung_ca [outcome, pos="4,0"]
  C [pos="2,2"]
  smoke_rec [pos="0,-2"]
  C -> stop_smoke
  C -> lung_ca
  stop_smoke -> lung_ca
  stop_smoke -> smoke_rec
}')
plot(dag)
adjustmentSets(dag)
# C is the proposed baseline adjustment set: age, sex, pack-years and income.
# In the core simulation, recording accuracy does not depend on lung_ca.
# The recorded label does not cause cancer; the original exposure may affect it.

# Analysis 3. Descriptive analysis and presentation table
# Describe the cohort before introducing recording error.
desc <- tbl_summary(
  dat, by = stop_smoke,
  include = c(age, sex, pack_yr, income_pc, lung_ca),
  statistic = list(all_continuous() ~ "{mean} ({sd})",
                   income_pc ~ "{median} ({p25}, {p75})")
)
desc
# Compare the groups; these summaries do not select adjustment variables.

# Analysis 4. Analysis and interpretation
# 4a. Create an imperfect recorded measure
# Sensitivity: chance of recording cessation among people who achieved it.
# Specificity: chance of recording continued smoking among those still smoking.
se <- 0.80
sp <- 0.90
# pr is the chance of RECORDING cessation for each person.
# Original cessation: pr = 0.80; original smoking: pr = 1 - 0.90 = 0.10.
pr <- ifelse(dat$stop_smoke == "Stopped smoking", se, 1 - sp)
set.seed(8316)
dat$smoke_rec <- factor(rbinom(nrow(dat), size = 1, prob = pr),
                        levels = c(0, 1),
                        labels = c("Continued smoking", "Stopped smoking"))
var_label(dat) <- list(
  smoke_rec = "Recorded smoking status"
)
# rbinom draws a recorded value of 0 or 1 for each person.
# The seed makes this example reproducible.
# These are assumed accuracy values, not estimates from real validation data.
# The original dat_ori and its CSV remain unchanged; smoke_rec is added to dat.

# 4b. Compare recorded and original exposure

# Descriptive check 1: compare original and recorded cessation status.
ct <- table(Original = dat$stop_smoke, Recorded = dat$smoke_rec)
ct
# Rows and columns use the same labelled smoking categories.
# Off-diagonal cells are errors: recorded cessation among smokers, or recorded smoking among quitters.
# Diagonal cells agree. These are exposure classifications, not cancer diagnoses.

# Descriptive check 2: calculate the observed sensitivity and specificity.
acc <- data.frame(
  measure = c("Sensitivity", "Specificity"),
  assumed = c(se, sp),
  observed = c(mean(dat$smoke_rec[dat$stop_smoke == "Stopped smoking"] == "Stopped smoking"),
               mean(dat$smoke_rec[dat$stop_smoke == "Continued smoking"] == "Continued smoking"))
)
acc

# 4c. Display sensitivity and specificity
ft1 <- flextable(acc) %>%
  set_header_labels(measure = "Measure", assumed = "Assumed", observed = "Observed") %>%
  colformat_double(digits = 3) %>%
  autofit()
ft1
# Expect observed values close to 0.80 and 0.90, not exactly equal.
# Random sampling creates some variation around the assumed probabilities.

# Descriptive check 3: check recording accuracy within cancer groups.
by_ca <- dat %>%
  group_by(lung_ca) %>%
  summarise(n = n(),
            se = mean(smoke_rec[stop_smoke == "Stopped smoking"] == "Stopped smoking"),
            sp = mean(smoke_rec[stop_smoke == "Continued smoking"] == "Continued smoking"), .groups = "drop")
by_ca
# lung_ca is labelled No / Yes.
# The simulation uses the SAME accuracy probabilities for both groups.
# Observed rates can differ by chance; that alone does not make the mechanism
# differential. Here recording is independent of cancer given original status.

# 4d. Fit TWO comparable adjusted models
# Identify clinics once; use the same participants in both models.
des <- svydesign(ids = ~clinic, weights = ~1, data = dat)
# Model 1: use the original exposure as a benchmark.
fit0 <- svyglm(lung_ca ~ stop_smoke + age + sex + pack_yr + income_pc,
            family = quasibinomial(), design = des)
# Model 2: replace only the exposure with its recorded measure.
fit1 <- svyglm(lung_ca ~ smoke_rec + age + sex + pack_yr + income_pc,
            family = quasibinomial(), design = des)
# Same outcome, same people, same adjustment; only the exposure measure changes.
# The original-status model is a BENCHMARK, not the true causal effect.

# 4e. Display and compare the two exposure ORs
# svyglm accounts for people sharing a clinic; no helper file is needed.
t0 <- tbl_regression(fit0, exponentiate = TRUE, include = stop_smoke, show_single_row = stop_smoke,
                     label = list(stop_smoke ~ "Cessation vs continued smoking"))
t1 <- tbl_regression(fit1, exponentiate = TRUE, include = smoke_rec, show_single_row = smoke_rec,
                     label = list(smoke_rec ~ "Cessation vs continued smoking"))
tab <- tbl_stack(list(t0, t1), group_header = c(
  "Original simulated status", "Imperfect recorded status"))
tab
# Read both ORs and their 95% CIs. Null = 1; OR < 1 means lower cancer ODDS.
# Is the recorded-status estimate closer to 1 than the benchmark?
# This example illustrates attenuation, not a universal rule about error.
# A small p-value does not establish accurate measurement or a causal effect.
# These CIs account for clinic dependence under each model; they do not correct
# misclassification or include uncertainty about the assumed sensitivity/sp.
# Do not infer a formal difference between estimates from overlap of their CIs.

# Compare the ORs with 1 and with each other, considering both CIs.
# A recorded OR closer to 1 illustrates attenuation here, not a universal rule.
# Statistical significance does not establish accurate measurement.
# CI overlap is not a formal test of the difference between the two estimates.

# If recording is inaccurate, improve measurement or seek validation data.
# In real work, justify sensitivity/specificity and their possible uncertainty.
# Do not replace recorded values with unavailable true values in an actual study.

# Analysis 5. Suggested methods and results
# Methods: State the recording assumptions and the common adjustment variables.
# Results: Observed sensitivity = __; specificity = __.
# Original-status OR = __ (95% CI __ to __); recorded-status OR = __ (__ to __).

# Analysis 6. Conclusion: answer the question
# Conclusion (answer the question): Using the recorded exposure made the
# estimated cessation–cancer association __ compared with the original-status model.
# Inaccurate recording therefore __ our assessment of the proposed effect.
# The benchmark is not proof of the true causal effect; explain the uncertainty.

# Analysis 7. Save tables and figures
# Convert the accuracy data frame and the gtsummary model table.
ft2 <- as_flex_table(tab) %>%
  set_table_properties(layout = "autofit", width = 1)
save_as_docx("Cohort characteristics" = as_flex_table(desc),
             "Cessation recording accuracy" = ft1,
             "Original and recorded cessation associations" = ft2,
             path = "output/tables/s06_results.docx")
writexl::write_xlsx(list(models = as_tibble(tab), accuracy = acc,
                         by_cancer = by_ca, counts = as.data.frame(ct)),
                    "output/tables/s06_results.xlsx")
capture.output(summary(fit0), file = "output/s06_original_model.txt")
capture.output(summary(fit1), file = "output/s06_recorded_model.txt")
# Open output/tables/s06_results.docx. Rerunning updates it automatically.
# Excel sheets contain the displayed model table, accuracy checks and counts.

ggsave("output/figures/s06_dag.png",
       plot = ggdag(dag, node = FALSE, text = FALSE) +
         geom_dag_label(aes(label = name), fill = "white", size = 4) +
         theme_dag() +
         scale_x_continuous(expand = expansion(mult = 0.2)) +
         scale_y_continuous(expand = expansion(mult = 0.15)),
       width = 7, height = 5, dpi = 300)

# End of practical
