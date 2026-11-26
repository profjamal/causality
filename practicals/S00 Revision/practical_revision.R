# STAT 8311 | S00: Revision of R for Statistical Analysis

# 1. Introduction and principle
# Question: Which characteristics are associated with high BP in this sample?
# Compare age, sex, exercise, smoking and BMI status by BP status.
# This revision examines associations; it does not estimate causal effects.
# PECO-T describes this cross-sectional association question.
# P: Health Status survey participants.
# E: Age, sex, exercise, smoking and BMI status.
# C: The reference category for each characteristic, or a one-year age difference.
# O: High BP (SBP >= 140 or DBP >= 90 mmHg).
# T: The survey visit; no follow-up.
#
# Describe the Health Status sample, then compare characteristics by BP status.
# Use mean (SD) for roughly symmetric continuous data and n (%) for categories.
# Bivariable results describe unadjusted associations, not causal effects.

# 2. Setup: folders and packages
# Folders: data/; output/tables/; output/figures/.
getwd()
dir.create("data")
dir.create("output")
dir.create("output/tables")
dir.create("output/figures")

# Install once if needed:
# install.packages(c("dplyr", "labelled", "ggplot2", "gtsummary", "flextable", "writexl"))
library(dplyr)
library(labelled)
library(ggplot2)
library(gtsummary)
library(flextable)
library(writexl)

# 3. Prepare the data
# 3a. Download the Health Status data
url <- "https://raw.githubusercontent.com/profjamal/biostatistics/main/healthstatus.csv"
dat_ori <- read.csv(url)
# Offline: replace the line above with dat_ori <- read.csv("data/healthstatus.csv")
glimpse(dat_ori)


# 3b. Select, rename and label the variables
dat <- dat_ori %>%
  select(id, age, sex, ex = exercise, smk = smoking,
         weight_kg = wt, height_cm = ht, sbp, dbp) %>%
  mutate(
    sex = factor(sex, levels = c("Male", "Female")),
    ex = factor(ex, levels = c("Low", "Moderate", "High")),
    smk = factor(smk, levels = c("No", "Yes"),
                 labels = c("Non-smoker", "Smoker")),
    bmi = weight_kg / (height_cm / 100)^2,
    bmi_status = ifelse(bmi < 25, "<25",
                        ifelse(bmi < 30, "25 to <30", "30 or above")),
    bmi_status = factor(bmi_status, levels = c("<25", "25 to <30", "30 or above")),
    bp = factor(if_else(sbp >= 140 | dbp >= 90, "High", "Not high"),
                levels = c("Not high", "High"))
  )
glimpse(dat)
colSums(is.na(dat))
var_label(dat) <- list(
  age = "Age (years)",
  sex = "Sex",
  ex = "Exercise level",
  smk = "Smoking status",
  bmi = "BMI (kg/m²)",
  bmi_status = "BMI status (kg/m²)",
  bp = "BP status",
  sbp = "Systolic BP (mmHg)",
  dbp = "Diastolic BP (mmHg)"
)

# BP status is a screening category.

# 4. Statistical plan
# Check distributions, describe the sample and make simple plots.
# Then compare age and categorical characteristics by BP status.
# Use a t-test for age and Fisher's exact test for categorical variables.

# 5. Analysis and presentable tables
# 5a. Check the shape of age before choosing a summary
# The line is a normal curve with the same mean and SD as the observed ages.
p_age <- ggplot(dat, aes(age)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 5,
                 fill = "steelblue", colour = "white") +
  stat_function(fun = dnorm,
                args = list(mean = mean(dat$age, na.rm = TRUE),
                            sd = sd(dat$age, na.rm = TRUE))) +
  labs(x = "Age (years)", y = "Density") +
  theme_minimal()
p_age
# Look for symmetry and long tails. Use mean (SD) if roughly symmetric;
# for clearly skewed data, use median (Q1, Q3).


# 5b. Describe the sample using gtsummary
# For the supplied data, use mean (SD) for age, BMI and BP readings.
# Categorical variables use counts and percentages: n (%).
tab <- dat %>%
  tbl_summary(
    include = c(age, sex, ex, smk, bmi, bmi_status, sbp, dbp, bp),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)")
  )
tab


# 5c. Make simple plots with ggplot2
p_ex <- ggplot(dat, aes(ex)) +
  geom_bar(fill = "steelblue") +
  labs(x = "Exercise level", y = "Count") +
  theme_minimal()
p_ex

p_sbp <- ggplot(dat, aes(ex, sbp)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Exercise level", y = "Systolic BP (mmHg)") +
  theme_minimal()
p_sbp

p_age_sbp <- ggplot(dat, aes(age, sbp)) +
  geom_point() +
  labs(x = "Age (years)", y = "Systolic BP (mmHg)") +
  theme_minimal()
p_age_sbp

# 6. Interpret the descriptive results
# Read the units, denominator and missing counts before comparing summaries.
# The normal curve is a guide; a single plot does not prove normality.
# Compare patterns in the plots with the numerical summaries.

# 7. Subsequent analysis: bivariable comparisons by BP status
# Objective: examine how age, sex, exercise, smoking and BMI status
# are each associated with high BP status.
# SBP and DBP define BP status, so leave them out of this comparison.
tab_bp <- dat %>%
  tbl_summary(
    by = bp,
    include = c(age, sex, ex, smk, bmi_status),
    statistic = all_continuous() ~ "{mean} ({sd})"
  ) %>%
  add_p(test = list(age ~ "t.test", all_categorical() ~ "fisher.test"))
tab_bp
# Compare the two BP columns. These are unadjusted associations.

# Read group differences and p-values together, not p-values alone.
# A non-significant result does not establish equal groups.

# 8. Write the report
# Methods: Describe participants, BP/BMI definitions, summaries and tests.
# Results: N = __; high BP = __ (__%). Describe the main group differences.
# Conclusion (answer the question): High BP was associated with __ in this sample.
# For __, the comparison was uncertain. These unadjusted, cross-sectional
# comparisons do not establish which characteristics cause high BP.

# 9. Save the figures and tables
# Convert the tables for Word. The path gives the folder and filename.
ft1 <- as_flex_table(tab)
save_as_docx("Participant characteristics" = ft1,
             path = "output/tables/revision_tables.docx",
             pr_section = officer::prop_section(
               page_margins = officer::page_mar(top = 0.6, bottom = 0.6)
             ))

# Save the bivariable table after viewing it in RStudio.
ft_bp <- as_flex_table(tab_bp)
save_as_docx("Characteristics by blood pressure status" = ft_bp,
             path = "output/tables/revision_bp_bivariable.docx")

# Excel stores the displayed table as rows and columns.
write_xlsx(as_tibble(tab), "output/tables/table1.xlsx")
write_xlsx(as_tibble(tab_bp), "output/tables/table2.xlsx")

ggsave("output/figures/age.png", p_age, width = 6, height = 4, dpi = 300)
ggsave("output/figures/exercise.png", p_ex, width = 6, height = 4, dpi = 300)
ggsave("output/figures/sbp_exercise.png", p_sbp, width = 6, height = 4, dpi = 300)
ggsave("output/figures/age_sbp.png", p_age_sbp, width = 6, height = 4, dpi = 300)
write.csv(dat, "data/health_clean.csv", row.names = FALSE)
# In RStudio's Files pane, open output > tables or output > figures.

# 10. Summary
# End by answering the opening question using the descriptive and bivariable results.
# Prepare and label variables, describe distributions, then compare BP groups.
# Present tables before saving; distinguish description from causal inference.
