# STAT 8311 | Session 7: Mediation

# 1. Introduction and principle
# Causal question: Among people aged 30–65 with a smoking history in the teaching clinic
# cohort, would baseline cessation, compared with continued smoking, reduce lung cancer
# after year 1 through year 20; how much of that effect operates through sustained
# abstinence during months 2–12?
# PICO-T (I/E is the exposure; no cessation programme is assigned).
# P: People aged 30–65 with a smoking history in the teaching clinic cohort.
# I/E: Baseline cessation (seven-day abstinence at the baseline visit).
# C: Continued smoking at baseline.
# O: Lung cancer.
# T: Cancer after year 1 through year 20.
# Mediator: sustained abstinence during months 2–12; month 1 is a grace period.
#
# Mediation separates total, indirect and direct effects under causal assumptions.

# 2. Setup: folders and packages
# Folders: data/; output/tables/; output/figures/.
getwd()
dir.create("data")
dir.create("output")
dir.create("output/tables")
dir.create("output/figures")

# Install once if needed:
# install.packages(c("dplyr", "dagitty", "ggdag", "ggplot2", "mediation", "gtsummary", "flextable"))
library(dplyr)
library(dagitty)
library(mediation)
library(gtsummary)
library(flextable)
library(ggdag)
library(ggplot2)

# 3. Prepare the data
# 3a. Download the original data from GitHub
url <- paste0("https://raw.githubusercontent.com/profjamal/causality/main/",
              "data/smoking_cohort_demo.csv")
dat_ori <- read.csv(url)
# If data already in the folder: dat_ori <- read.csv("data/smoking_cohort.csv")

glimpse(dat_ori)
head(dat_ori, 10)


# 3b. Select and name the analysis variables
# Use the complete cancer outcome to focus on mediation.
dat <- dat_ori %>%
  dplyr::select(stop_smoke = cessation, abstinent = sustained_abstinence,
                lung_ca = lung_cancer_complete, asbestos = asbestos_exposure,
                age, pack_yr, income_pc, clinic = clinic_id) %>%
  mutate(
    stop_smoke = factor(stop_smoke, levels = c(0, 1),
                        labels = c("Continued smoking", "Stopped smoking")),
    lung_ca = factor(lung_ca, levels = c(0, 1), labels = c("No", "Yes")),
    asbestos = factor(asbestos, levels = c(0, 1), labels = c("No", "Yes")),
    abstinent = factor(abstinent, levels = c(0, 1), labels = c("No", "Yes"))
  )
attr(dat$stop_smoke, "label") <- "Baseline smoking status"
attr(dat$abstinent, "label") <- "Sustained abstinence (months 2–12)"
attr(dat$lung_ca, "label") <- "Later lung cancer"
attr(dat$asbestos, "label") <- "Previous asbestos exposure"
attr(dat$age, "label") <- "Age (years)"
attr(dat$pack_yr, "label") <- "Baseline pack-years"
attr(dat$income_pc, "label") <- "Annual income (RM per person)"

# 4. Statistical plan and causal structure
# Fit TWO logistic models using the SAME participants:
# Model 1: cessation and baseline factors predict sustained abstinence.
# Model 2: cessation, abstinence and baseline factors predict cancer.
# Then combine their predictions to estimate indirect, direct and total effects.
# Does stopping smoking at baseline reduce later lung cancer risk
# through continuous abstinence during months 2–12?
# C = age, baseline pack-years, income and prior asbestos exposure.

dag <- dagitty('dag {
  stop_smoke [exposure, pos="0,0"]
  abstinent [pos="2,-1"]
  lung_ca [outcome, pos="4,0"]
  C [pos="2,2"]
  stop_smoke -> abstinent -> lung_ca
  stop_smoke -> lung_ca
  C -> stop_smoke
  C -> abstinent
  C -> lung_ca
}')
plot(dag)
adjustmentSets(dag)  # Total effect: adjust for C.

# 5. Analysis and presentable tables
# 5a. Describe the cohort and display the table
# Mean (SD) for age and pack-years; median (Q1, Q3) for skewed income.
tab <- dat %>%
  tbl_summary(
    by = stop_smoke,
    include = c(age, pack_yr, income_pc, asbestos, abstinent, lung_ca),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     income_pc ~ "{median} ({p25}, {p75})"),
    type = list(c(asbestos, abstinent, lung_ca) ~ "categorical"),
    label = list(age ~ "Age (years)", 
                 pack_yr ~ "Baseline pack-years",
                 income_pc ~ "Annual income (RM per person)",
                 asbestos ~ "Previous asbestos exposure",
                 abstinent ~ "Continuous abstinence (months 2–12)",
                 lung_ca ~ "Later lung cancer")
  ) %>%
  add_overall() %>%
  add_p(test = list(c(age, pack_yr) ~ "t.test",
                    income_pc ~ "wilcox.test",
                    c(asbestos, abstinent, lung_ca) ~ "chisq.test.no.correct")) %>%
  modify_caption("Participant characteristics and follow-up outcomes by baseline smoking status")
tab
# Group comparisons are descriptive; they do not select adjustment variables.

# 5b. Fit Model 1: predict the mediator (abstinence)
# family = binomial() fits logistic regression for the binary mediator.
fit_m <- glm(abstinent ~ stop_smoke + age + pack_yr + income_pc + asbestos,
             family = binomial(), data = dat)
summary(fit_m)
# Coefficients are log-odds; they are not the indirect effect.


# 5c. Fit Model 2: predict the outcome (cancer)
# Include abstinence as well as cessation and the baseline adjustment variables.
fit_y <- glm(lung_ca ~ stop_smoke + abstinent + age + pack_yr + income_pc + asbestos,
             family = binomial(), data = dat)
summary(fit_y)
# The stop_smoke coefficient is adjusted for abstinence and baseline factors.
# It is not the average direct risk difference reported below.


# 5d. Combine the two models to estimate mediation
# Compare the labelled Stopped smoking group with Continued smoking.
# set.seed() makes the simulation reproducible; sims = 1000 sets the number of draws.
set.seed(8311)
med <- mediate(fit_m, fit_y, treat = "stop_smoke", mediator = "abstinent",
               control.value = "Continued smoking", treat.value = "Stopped smoking",
               sims = 1000, cluster = dat$clinic)
summary(med)
# Read ACME (average), ADE (average) and Total Effect in the output.

# 5d check (optional): total-association model, mediator omitted (path c)
fit_c <- glm(lung_ca ~ stop_smoke + age + pack_yr + income_pc + asbestos,
             family = binomial(), data = dat)
summary(fit_c)
# Do not compare c - c' with a * b: logistic coefficients do not decompose.

# Decomposition check on the risk scale
med$tau.coef                 # total
med$z.avg + med$d.avg        # direct + indirect = total


# 5e. Put the four effects into one table
res <- data.frame(
  Measure = c("Indirect (ACME), pp", 
              "Direct (ADE), pp",
              "Total, pp", 
              "Proportion mediated, %"),
  Estimate = c(med$d.avg, med$z.avg, med$tau.coef, med$n.avg),
  Lower = c(med$d.avg.ci[1], med$z.avg.ci[1], med$tau.ci[1], med$n.avg.ci[1]),
  Upper = c(med$d.avg.ci[2], med$z.avg.ci[2], med$tau.ci[2], med$n.avg.ci[2])
)
res[, 2:4] <- round(100 * res[, 2:4], 2)  # Risk differences: pp; proportion mediated: %.
res$`95% CI` <- paste(res$Lower, "to", res$Upper)

# Display the results in the RStudio Viewer.
tab_med <- flextable(res[, c("Measure", "Estimate", "95% CI")]) %>%
  colformat_double(digits = 2) %>%
  autofit()
tab_med

# 6. Interpret the results
# Compare STOPPED with CONTINUED smoking. Negative risk differences mean lower risk.
# Indirect (ACME): difference through sustained abstinence.
# Direct (ADE): difference through pathways outside this mediator.
# Total: indirect plus direct; do not add their confidence limits.
# Report all three effects with CIs, regardless of statistical significance.

# Read each estimate with its 95% CI; the null for risk differences is 0.
# Indirect CI excludes 0: evidence consistent with mediation, under the assumptions.
# Indirect CI includes 0: the mediated effect is uncertain; do not conclude none exists.
# A significant total effect is not required before estimating mediation.
# A non-significant direct effect does not establish complete mediation.

# Effects are in percentage points (pp), not relative percent reductions.
# Example: -2.80 pp means about 2.80 fewer cases per 100 people through the mediator.
# Proportion mediated is a percentage of the total effect, not a risk difference.
# Do not interpret it when the total effect is near zero or effects oppose each other.

# Causal interpretation requires the proposed time order, adequate confounding
# control for all three relationships, overlap and suitable models.
# The standard method also assumes no exposure-induced mediator–outcome confounder.

# 7. Subsequent analysis decisions
# Keep all three effects in the report, including uncertain estimates.
# Reconsider time order, measurement and common causes before claiming a pathway.
# If an effect is uncertain, report its CI rather than removing the mediator.
# If an estimate is unexpected, check coding, the exposure comparison and model specification.

# 8. Write the report
# Methods: Define exposure, mediator window, outcome, adjustment and the two models.
# Results: Indirect effect = __ pp (95% CI __ to __); direct = __ pp (__ to __);
# total = __ pp (__ to __); proportion mediated = __% (__ to __), if interpretable.
# Conclusion (answer the question): The findings are [consistent with mediation /
# inconclusive about mediation] through sustained abstinence, under the assumptions.
# The estimated risk difference through this pathway was __ pp; it accounted for
# __% of the total effect, if interpretable. State whether a direct effect remains.

# 9. Save the figures and tables

ft_desc <- as_flex_table(tab) %>%
  fontsize(size = 10, part = "all") %>%
  width(j = 1, width = 1.6) %>%
  width(j = 2:4, width = 1.3) %>%
  width(j = 5, width = 0.6)
save_as_docx(ft_desc,
             path = "output/tables/s07_descriptive.docx")

save_as_docx(tab_med, path = "output/tables/s07_results.docx")

p_dag <- ggdag(dag, node = FALSE, text = FALSE) +
  geom_dag_label(aes(label = name), fill = "white", size = 4) +
  theme_dag() +
  scale_x_continuous(expand = expansion(mult = 0.2)) +
  scale_y_continuous(expand = expansion(mult = 0.15))
ggsave("output/figures/mediation_dag.png", p_dag,
       width = 7, height = 5, dpi = 300)

# 10. Summary
# End by answering the opening question, supported by the estimates and their CIs.
# Fit two models, estimate mediation and read the effects with their CIs.
# Distinguish percentage-point risk differences from the percentage mediated.
