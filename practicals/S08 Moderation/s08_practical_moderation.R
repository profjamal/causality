# STAT 8311 | Session 8: Moderation and Interaction

# 1. Introduction and principle
# Causal question: Among people aged 30–65 with a smoking history in the teaching clinic
# cohort, would baseline cessation, compared with continued smoking, reduce lung cancer
# after year 1 through year 20; does the effect differ by prior asbestos exposure on the
# odds-ratio scale?
# PICO-T (I/E is the exposure; no cessation programme is assigned).
# P: People aged 30–65 with a smoking history in the teaching clinic cohort.
# I/E: Baseline cessation (seven-day abstinence at the baseline visit).
# C: Continued smoking at baseline.
# O: Lung cancer.
# T: Cancer after year 1 through year 20.
# Modifier: occupational asbestos exposure before baseline.
#
# Moderation concerns a stronger or weaker effect at different moderator levels.
# State the scale: this practical compares odds ratios.

# 2. Setup: folders and packages
# Folders: data/; output/tables/; output/figures/.
getwd()
dir.create("data")
dir.create("output")
dir.create("output/tables")
dir.create("output/figures")

# Install once if needed:
# install.packages(c("dplyr", "labelled", "dagitty", "ggdag", "ggplot2",
#                    "broom", "gtsummary", "flextable"))
library(dplyr)
library(labelled)
library(dagitty)
library(broom)
library(gtsummary)


library(flextable)
library(ggdag)
library(ggplot2)

# 3. Prepare the data
# 3a. Import the cohort; then select variables and set their coding.
url <- paste0("https://raw.githubusercontent.com/profjamal/causality/main/",
              "data/smoking_cohort_demo.csv")
dat_ori <- read.csv(url)
# If data already in the folder: dat_ori <- read.csv("data/smoking_cohort.csv")

# Use the complete cancer outcome to focus on interaction.
dat <- dat_ori %>%
  dplyr::select(stop_smoke = cessation, lung_ca = lung_cancer_complete,
                asbestos = asbestos_exposure, age, sex, pack_yr, income_pc) %>%
  mutate(
    stop_smoke = factor(stop_smoke, levels = c(0, 1),
                        labels = c("Continued smoking", "Stopped smoking")),
    lung_ca = factor(lung_ca, levels = c(0, 1), labels = c("No", "Yes")),
    sex = factor(sex, levels = c("female", "male"), labels = c("Female", "Male")),
    asbestos = factor(asbestos, levels = c(0, 1), labels = c("No", "Yes"))
  )
var_label(dat) <- list(
  stop_smoke = "Baseline smoking status",
  lung_ca = "Later lung cancer",
  asbestos = "Previous asbestos exposure",
  age = "Age (years)",
  sex = "Sex",
  pack_yr = "Baseline pack-years",
  income_pc = "Annual income (RM per person)"
)

# 4. Statistical plan and causal structure
# Fit the interaction model, assess its interaction term, then report each group OR.
# Exposure: baseline cessation. Outcome: cancer after year 1 through year 20.
# Proposed moderator: asbestos exposure before baseline.
# C = age, sex, baseline pack-years and annual income per person.

# Structure A: ordinary causal DAG - identify adjustment with this diagram.
dag <- dagitty('dag {
  stop_smoke [exposure, pos="0,2"]
  lung_ca [outcome, pos="5,0"]
  asbestos [pos="0,-2"]
  C [pos="2,4"]
  stop_smoke -> lung_ca
  asbestos -> lung_ca
  C -> stop_smoke
  C -> asbestos
  C -> lung_ca
}')
plot(dag)
adjustmentSets(dag)
# Adjust for C; include asbestos and its interaction to estimate group effects.
# Do not adjust for subsequent abstinence or smoking in this total-association model.

# Structure B: the product node illustrates the proposed moderation.
# It is calculated from the two variables, not an additional confounder.
# Use Structure A for adjustmentSets(); test interaction in the regression.
# Published proposal: https://doi.org/10.1093/ije/dyac126
dag_int <- dagitty('dag {
  stop_smoke [exposure, pos="0,2"]
  lung_ca [outcome, pos="5,0"]
  asbestos [pos="0,-2"]
  interaction [pos="2,0"]
  C [pos="2,4"]
  stop_smoke -> lung_ca
  asbestos -> lung_ca
  stop_smoke -> interaction
  asbestos -> interaction
  interaction -> lung_ca
  C -> stop_smoke
  C -> asbestos
  C -> lung_ca
}')
plot(dag_int)
# interaction represents stop_smoke × asbestos; the drawing does not prove it exists.

# 5. Analysis and presentable tables
# 5a. Compare lung cancer between smoking groups
# Descriptive check 1: overall comparison.
tab_group <- dat %>%
  tbl_summary(
    by = stop_smoke, include = lung_ca,
    type = lung_ca ~ "categorical",
    statistic = all_categorical() ~ "{n} ({p}%)",
    label = lung_ca ~ "Later lung cancer"
  ) %>%
  add_p(test = lung_ca ~ "chisq.test.no.correct") %>%
  modify_caption("Lung cancer by baseline smoking group")
tab_group
# Percentages are within each smoking group; compare the Yes row.

# Descriptive check 2: repeat the comparison within each asbestos group.
tab_cancer <- dat %>%
  tbl_strata(
    strata = asbestos, .header = "**Asbestos: {strata}**",
    .tbl_fun = ~ .x %>%
      tbl_summary(
        by = stop_smoke, include = lung_ca,
        type = lung_ca ~ "categorical",
        statistic = all_categorical() ~ "{n} ({p}%)",
        label = lung_ca ~ "Later lung cancer"
      ) %>%
      add_p(test = lung_ca ~ "chisq.test.no.correct")
  ) %>%
  modify_caption("Lung cancer by smoking status, within each asbestos group")
tab_cancer
# Each column is one smoking–asbestos group; read the Yes row for cancer %.
# These are unadjusted comparisons. Separate p-values do not test interaction.
# Use the interaction model below to compare the effects between asbestos groups.


# 5b. Display the descriptive table
# Mean (SD) for age and pack-years; median (Q1, Q3) for income; n (%) for categories.
tab_desc <- dat %>%
  tbl_summary(
    by = asbestos,
    include = c(age, sex, pack_yr, income_pc, stop_smoke, lung_ca),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     income_pc ~ "{median} ({p25}, {p75})"),
    type = list(c(stop_smoke, lung_ca) ~ "categorical"),
    label = list(age ~ "Age (years)", sex ~ "Sex",
                 pack_yr ~ "Baseline pack-years",
                 income_pc ~ "Annual income (RM per person)",
                 stop_smoke ~ "Baseline smoking status", lung_ca ~ "Later lung cancer")
  ) %>%
  add_overall() %>%
  modify_caption("Participant characteristics and follow-up outcomes by asbestos exposure")
tab_desc


# 5c. Fit the outcome model with interaction
# * includes cessation, asbestos and their interaction.
# family = binomial() fits logistic regression for the binary cancer outcome.
fit <- glm(lung_ca ~ stop_smoke * asbestos + age + sex + pack_yr + income_pc,
           family = binomial(), data = dat)
summary(fit)
# summary() gives log-odds coefficients; exp(beta) gives odds ratios.
# The stop_smoke coefficient applies to asbestos = No, not the whole cohort.

# broom::tidy() converts model results into a table for selecting estimates.
# conf.int adds 95% CIs; exponentiate converts estimates and CIs to ORs.
est <- broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE)
est


# 5d. Inspect the interaction estimate and CI
inter <- est %>%
  filter(term == "stop_smokeStopped smoking:asbestosYes") %>%
  dplyr::select(estimate, conf.low, conf.high, p.value)
inter
# This estimate is the RATIO of cessation ORs: asbestos Yes / No. Null = 1.
# Read its size, 95% CI and p-value together; it is not a group-specific cancer OR.

# 6. Interpret the interaction result
# If p < 0.05: evidence that the ORs differ. Retain the interaction and report
# the cessation OR in EACH asbestos group, plus their interaction comparison.
# If p >= 0.05: no clear evidence of a difference. Examine the CI for uncertainty;
# do not conclude the effects are equal or drop the interaction automatically.
# Report planned group estimates in either case. A common-effect summary needs
# scientific justification, not just a non-significant interaction p-value.

# 7. Adjusted comparisons: estimate and interpret each group OR
# Repeat the descriptive comparison using multivariable logistic regression.
# Both group ORs come from the model containing stop_smoke * asbestos.
# Relevelling below re-expresses the same model; it does not split the sample.
# 7a. Asbestos = No: extract cessation from the original model.
or_no <- est %>%
  filter(term == "stop_smokeStopped smoking")
or_no

# 7b. Asbestos = Yes: change the reference and refit the same interaction model.
dat_yes <- dat %>%
  mutate(asbestos = relevel(asbestos, ref = "Yes"))
fit_yes <- update(fit, data = dat_yes)
or_yes <- broom::tidy(fit_yes, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term == "stop_smokeStopped smoking")
or_yes
# Changing the reference changes the coefficient's meaning, not fitted probabilities.

res <- bind_rows(or_no, or_yes, est %>% filter(term == "stop_smokeStopped smoking:asbestosYes")) %>%
  mutate(Measure = c("Adjusted cessation OR: asbestos No", "Adjusted cessation OR: asbestos Yes",
                     "Interaction: ratio of ORs (Yes / No)")) %>%
  dplyr::select(Measure, Estimate = estimate, Lower = conf.low,
                Upper = conf.high, p.value)
res

# 7c. Display both group ORs and their interaction comparison
tab_res <- res %>%
  as_gtsummary() %>%
  modify_fmt_fun(c(Estimate, Lower, Upper) ~ label_style_number(digits = 2),
                 p.value ~ label_style_pvalue(digits = 3)) %>%
  modify_column_merge(pattern = "{Lower} to {Upper}") %>%
  modify_header(Measure = "**Measure**", Estimate = "**Estimate**",
                Lower = "**95% CI**", p.value = "**p-value**") %>%
  modify_caption("Adjusted cessation–lung cancer association by asbestos exposure") %>%
  modify_source_note(
    "Cessation versus continued smoking within each asbestos group. Adjusted for age, sex, baseline pack-years and annual income per person. Both group ORs come from the same interaction model. The final row compares the ORs (asbestos Yes / No). OR = odds ratio; CI = confidence interval."
  )
tab_res


# For each group, compare STOPPED with CONTINUED smoking:
# OR < 1 indicates lower cancer odds; OR > 1 indicates higher odds.
# For example, OR = 0.48 means about 52% lower ODDS, not 52% lower risk.
# For these data, ORs are about 0.48 (No) and 0.19 (Yes): a stronger inverse
# association in the asbestos group. Their ratio is about 0.39, p = 0.002.
# This is interaction on the ODDS scale; it need not hold on other scales.
# Do not infer a group difference from one subgroup being significant and the other not.

# 8. Write the report
# Methods: State exposure coding, moderator reference, covariates and interaction.
# Results: Cessation OR without asbestos = __ (95% CI __ to __);
# with asbestos = __ (__ to __); ratio of ORs = __ (__ to __), p = __.
# Conclusion (answer the question): The cessation–cancer association was
# [stronger / weaker / not clearly different] in those with prior asbestos exposure,
# on the odds-ratio scale. Use the interaction estimate and CI to support this
# answer; a causal interpretation requires the stated assumptions.

# 9. Save the figures and tables

ft_group <- as_flex_table(tab_group) %>%
  fontsize(size = 10, part = "all") %>%
  set_table_properties(layout = "autofit", width = 1)
save_as_docx(ft_group,
             path = "output/tables/s08_groups.docx")
ft_cancer <- as_flex_table(tab_cancer) %>%
  fontsize(size = 10, part = "all") %>%
  set_table_properties(layout = "autofit", width = 1)
save_as_docx(ft_cancer,
             path = "output/tables/s08_cancer_groups.docx")


ft_desc <- as_flex_table(tab_desc) %>%
  fontsize(size = 10, part = "all") %>%
  set_table_properties(layout = "autofit", width = 1)
save_as_docx(ft_desc, path = "output/tables/s08_descriptive.docx")

ft <- as_flex_table(tab_res) %>%
  fontsize(size = 10, part = "all") %>%
  set_table_properties(layout = "autofit", width = 1)
save_as_docx(ft, path = "output/tables/s08_results.docx")

p_dag <- ggdag(dag, node = FALSE, text = FALSE) +
  geom_dag_label(aes(label = name), fill = "white", size = 4) +
  theme_dag() +
  scale_x_continuous(expand = expansion(mult = 0.25)) +
  scale_y_continuous(expand = expansion(mult = 0.15))
ggsave("output/figures/s08_dag.png", p_dag, width = 9, height = 6, dpi = 300)

p_int <- ggdag(dag_int, node = FALSE, text = FALSE) +
  geom_dag_label(aes(label = ifelse(name == "interaction",
                                  "stop_smoke × asbestos", name)),
                 fill = "white", size = 4) +
  theme_dag() +
  scale_x_continuous(expand = expansion(mult = 0.25)) +
  scale_y_continuous(expand = expansion(mult = 0.15))
ggsave("output/figures/s08_interaction_dag.png", p_int,
       width = 9, height = 6, dpi = 300)

# 10. Summary
# End by answering the opening question, supported by the estimates and their CIs.
# Use the causal DAG for adjustment, the interaction model to compare effects,
# and group-specific ORs to explain the result. Do not compare subgroup p-values.
