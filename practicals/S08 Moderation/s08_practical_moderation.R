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
#                    "gtsummary", "flextable"))
library(dplyr)
library(labelled)
library(dagitty)
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
# Four analysis steps:
# 1. Describe cancer by smoking group, overall and within asbestos groups.
# 2. Fit the adjusted outcome model with interaction.
# 3. Present and assess the interaction term using gtsummary.
# 4. Present the adjusted cessation OR in each asbestos group.
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
# 5a. Step 1: describe lung cancer in the comparison groups
# Descriptive check 1: overall comparison.
tab_group <- dat %>%
  tbl_summary(
    by = stop_smoke, include = lung_ca,
    type = lung_ca ~ "categorical",
    statistic = all_categorical() ~ "{n} ({p}%)"
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
        statistic = all_categorical() ~ "{n} ({p}%)"
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
    type = lung_ca ~ "categorical"
  ) %>%
  add_overall() %>%
  modify_caption("Participant characteristics and follow-up outcomes by asbestos exposure")
tab_desc


# 5c. Step 2: fit the adjusted model with interaction
# * includes cessation, asbestos and their interaction.
# family = binomial() fits logistic regression for the binary cancer outcome.
fit <- glm(lung_ca ~ stop_smoke * asbestos + age + sex + pack_yr + income_pc,
           family = binomial(), data = dat)
summary(fit)
# summary() gives log-odds coefficients; exp(beta) gives odds ratios.
# The stop_smoke coefficient applies to asbestos = No, not the whole cohort.

# 5d. Step 3: present and assess the interaction term
# gtsummary reads the fitted model directly and displays ORs, CIs and p-values.
tab_int <- tbl_regression(
  fit, exponentiate = TRUE,
  include = "stop_smoke:asbestos",
  show_single_row = "stop_smoke:asbestos",
  label = list(`stop_smoke:asbestos` ~ "Interaction: ratio of ORs (Yes / No)")
)
tab_int
# This is the RATIO of cessation ORs: asbestos Yes / No. Null = 1.
# It tests whether the cessation OR differs between the two asbestos groups.

# 6. Interpret the interaction result
# If p < 0.05: evidence that the ORs differ. Retain the interaction and report
# the cessation OR in EACH asbestos group, plus their interaction comparison.
# If p >= 0.05: no clear evidence of a difference. Examine the CI for uncertainty;
# do not conclude the effects are equal or drop the interaction automatically.
# Report planned group estimates in either case. A common-effect summary needs
# scientific justification, not just a non-significant interaction p-value.

# 7. Step 4: present the adjusted cessation OR in each asbestos group
# 7a. Asbestos = No: use the original model's reference group.
tab_no <- tbl_regression(
  fit, exponentiate = TRUE, include = stop_smoke,
  show_single_row = stop_smoke,
  label = list(stop_smoke ~ "Cessation OR: asbestos No")
)

# 7b. Asbestos = Yes: change the reference and refit the SAME model.
dat_yes <- dat %>%
  mutate(asbestos = relevel(asbestos, ref = "Yes"))
fit_yes <- update(fit, data = dat_yes)
tab_yes <- tbl_regression(
  fit_yes, exponentiate = TRUE, include = stop_smoke,
  show_single_row = stop_smoke,
  label = list(stop_smoke ~ "Cessation OR: asbestos Yes")
)
# Changing the reference changes the coefficient's meaning, not fitted risks.
# Both group ORs use all participants; these are not separate subgroup models.

# 7c. Put both group ORs and the interaction comparison in one table.
tab_res <- tbl_stack(list(tab_no, tab_yes, tab_int)) %>%
  modify_header(label = "**Comparison**", estimate = "**Adjusted OR / ratio**") %>%
  modify_caption("Adjusted cessation–lung cancer association by asbestos exposure") %>%
  modify_source_note(
    "Stopped versus continued smoking. Adjusted for age, sex, baseline pack-years and annual income per person. The final row is the ratio of group ORs (asbestos Yes / No)."
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
# Describe the groups, fit the interaction model, assess interaction, then report
# each group OR. Use the DAG for adjustment; do not compare subgroup p-values.
