# Causality in Epidemiology

Teaching resources by Jamalludin Ab Rahman, Kulliyyah of Medicine, IIUM.

## Current synthetic smoking cohort

3,000 fictional participants aged 30–65 with a smoking history in 30 clinics; 33 columns. No real participants.

- `cessation`: seven-day abstinence at baseline (1), compared with smoking at baseline (0). Later relapse or quitting is possible.
- `sustained_abstinence`: zero reported smoking in every month 2–12 (1), otherwise 0. Month 1 is a grace period. This is the binary mediator in the current smoking example.
- `later_tobacco_exposure`: mean cigarettes/day across months 1–12, retained as a descriptive variable.
- `pack_yr`: cumulative smoking before baseline, in pack-years.
- `income_pc`: annual household income divided by household members, RM/person/year. These are simulated teaching values, not Malaysian population income norms.
- `asbestos_exposure`: occupational exposure before baseline.
- Cancer is generated after year 1 through year 20.

Use `lung_cancer_complete` for complete-data teaching examples. Use `lung_cancer`, retaining its NAs, for selection and missing-data examples. Do not use the complete simulated truth to fill missing outcomes in an analysis representing a real study. Models contain deliberately omitted causes; fitted effects require causal assumptions.

## Files

- [Current dataset](data/smoking_cohort_demo.csv)
- [Codebook](codebook/smoking_cohort_codebook.md)
- [Reproducible generator](scripts/generate_demo_data.R)
- [Mediator definition](data/smoking_mediation_definition.txt)

```r
url <- "https://raw.githubusercontent.com/profjamal/causality/main/data/smoking_cohort_demo.csv"
dat_ori <- read.csv(url)
```

Keep the raw import as `dat_ori`. Select, rename and label the analysis variables, and set factor levels, in `dat`. Use `dat` for tables, plots and models. Define PICO-T (or PECO-T for an observed exposure) before analysis and answer that same question in the conclusion.

The current smoking dataset is unchanged in this documentation update. The retired walking-mediation example is no longer part of the current teaching set; earlier versions remain in Git history.
