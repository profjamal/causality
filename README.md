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

## Current student practicals

Each script uses `dat_ori` for the unchanged import and `dat` for preparation, labels, factors and analysis. Questions follow PICO-T or PECO-T where relevant, and conclusions answer the opening question.

- [S00 Revision](practicals/S00%20Revision/practical_revision.R)
- [S05 Confounding & adjustment](practicals/S05%20Confounding%20%26%20adjustment/s05_practical_confounding.R)
- [S06 Measurement Error](practicals/S06%20Measurement%20Error/s06_practical_measurement.R)
- [S07 Mediation](practicals/S07%20Mediation/s07_practical_mediation.R)
- [S08 Moderation](practicals/S08%20Moderation/s08_practical_moderation.R)
- [S09-S10 Missing Data](practicals/S09-S10%20Missing%20Data/s09_s10_practical_missing.R)

Run each script from its own working folder. It creates the data and output folders and imports the dataset from GitHub. The book manuscript, PDF and book downloads are maintained locally and are not published in this repository.
