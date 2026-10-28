# Causality in Epidemiology

Teaching resources by Jamalludin Ab Rahman, Kulliyyah of Medicine, IIUM.

## Revised synthetic cohort 29 October 2026

3,000 fictional baseline smokers in 30 clinics, 32 columns. No real participants.

- `pack_yr`: cumulative smoking before baseline, in pack-years.
- `income_pc`: annual household income per member, RM/person/year.
- `hh_income`: gross household income from all sources over the preceding 12 months, RM/year.
- `hh_size`: usual household members at baseline, including children, integer 1-6.

`income_pc = hh_income / hh_size`. For example, RM120,000/year divided by four members is RM30,000/person/year. All values are generated for teaching, not measured in a real survey and not Malaysian income norms. The old unitless socioeconomic score is not exported. `pack_yr` replaces the former `previous_pack_years` column; update old scripts to the current names.

The generator uses an illustrative log-income distribution centred on RM30,000/person/year and a separate household-size seed. The smoking histories, cessation classification and cancer outcomes from the October 28 revision are preserved, but adjusted model estimates must be recalculated with the new income variable.

Cessation means seven-day abstinence at baseline; later relapse or quitting is possible. `later_tobacco_exposure` is average cigarettes/day across months 1-12. Cancer is generated after that first-year window through year 20.

Use `lung_cancer_complete` for complete-data teaching examples. Use `lung_cancer`, retaining its NAs, for selection and missing-data lessons. Models include deliberately omitted causes and do not identify causal effects automatically.

## Files

- `data/smoking_cohort_demo.csv`: current 3,000-row, 32-column dataset.
- `scripts/generate_demo_data.R`: reproducible generator, seeds 8311 and 8312.
- `codebook/smoking_cohort_codebook.md`: definitions, units and teaching roles.

```r
url <- "https://raw.githubusercontent.com/profjamal/causality/main/data/smoking_cohort_demo.csv"
dat <- read.csv(url)
stopifnot(all(dat$income_pc == dat$hh_income / dat$hh_size))
```

The book and practical packages include the same CSV for offline use.
