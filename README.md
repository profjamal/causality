# Causality in Epidemiology

Teaching resources by Jamalludin Ab Rahman, Kulliyyah of Medicine, International Islamic University Malaysia (IIUM).

## Synthetic cohort: revised 28 October 2026

3,000 fictional baseline smokers nested in 30 clinics. No real participants; do not use these data as clinical or policy evidence.

The mediator `later_tobacco_exposure` now measures **average cigarettes/day during months 1-12**, calculated from the 12 monthly values `cigs_m01` through `cigs_m12`. It is no longer a unitless score. The monthly cigarette-use values and cancer outcomes were regenerated, so earlier numerical results must be recalculated.

`cessation` means seven-day smoking abstinence achieved at the baseline visit; later relapse or quitting is possible. Cancer is assessed after the first-year mediator window through year 20.

Use `lung_cancer_complete` for the complete-data introductory mediation exercise; use `lung_cancer` for the selection and missing-data lessons. The generator includes omitted causes for sensitivity exercises; a model fitted to these data is not automatically causally identified.

## Files

- `data/smoking_cohort_demo.csv`: revised 3,000-row, 30-column dataset.
- `scripts/generate_demo_data.R`: reproducible generator, seed 8311.
- `codebook/smoking_cohort_codebook.md`: measurement definitions and teaching roles.

## Import

```r
url <- "https://raw.githubusercontent.com/profjamal/causality/main/data/smoking_cohort_demo.csv"
dat <- read.csv(url)
```

Direct import needs internet. The Quarto book also includes a matching local copy for reproducible offline analysis. Run the generator from the repository root to recreate the CSV.
