# Causality in Epidemiology

Teaching resources for *Causality in Epidemiology* by Jamalludin Ab Rahman, Kulliyyah of Medicine, International Islamic University Malaysia (IIUM).

## Demonstration dataset

The repository contains a synthetic longitudinal cohort used to explain causal inference with R. It includes baseline confounders, smoking cessation, later tobacco exposure as a mediator, asbestos exposure as a moderator, missing outcomes, clinic-level variables, and indicators for a latent nicotine-dependence construct.

The dataset contains no real participants and must not be used for clinical or policy decisions.

## Repository structure

- `data/smoking_cohort_demo.csv`: synthetic cohort of 3,000 current smokers.
- `scripts/generate_demo_data.R`: reproducible data-generating script.
- `codebook/smoking_cohort_codebook.md`: variable definitions and teaching roles.

## Import into R

```r
data_url <- paste0(
  "https://raw.githubusercontent.com/profjamal/",
  "causality/main/data/smoking_cohort_demo.csv"
)
demo <- read.csv(data_url)
```

An internet connection is required for direct import. The accompanying Quarto book project also contains a local copy for offline use.
