# Smoking cohort demonstration data: codebook

Revision: 23 November 2026. Completely synthetic data: 3,000 fictional baseline smokers in 30 clinics. No record represents a real person. The monthly cigarette-use values, exposure and outcomes are generated for teaching, not clinical inference.

## Time order and measurement

Baseline information is recorded before the exposure classification. `cessation = 1` means seven-day smoking abstinence achieved at the baseline visit; `0` means smoking at that visit. Later relapse or quitting can occur. This variable does not guarantee sustained abstinence.

At each of months 1-12, the fictional participant reports average cigarettes smoked per day during that month. `cigs_m01` to `cigs_m12` contain these simulated monthly values, including zero for a smoke-free month. `later_tobacco_exposure` is their equal-month arithmetic mean, rounded to one decimal place. It is measured in **cigarettes/day**, not a unitless score or pack-years. Six months at zero and six months at 10 cigarettes/day give 5 cigarettes/day. This is a defined teaching measurement protocol, not an existing validated instrument.

Lung cancer is generated after the first-year mediator window, through year 20. The complete simulated outcome is used for the complete-data teaching exercises (DAGs, adjustment, measurement error, mediation, moderation, SEM and sensitivity). A separate column deliberately hides some outcomes for the missing-data and selection exercises.

| Variable | Definition / unit | Role |
|---|---|---|
| `id` | Unique fictional participant ID, 1-3000 | Identifier |
| `clinic_id` | Clinic ID, 1-30 | Clustering |
| `clinic_support` | Simulated baseline support score | Contextual cause |
| `clinic_deprivation` | Simulated baseline deprivation score | Contextual cause |
| `age` | Age at baseline, years | Baseline covariate |
| `sex` | female / male | Baseline covariate |
| `hh_income` | Gross annual household income from all sources in the 12 months before baseline, RM/year; synthetic | Component of income calculation |
| `hh_size` | Number of usual household members, including children, at baseline; integer 1-6 | Denominator of income calculation |
| `income_pc` | `hh_income / hh_size`: annual income per capita, RM/person/year; synthetic, not an SES score | Baseline covariate |
| `asbestos_exposure` | Previous occupational exposure: 0 = no, 1 = yes | Cause of cancer / effect modifier |
| `unmeasured_health_risk` | Simulated hidden risk: 0 = absent, 1 = present | Omitted-cause sensitivity exercise |
| `cigarettes_per_day` | Cigarettes/day before baseline cessation | Nicotine-dependence indicator |
| `morning_urgency` | Simulated score, 0-10 | Nicotine-dependence indicator |
| `withdrawal_score` | Simulated score, 0-20 | Nicotine-dependence indicator |
| `pack_yr` | Cumulative smoking before baseline, pack-years | Baseline covariate |
| `cessation` | Seven-day abstinence at baseline: 1 = yes, 0 = smoking at baseline; relapse is allowed later | Exposure |
| `cigs_m01` through `cigs_m12` | Average cigarettes/day for each of months 1-12; zero for a smoke-free month | Monthly mediator measurements |
| sustained_abstinence | 1 = zero reported smoking in every month 2–12; 0 = any smoking in those months; month 1 is a grace period | Binary mediator |
| `later_tobacco_exposure` | Mean of the 12 monthly values, rounded to one decimal; cigarettes/day | Descriptive follow-up consumption |
| `lung_cancer_complete` | Cancer after year 1 through year 20: 0 = no, 1 = yes, fully observed in the simulation | Complete-data teaching outcome |
| `followup_status` | Whether the cancer outcome is observed: 1 = yes, 0 = no | Missingness indicator |
| `lung_cancer` | Same cancer outcome, but blank when followup_status = 0 | Missing-data / selection outcome |

The dataset has 33 columns. All variables except `lung_cancer` are complete. Keep the hidden health-risk variable out of the primary working models unless explicitly studying sensitivity to that omitted cause. Using `lung_cancer_complete` in complete-data lessons is a deliberate complete-data teaching choice, not a way to repair missing outcomes in a real study.

## Annual income per capita

Income per capita is annual gross household income divided by the number of usual household members, including children. It is not equivalised income, individual earnings, monthly income, or a multi-component socioeconomic-status index. The unit is Malaysian ringgit per person per year. For example, RM120,000/year for a household of four gives RM30,000/person/year.

All values are synthetic. The generator draws an internal centred log-income variable related to clinic deprivation, then sets `income_pc = round(30000 * exp(0.5 * log_income))`. It generates household size with a separate seed (8312) and constructs `hh_income = income_pc * hh_size`, so the division is exact. RM30,000 and the distribution are illustrative design choices, not estimates of Malaysian population income. This revision preserves the other simulated participant values and outcomes while replacing the former unitless score with defined monetary data. Models must be recalculated because adjustment for linear income differs from adjustment for the former score.

`pack_yr` is cumulative smoking before baseline, in pack-years. In an actual study it is calculated from smoking intensity and duration (packs/day times years, summed across periods). Here it is simulated directly: the dataset has no smoking-duration history from which to reconstruct it.

## Reproduce and verify

Run `scripts/generate_demo_data.R` from the repository root. Its fixed seed reproduces the CSV. The local Quarto project uses the equivalent script `scripts/generate-demo-data.R`.

```r
dat <- read.csv("data/smoking_cohort_demo.csv")
months <- sprintf("cigs_m%02d", 1:12)
stopifnot(all(dat$later_tobacco_exposure ==
              round(rowMeans(dat[months]), 1)))
```

The October 28 revision regenerated cigarette-use trajectories and cancer outcomes. The October 29 income revision preserves those trajectories and outcomes but changes the income adjustment variable, so model results must be recalculated. The data-generating mechanism still includes intentionally omitted causes. Correct units alone do not identify causal effects. Zero-heavy monthly consumption also requires checking the linear mediator model used for the introductory exercise.

## Revised abstinence pathway

sustained_abstinence is 1 when every cigs_m02 through cigs_m12 value is zero and 0 otherwise. Month 1 is a grace period. Baseline seven-day cessation is a different variable; some baseline smokers quit during month 1, and some baseline quitters subsequently relapse.

The fixed-seed generator first assigns the binary follow-up state from baseline cessation, age, prior pack-years and income, then produces compatible monthly reports. The cancer model includes sustained abstinence, other cessation pathways, age, prior pack-years, income, asbestos exposure, a cessation-by-asbestos interaction and the intentionally omitted health-risk cause. This is a simplified fictional mechanism, not evidence about clinical effect sizes or guaranteed later abstinence.

The dataset has 3,000 records and 33 columns; 405 complete cancer events; 734 observed cancer outcomes are missing. All baseline data, participants and income definitions are preserved. Smoking trajectories, complete cancer outcomes and observation flags were regenerated with the same fixed seed. Do not mix outputs from the October version with this revision. All actual numerical results must be rerun.

Known relationships: 335/1446 baseline smokers and 1047/1554 baseline quitters achieve sustained abstinence. Prior asbestos exposure modifies the cessation effect on the log-odds scale. Later tobacco consumption is retained as a descriptive average; the primary mediation analysis now uses a binary mediator and two logistic regressions.

These monthly entries are simulated self-reports, not biomarkers. Real studies require clear lapse rules, repeated assessment, suitable validation and longer-term smoking histories.
