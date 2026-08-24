# Smoking cohort demonstration data: codebook

This completely synthetic dataset contains 3,000 fictional current smokers nested within 30 clinics. It was generated for the R practicals in *Causality in Epidemiology*. No record represents a real person.

| Variable | Description | Type or coding | Main teaching role |
|---|---|---|---|
| `id` | Fictional participant identifier | Integer | Record identifier |
| `clinic_id` | Clinic in which the person receives care | Integer, 1–30 | Clustering or multilevel structure |
| `clinic_support` | Availability of cessation support at the clinic | Continuous standardized score | Contextual cause of cessation |
| `clinic_deprivation` | Deprivation surrounding the clinic | Continuous standardized score | Contextual common cause |
| `age` | Age at baseline | Years | Baseline confounder |
| `sex` | Recorded sex | `female`, `male` | Baseline characteristic |
| `socioeconomic_position` | Individual socioeconomic position | Continuous standardized score; higher means more advantaged | Baseline confounder |
| `asbestos_exposure` | Previous occupational asbestos exposure | 0 = no, 1 = yes | Proposed moderator and cause of lung cancer |
| `unmeasured_health_risk` | Simulated baseline health risk deliberately omitted from primary models | 0 = no, 1 = yes | Residual-confounding and sensitivity teaching |
| `cigarettes_per_day` | Cigarettes smoked per day at baseline | Count | Indicator of latent nicotine dependence |
| `morning_urgency` | Urgency to smoke after waking | Score, 0–10 | Indicator of latent nicotine dependence |
| `withdrawal_score` | Withdrawal symptoms | Score, 0–20 | Indicator of latent nicotine dependence |
| `previous_pack_years` | Smoking accumulated before baseline | Pack-years | Baseline confounder |
| `cessation` | Sustained smoking cessation beginning at baseline | 0 = continued smoking, 1 = cessation | Exposure or intervention of interest |
| `later_tobacco_exposure` | Tobacco exposure after the baseline cessation decision | Continuous score | Mediator |
| `lung_cancer_complete` | Simulated outcome before missingness was introduced | 0 = no, 1 = yes | Simulation truth; not for primary analysis |
| `followup_observed` | Whether lung-cancer status was observed | 0 = missing, 1 = observed | Missingness or selection indicator |
| `lung_cancer` | Observed lung-cancer outcome | 0 = no, 1 = yes, blank = missing | Primary outcome |

## Important teaching note

The variables `unmeasured_health_risk` and `lung_cancer_complete` would not normally be available in a real study. They are retained so learners can compare an analysis with the known data-generating process. They should be excluded from the primary causal analysis.

The causal role of a variable depends on the causal question. The roles above refer to the book’s principal comparison of sustained smoking cessation versus continued smoking and subsequent lung-cancer risk.
