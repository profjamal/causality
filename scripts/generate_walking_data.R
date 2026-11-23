# Synthetic independent participants; seed fixed for reproducibility.
set.seed(8311)
n <- 1000
age <- rnorm(n, 52, 8)
base_act <- rnorm(n, 150, 25)
base_sbp <- rnorm(n, 142, 10)
prog <- rbinom(n, 1, plogis(-0.1 + 0.025*(age-52) -
                            0.015*(base_act-150) + 0.035*(base_sbp-142)))
activity <- 175 + 45*prog + 0.5*(base_act-150) -
  0.4*(age-52) - 0.25*(base_sbp-142) + rnorm(n, 0, 25)
sbp <- 138 - 2*prog - 0.06*(activity-175) +
  0.55*(base_sbp-142) + 0.12*(age-52) -
  0.015*(base_act-150) + rnorm(n, 0, 6)
dat <- data.frame(id=1:n, programme=prog, age,
                  baseline_activity=base_act, baseline_sbp=base_sbp,
                  activity_3mo=activity, sbp_6mo=sbp)
write.csv(dat, "data/walking_mediation.csv", row.names=FALSE)
