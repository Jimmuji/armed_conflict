# Week 10 in-class assignment: simulation study of a binary confounder.
#
# Question: if the true effect of X on Y is zero (beta1 = 0) but a confounder Z
# affects both X and Y, how often does a logistic regression reject H0: beta1 = 0
# at the 0.05 level when Z is left out (unadjusted) and when Z is included
# (adjusted)? Because beta1 = 0, the rejection rate is the Type I error rate.
#
# Run from the project root. It writes outputs/tables/simulation_type1_error.csv
# and needs only base R.

library(parallel)

# ---- simulation settings ---------------------------------------------------
n_obs     <- 500            # sample size
n_sim     <- 1000           # simulation size
sig_level <- 0.05
alpha1_values <- c(0, 1, 2) # log odds ratio of X = 1 for Z = 1 vs Z = 0

pz     <- 0.2   # probability of Z = 1
alpha0 <- 0     # logit probability of X = 1 when Z = 0
beta0  <- -3    # logit probability of Y = 1 when X = 0 and Z = 0
beta1  <- 0     # true effect of X on Y: zero, so rejecting H0 is a Type I error
beta2  <- 2     # log odds ratio of Y = 1 for Z = 1 vs Z = 0

inv_logit <- function(x) exp(x) / (1 + exp(x))

# ---- one simulated data set, fitted with both models ------------------------
run_one <- function(i, alpha1, seed_offset) {
  set.seed(2024 + i + seed_offset)  # a different seed for every run

  z <- rbinom(n_obs, size = 1, prob = pz)
  x <- rbinom(n_obs, size = 1, prob = inv_logit(alpha0 + alpha1 * z))
  y <- rbinom(n_obs, size = 1, prob = inv_logit(beta0 + beta1 * x + beta2 * z))
  dat <- data.frame(lung = y, coffee = x, smoke = z)

  unadj <- glm(lung ~ coffee, data = dat, family = "binomial")
  adj   <- glm(lung ~ coffee + smoke, data = dat, family = "binomial")
  unadj_coef <- summary(unadj)$coef["coffee", ]
  adj_coef   <- summary(adj)$coef["coffee", ]

  c(est_unadj = unname(unadj_coef["Estimate"]),
    p_unadj   = unname(unadj_coef["Pr(>|z|)"]),
    est_adj   = unname(adj_coef["Estimate"]),
    p_adj     = unname(adj_coef["Pr(>|z|)"]),
    converged = unadj$converged && adj$converged)
}

# ---- repeat for each scenario ------------------------------------------------
run_scenario <- function(alpha1, scenario_number) {
  runs <- mclapply(seq_len(n_sim), run_one,
                   alpha1 = alpha1, seed_offset = 10000 * scenario_number,
                   mc.cores = max(1, detectCores() - 1))
  out <- do.call(rbind, runs)

  rate <- function(p) mean(p < sig_level)
  mc_se <- function(p) sqrt(rate(p) * (1 - rate(p)) / n_sim)

  data.frame(
    alpha1 = alpha1,
    type1_adjusted   = rate(out[, "p_adj"]),
    type1_unadjusted = rate(out[, "p_unadj"]),
    mcse_adjusted    = mc_se(out[, "p_adj"]),
    mcse_unadjusted  = mc_se(out[, "p_unadj"]),
    mean_beta1_adjusted   = mean(out[, "est_adj"]),
    mean_beta1_unadjusted = mean(out[, "est_unadj"]),
    n_not_converged = sum(out[, "converged"] == 0)
  )
}

timing <- system.time(
  results <- do.call(rbind, Map(run_scenario, alpha1_values,
                                seq_along(alpha1_values)))
)

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
write.csv(results, "outputs/tables/simulation_type1_error.csv", row.names = FALSE)

print(results, digits = 3, row.names = FALSE)
cat(sprintf("\n%d scenarios x %d simulations x 2 models fitted in %.1f seconds\n",
            length(alpha1_values), n_sim, timing[["elapsed"]]))
