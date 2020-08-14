# Test for discrete optimization
source("optimParamsDiscrete.R")
family_list <- dget("all_families.R")
# define fake distribution with two integer parameters
info <- family_list[[1]]$family_info
info$accepts_float["ncp"] <- FALSE
info$accepts_float["shape1"] <- FALSE
info$defaults["ncp"] <- 0
info$defaults["shape1"] <- 1
info$lower["shape1"] <- 1

r1 <- optimParamsDiscrete(rbeta(n = 1000, shape1 = 5, shape2 = 10, ncp = 2),
  family = family_list[[1]][c(
    "package",
    "family"
  )], family_info = info, debug_error = F, max_discrete_steps = 100, plot = T,
  discrete_fast = FALSE, show_optim_progress = FALSE
)

r1
r1$par

# use the same distribution, but change parameter to trigger Google Earth

family_list <- dget("all_families.R")
info <- family_list[[1]]$family_info

r2 <- optimParamsDiscrete(rbeta(n = 1000, shape1 = 30, shape2 = 10, ncp = 2),
  family = family_list[[1]][c(
    "package",
    "family"
  )], family_info = info, debug_error = F, max_discrete_steps = 100, plot = T,
  discrete_fast = FALSE, show_optim_progress = FALSE
)

r2
r2$par

# binom as an example where exactly one parameter accepts integers only

r3 <- optimParamsDiscrete(rnorm(n = 1e+06, mean = 2, sd = 0.5), family = family_list[[12]][c(
  "package",
  "family"
)], family_info = family_list[[12]]$family_info, debug_error = F)
r3
r3$par

# hyper as three-dimensional grid search
r4 <- optimParamsDiscrete(rhyper(nn = 10000, m = 20, n = 10, k = 1), family = family_list[[9]][c(
  "package",
  "family"
)], family_info = family_list[[9]]$family_info, debug_error = F)
r4
r4$par
