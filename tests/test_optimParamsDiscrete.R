# Test for discrete optimization
####################

source("optimParamsDiscrete.R")

load('all_families.Rda')

# define fake distribution with two integer parameters
info <- family_list[[1]]$family_info
info$accepts_float["ncp"] <- FALSE
info$accepts_float["shape1"] <- FALSE
info$defaults["ncp"] <- 0
info$defaults["shape1"] <- 1
info$lower["shape1"] <-1

r <- optimParamsDiscrete(rbeta(n = 1000, shape1 = 5, shape = 10, ncp = 2), family = family_list[[2]][c('package', 'family')], 
                         family_info = info, debug_error=F, max_discrete_steps = 100, plot=T,
                         discrete_fast = FALSE, show_optim_progress = FALSE)

r

r2 <- optimParamsDiscrete(rnorm(n=1e6, mean=2, sd=0.5), family = family_list[[12]][c('package', 'family')],
                          family_info = family_list[[12]]$family_info, debug_error=F)
r2
r2$par
