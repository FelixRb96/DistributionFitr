# Test for discrete optimization
####################
source("optimParamsDiscrete.R")

load('all_families.Rda')


info <- family_list[[2]]$family_info
info$defaults["size"] <- 12
r <- optimParamsDiscrete(rbinom(n=1000, size=10, prob=0.9), family = family_list[[2]][c('package', 'family')], 
                         family_info = info, debug_error=F, max_discrete_steps = 30, plot=T,
                         discrete_fast = TRUE, show_optim_progress = FALSE)

r

r2 <- optimParamsDiscrete(rnorm(n=1e6, mean=2, sd=0.5), family = family_list[[12]][c('package', 'family')],
                          family_info = family_list[[12]]$family_info, debug_error=F)
r2
r2$par
