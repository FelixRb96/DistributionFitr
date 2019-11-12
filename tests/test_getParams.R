### Tests for getParams
#######################

source("getParams.R")

fam <- list(package="stats", family="beta")
all_params <- get_all_params(fam)
all_params

all_params_defaulted <- get_default_values(all_params, fam)
all_params_defaulted

check_values_for_param("shape1", all_params_defaulted, fam, values=c(-100, -10, -1, 1))
iterate_min_max_vals("shape1", all_params, fam, cur_val=0, step_sizes = c(1, 0.5, 0.1, 0.05))

get_param_ranges(all_params_defaulted, fam)

load("all_families.Rda")
for (fam in families) {
  cat("\nCurrent Family:", fam$family, "\n")
  result <- tryCatch(getParams(fam), error=function(e) warning('Error') )
  print(result)
}
