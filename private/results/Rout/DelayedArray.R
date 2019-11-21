list(list(package = "DelayedArray", family = "binom", family_info = list(
    lower = c(size = 0, prob = 0), upper = c(size = Inf, prob = 1
    ), accepts_float = c(size = FALSE, prob = TRUE), defaults = c(size = 1, 
    prob = 0.5), log = TRUE, discrete = TRUE, support_min = 0, 
    support_max = Inf, supp_max_depends_on = c(size = TRUE, prob = FALSE
    ), supp_min_depends_on = c(size = FALSE, prob = FALSE))), 
    list(package = "DelayedArray", family = "logis", family_info = list(
        lower = c(location = -Inf, scale = 0.01), upper = c(location = Inf, 
        scale = Inf), accepts_float = c(location = TRUE, scale = TRUE
        ), defaults = c(location = 0, scale = 1), log = TRUE, 
        discrete = FALSE, support_min = -Inf, support_max = Inf, 
        supp_max_depends_on = c(location = FALSE, scale = FALSE
        ), supp_min_depends_on = c(location = FALSE, scale = FALSE
        ))), list(package = "DelayedArray", family = "norm", 
        family_info = list(lower = c(mean = -Inf, sd = 0), upper = c(mean = Inf, 
        sd = Inf), accepts_float = c(mean = TRUE, sd = TRUE), 
            defaults = c(mean = 0, sd = 1), log = TRUE, discrete = FALSE, 
            support_min = -Inf, support_max = Inf, supp_max_depends_on = c(mean = FALSE, 
            sd = FALSE), supp_min_depends_on = c(mean = FALSE, 
            sd = FALSE))), list(package = "DelayedArray", family = "pois", 
        family_info = list(lower = c(lambda = 0), upper = c(lambda = Inf), 
            accepts_float = c(lambda = TRUE), defaults = c(lambda = 0.5), 
            log = TRUE, discrete = TRUE, support_min = 0, support_max = Inf, 
            supp_max_depends_on = c(lambda = FALSE), supp_min_depends_on = c(lambda = FALSE))))
