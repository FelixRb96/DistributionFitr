list(list(package = "Rlab", family = "bern", family_info = list(
    lower = c(prob = 0), upper = c(prob = 1), accepts_float = c(prob = TRUE), 
    defaults = c(prob = 0.5), log = TRUE, discrete = TRUE, support_min = 0, 
    support_max = 1, supp_max_depends_on = c(prob = FALSE), supp_min_depends_on = c(prob = FALSE))), 
    list(package = "Rlab", family = "exp", family_info = list(
        lower = c(rate = 0.01), upper = c(rate = Inf), accepts_float = c(rate = TRUE), 
        defaults = c(rate = 1), log = TRUE, discrete = FALSE, 
        support_min = 0, support_max = Inf, supp_max_depends_on = c(rate = FALSE), 
        supp_min_depends_on = c(rate = FALSE))), list(package = "Rlab", 
        family = "gamma", family_info = list(lower = c(shape = -Inf, 
        rate = 0.01, alpha = 8.32667268468867e-17, beta = 8.32667268468867e-17
        ), upper = c(shape = Inf, rate = Inf, alpha = Inf, beta = Inf
        ), accepts_float = c(shape = TRUE, rate = TRUE, alpha = TRUE, 
        beta = TRUE), defaults = c(shape = 1, rate = 1, alpha = 0.5, 
        beta = 0.5), log = TRUE, discrete = FALSE, support_min = 0, 
            support_max = Inf, supp_max_depends_on = c(shape = FALSE, 
            rate = FALSE, alpha = FALSE, beta = FALSE), supp_min_depends_on = c(shape = FALSE, 
            rate = FALSE, alpha = FALSE, beta = FALSE))), list(
        package = "Rlab", family = "weibull", family_info = list(
            lower = c(shape = -Inf, scale = -Inf, alpha = 8.32667268468867e-17, 
            beta = 8.32667268468867e-17), upper = c(shape = Inf, 
            scale = Inf, alpha = Inf, beta = Inf), accepts_float = c(shape = TRUE, 
            scale = TRUE, alpha = TRUE, beta = TRUE), defaults = c(shape = 1, 
            scale = 1, alpha = 0.5, beta = 0.5), log = TRUE, 
            discrete = FALSE, support_min = 0, support_max = Inf, 
            supp_max_depends_on = c(shape = FALSE, scale = FALSE, 
            alpha = FALSE, beta = FALSE), supp_min_depends_on = c(shape = FALSE, 
            scale = FALSE, alpha = FALSE, beta = FALSE))))
