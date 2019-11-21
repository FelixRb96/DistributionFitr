list(list(package = "nimble", family = "cat", family_info = list(
    lower = c(prob = -Inf), upper = c(prob = Inf), accepts_float = c(prob = TRUE), 
    defaults = c(prob = 0.5), log = TRUE, discrete = TRUE, support_min = 1, 
    support_max = 1, supp_max_depends_on = c(prob = FALSE), supp_min_depends_on = c(prob = FALSE))), 
    list(package = "nimble", family = "dexp", family_info = list(
        lower = c(location = -Inf, scale = 0), upper = c(location = Inf, 
        scale = Inf), accepts_float = c(location = TRUE, scale = TRUE
        ), defaults = c(location = 0, scale = 1), log = TRUE, 
        discrete = FALSE, support_min = -Inf, support_max = Inf, 
        supp_max_depends_on = c(location = FALSE, scale = FALSE
        ), supp_min_depends_on = c(location = FALSE, scale = FALSE
        ))), list(package = "nimble", family = "exp_nimble", 
        family_info = list(lower = c(scale = 0.01), upper = c(scale = Inf), 
            accepts_float = c(scale = TRUE), defaults = c(scale = 1), 
            log = TRUE, discrete = FALSE, support_min = 0, support_max = Inf, 
            supp_max_depends_on = c(scale = FALSE), supp_min_depends_on = c(scale = FALSE))), 
    list(package = "nimble", family = "interval", family_info = list(
        lower = c(t = -Inf, c = -Inf), upper = c(t = Inf, c = Inf
        ), accepts_float = c(t = TRUE, c = TRUE), defaults = c(t = 0.5, 
        c = 0.5), log = TRUE, discrete = TRUE, support_min = 0, 
        support_max = 1, supp_max_depends_on = c(t = FALSE, c = FALSE
        ), supp_min_depends_on = c(t = FALSE, c = FALSE))), list(
        package = "nimble", family = "invgamma", family_info = list(
            lower = c(scale = 0, shape = 0.0100000000000001), 
            upper = c(scale = Inf, shape = Inf), accepts_float = c(scale = TRUE, 
            shape = TRUE), defaults = c(scale = 1, shape = 0.5
            ), log = TRUE, discrete = FALSE, support_min = 0.0100000000000051, 
            support_max = Inf, supp_max_depends_on = c(scale = FALSE, 
            shape = FALSE), supp_min_depends_on = c(scale = FALSE, 
            shape = FALSE))), list(package = "nimble", family = "sqrtinvgamma", 
        family_info = list(lower = c(scale = 0, shape = 0.0100000000000001
        ), upper = c(scale = Inf, shape = Inf), accepts_float = c(scale = TRUE, 
        shape = TRUE), defaults = c(scale = 1, shape = 0.5), 
            log = TRUE, discrete = FALSE, support_min = 0.0400000000000063, 
            support_max = Inf, supp_max_depends_on = c(scale = FALSE, 
            shape = FALSE), supp_min_depends_on = c(scale = FALSE, 
            shape = FALSE))), list(package = "nimble", family = "t_nonstandard", 
        family_info = list(lower = c(df = 0.01, mu = -Inf, sigma = 0
        ), upper = c(df = Inf, mu = Inf, sigma = Inf), accepts_float = c(df = TRUE, 
        mu = TRUE, sigma = TRUE), defaults = c(df = 1, mu = 0, 
        sigma = 1), log = TRUE, discrete = FALSE, support_min = -Inf, 
            support_max = Inf, supp_max_depends_on = c(df = FALSE, 
            mu = FALSE, sigma = FALSE), supp_min_depends_on = c(df = FALSE, 
            mu = FALSE, sigma = FALSE))))
