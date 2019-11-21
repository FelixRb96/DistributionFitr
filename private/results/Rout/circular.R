list(list(package = "circular", family = "cardioid", family_info = list(
    lower = c(rho = -0.5), upper = c(rho = 0.5), accepts_float = c(rho = TRUE), 
    defaults = c(rho = 0), log = FALSE, discrete = FALSE, support_min = -Inf, 
    support_max = Inf, supp_max_depends_on = c(rho = FALSE), 
    supp_min_depends_on = c(rho = FALSE))), list(package = "circular", 
    family = "mixedvonmises", family_info = list(lower = c(mu1 = -Inf, 
    mu2 = -Inf, kappa1 = 0, kappa2 = 0, prop = 0), upper = c(mu1 = Inf, 
    mu2 = Inf, kappa1 = Inf, kappa2 = Inf, prop = 1), accepts_float = c(mu1 = TRUE, 
    mu2 = TRUE, kappa1 = TRUE, kappa2 = TRUE, prop = TRUE), defaults = c(mu1 = 0.5, 
    mu2 = 0.5, kappa1 = 0.5, kappa2 = 0.5, prop = 0.5), log = FALSE, 
        discrete = FALSE, support_min = -Inf, support_max = Inf, 
        supp_max_depends_on = c(mu1 = FALSE, mu2 = FALSE, kappa1 = FALSE, 
        kappa2 = FALSE, prop = FALSE), supp_min_depends_on = c(mu1 = FALSE, 
        mu2 = FALSE, kappa1 = FALSE, kappa2 = FALSE, prop = FALSE
        ))), list(package = "circular", family = "vonmises", 
    family_info = list(lower = c(mu = -Inf, kappa = 0), upper = c(mu = Inf, 
    kappa = Inf), accepts_float = c(mu = TRUE, kappa = TRUE), 
        defaults = c(mu = 0.5, kappa = 0.5), log = TRUE, discrete = FALSE, 
        support_min = -Inf, support_max = Inf, supp_max_depends_on = c(mu = FALSE, 
        kappa = FALSE), supp_min_depends_on = c(mu = FALSE, kappa = FALSE
        ))))
