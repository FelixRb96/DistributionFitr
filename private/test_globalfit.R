r <- globalfit(rnorm(n = 1000, mean=10, sd=1), packages = "stats", verbose = TRUE)

summary(r, count=10)
hist(r)
for(i in 1:20) {
  tryCatch(hist(r, which=i), error=function(x) return(NA))
  Sys.sleep(1)
}

r <- globalfit(rgamma(n = 1000, shape=3, rate = 4))
summary(r, ic='BIC')
summary(r, ic='AICc')
summary(r, ic='BIC', count=7)
hist(r, ic='BIC')
hist(r, ic='BIC', which=7)
hist(r, ic='BIC', which=18)
hist(r, ic='BIC', which=4)
hist(r, ic='BIC', which=30)

r <- globalfit(rbinom(n = 1000, size=10, prob=0.7))
summary(r)

AIC(r, count=2)
BIC(r)

hist(r)



# examples, tests, 
x <- seq(6,20, 0.01)
y <- seq(0,10,0.1)
plot(x, CoSMoS::dggamma(x=x, scale=3.59, shape1=19.09, shape2=5.46))
plot(x, CoSMoS::dggamma(x=x, scale=4, shape1=20, shape2=5.46))
plot(y, CoSMoS::dggamma(x=y, scale=1, shape1=1, shape2=1))


x <- 0:10 
plot(ForestFit::dgsm(x, omega = 5.491870e+08, beta = 6.732193e-02 ))

plot(ForestFit::dgsm(x, omega = 1, beta = 1 ))





fitting_sanity_check(r@fits[[1]], rnorm(n = 1000, mean=10, sd=1))
