r <- globalfit(rnorm(n = 1000, mean=10, sd=1))
summary(r, count=50, which=13)  
hist(r)
hist(r, which=13)

r <- globalfit(rgamma(n = 1000, shape=3, rate = 4))
summary(r, ic='BIC')
summary(r, ic='AICc')
summary(r, which=1:3)
summary(r, which=2, count=5)
summary(r, which=6, count=5)
hist(r, ic='BIC')

r <- globalfit(rbinom(n = 10000, size=10, prob=0.7))
summary(r)

AIC(r, count=2)
BIC(r)

hist(r)