#### FORMAL TEST FOR GLOBALFIT ######


set.seed(0)
n <- 10000
previous <- 'private/test_case.rds'

if((file.exists(previous))) 
  compare <- readRDS(previous)

# Large distribution test
# TODO: fails for runif
rand <- list(rnorm(n = n, mean=10, sd=1),
             rbinom(n = n, size=20, prob=0.1),
             rgamma(n = n, shape=3, rate=1),
             rpois(n = n, lambda=5),
             rweibull(n = n, shape=7, scale=2),
             runif(n = n, min = 5, max = 20))
    
r <- lapply(rand, globalfit, stats_only = F)
s <- lapply(r, summary)

if(!(file.exists(previous))) {
  saveRDS(object=s, file=previous)
} else {
  if(identical(compare,s)) print("Sucessfull") else print("Failed")
}