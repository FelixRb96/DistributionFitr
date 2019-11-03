
rm(list=ls())


# Loglik-Function
# Formate

# arg_opt:  Charakter Vektor,   Bezeichnung der Parameter von denen die Funktion abhängen soll
# family:   Charakter Vektor,   Name der Familie
# data:     Numerischer Vektor, Datenreihe
# fixed:    banamte Liste,      Namen und fixierte Werte für die übringen Parameter
#                               Arg_opt und fixed sind disjunkt und ergeben zusammen ALLE paramter der Familie
# data:     Numerischer Vektor, Datenreihe
# log:      Boolean (T/F),      Does the log argument exist when calling the distribuition function of the family
# lower:    Numerischer Vektor, Lower Boundaries for paramters in arg_opt   (same order!)
# upper:    Numerischer Vektor, Upper Boundaries for paramters in arg_opt   (same order!)


loglik <- function(arg_opt, family, data, fixed=list(), log, lower, upper) { #NEW: fixed=list() default
  
  if(length(arg_opt)==0) warning('loglik does not depend on parameters.')
  
  
  arguments <- list(x=data) #NEW: arg_opt wird erst in likelihood in die Liste eingefügt
  
  # check wheter log-distribution function is directly available for distribution
  if(log==T)
    arguments$log <- T
  # add fixed parameter values of distribution to list
  if(length(fixed)>0) {
    arguments <- c(arguments, fixed)
  }
  
  
  # define loglikelihood function 
  likelihood <- function(params) {
    
    #NEW: Check boundaries
    if(any(params-lower<0) | any(upper-params<0))
      stop('Parameters are outside the boundaries.') #Alternativ hier einen Penalty einbauen
    
    #NEW: length arg_opt = length params
    if(length(params)!=length(arg_opt))
      stop('Incorrect number of arguments')
    
    #Add params with names of parameters to arguments list
    for(i in 1:length(params)){
      arguments[[arg_opt[i]]] <- params[i]
    }
    
    summands <- do.call(paste('d', family, sep=''), args=arguments)
    if(any(is.na(summands)))
      stop('In Log-Likelihood-Function NA occured.')
    
    # log values if not log so far
        if(!log) {
      summands <- log(summands)
      warning('Could be numerically instable.')
    } 
    loglik_value <- sum(summands)
    return(loglik_value)
  }
  return(likelihood)
}


#Testbeispiel

arg_opt <- c("mean")
family <- "norm"
data <- rnorm(10, mean = 7, sd=2)
fixed <- list(sd=2)
log = TRUE
lower <- 2
upper <- 10

out <- loglik(arg_opt = arg_opt, family = family, data = data, fixed = fixed,
              log = log, lower = lower, upper = upper)

test <- NULL
for(i in 1:9) {
  test[i] <- out(i+1)
}
plot(2:10,test, xlab = "mean", ylab = "likelihood")
out(7)