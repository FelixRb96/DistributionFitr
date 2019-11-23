# AIM: Filter distributions where density obiviously does not fit
# but returned with good logliks (due to unkown reasons)

fitting_sanity_check <- function(object, data, continuity, plot=F) {
  if(class(object)!='optimParams')
    stop('Wrong input.')
  
  lower <- min(data) - 0.2 * (max(data)-min(data))
  upper <- max(data) + 0.2 * (max(data)-min(data))
  
  breaks <- ifelse(continuity, sqrt(length(data)), min(nclass.Sturges(x@data), length(unique(x@data))))
  h <- suppressWarnings(hist(x = data, xlim=range(lower,upper), freq = FALSE, xlab = 'x', ylab = 'density', breaks=breaks, plot=plot))
  
  x <- h$mids
  y <- h$density
  command <- paste0("get_fun_from_package(fam = '", object@family, "', '", object@package, "', 'd')(x, ",
                    paste(names(object@estimatedValues),  object@estimatedValues, sep=" = ", collapse =", "), ')')
  z <- eval(parse(text = command))
  if(plot)
    lines(x,z, col='red')
  meanquot <- sum(x)/sum(y)
  
  good <- meanquot > 0.01 & meanquot<100
  return(list(meanquot=meanquot, good=good))
}


# fitting_sanity_check(r@fits[[30]], rnorm(n = 1000, mean=10, sd=1), continuity = T, plot = T)
