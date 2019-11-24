
## bitte deshalb auf mich zukommen.
### AIM: Filter distributions where density obiviously does not fit
### but returned with good logliks (due to unkown reasons)


fitting_sanity_check <- function(object, data, continuity, plot=F##ALSE
                                 ) {
  if(class(object)!='optimParams')
    ## besser
    ## if (!is(object, 'optimParams'))
    stop('Wrong input.')


  ## der nachfolgende Code ist zu 80% der gleiche wie in output.R
  ## lohnt sich Hilfsfunktion Funktion mit Argumen, dass zwischen x<-h$mids
  ## or supporting_points (z.B. TRUE/FALSE fuer x@continuity und NA fuer
  ## h$mids) unterscheidet?
  lower <- min(data) - 0.2 * (max(data) - min(data))
  upper <- max(data) + 0.2 * (max(data) - min(data))

  ## semantisch falsch. if (continuity)  sqrt(length(data)) else 
  breaks <- ifelse(continuity, sqrt(length(data)), min(nclass.Sturges(data),
                                                       length(unique(data))))
  h <- suppressWarnings(hist(x = data, xlim=range(lower,upper), freq = FALSE,
                             xlab = 'x', ylab = 'density', breaks=breaks,
                             plot=plot))

  x <- h$mids
  y <- h$density ## wo wird y verwendet?
  ## s. Kommentar output.R
  command <- paste0("get_fun_from_package(fam = '", object@family, "', '",
                    object@package, "', 'd')(x, ",
                    paste(names(object@estimatedValues),  object@estimatedValues, sep=" = ", collapse =", "), ')')
  z <- eval(parse(text = command))
  if(plot)
    lines(x,z, col='red')

  # meanquot <- sum(x)/sum(y)
  # good <- meanquot > 0.01 & meanquot<100
  
  meanquot <- sum(diff(h$breaks) * z)
  good <- meanquot > 0.5 & meanquot < 2
  
  return(list(meanquot=meanquot, good=good))
}


#fitting_sanity_check(r@fits[[10]], rnorm(n = 1000, mean=10, sd=10), continuity = T, plot = T)

