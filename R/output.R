## Authors 
## Moritz Kern, mkern@mail.uni-mannheim.de
##
## S4-Methods for objects
##
## Copyright (C) 2019 -- 2020 Moritz Kern
##
## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License
## as published by the Free Software Foundation; either version 3
## of the License, or (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. 

## sind die is.null() wirklich notwendig?? (i) sind defaults gesetzte
## (ii) wirft R eh Fehler

## braucht ihr wirklich (ueberall) die Abfrage is.natural?
## warum reicht nicht if (x != as.integer(x)) ?

"%@%" <- function(x, ic) eval(parse(text = paste0('x@', ic)))
  
setMethod(f = "sort", signature = c('globalfit'),
          def = function(x, decreasing = FALSE, ic='AIC') {
            if(is.null(ic) || !(ic %in% c('AIC', 'BIC', 'AICc')))
              stop("Argument 'ic' must be 'AIC', 'BIC' or 'AICc'")
            ic <- sapply(x@fits, function(x) x %@% ic) 
            x@fits <- x@fits[order(ic)]
            return(x)
          })



setMethod(f = "summary", signature = c("globalfit"),
          def = function(object, count=10, ic = 'AIC') {
            if(is.null(ic) || !(ic %in% c('AIC', 'BIC', 'AICc')))
              stop("Argument 'ic' must be 'AIC', 'BIC' or 'AICc'")
            if(is.null(count) || !is.natural(count) )
              stop("Argument 'count'  must be positive integer.")
            
	    if(length(object@fits) < 1) {
	      message("No family fitted. Either packages provided in argument
              'packages' do not supply reasonable parametric distributions or
	      some fatal error occured.\nTo troubleshoot try:
	      (1) changing argument input for 'packages'
	      (2) adjusting rigorosity of 'sanity'
	      (3) adjusting 'timeout'")
	      return(invisible())
	    }

            object <- sort(object, ic=ic)
            df <- data.frame(family = sapply(object@fits,
                                             function(f) f@family),
                           package = sapply(object@fits, function(f) f@package),
                             ic = sapply(object@fits, function(f) f %@% ic),
##                                  eval(parse(text = paste0('object@', ic)))),
                             params = sapply(object@fits, function(f) 
                                      paste(names(f@estimatedValues),
                                            signif(f@estimatedValues,
                                                   digits = 3),
                                            sep= " = ", collapse='; ')))
            colnames(df) <- c("family", "package", ic, "params")
            
            sum <- new("globalfitSummary",
                       data = object@data,
                       continuity = object@continuity,
                       method = object@method,
                       fits = df[1:min(count, nrow(df)),],
                       ic = ic
                )
            return(sum)
          })

setMethod(f = "show", signature = c("globalfitSummary"),
          def = function(object) {
            if(is.null(object@continuity)) {
              cont <- ''
            } else if(object@continuity) {
              cont <-
              '\nAssumption: Data was generated from a continuous distribution.'
            } else if(!object@continuity) {
              cont <- 
                '\nAssumption: Data was generated from a discrete distribution.'
            }
            cat(length(object@data), 
                'data points entered. Distributions were fitted via', 
                object@method, 'estimation.',
                cont, '
                \nBest fits sorted by', object@ic, ':\n\n')
            print(object@fits, right=FALSE)
          }
)

setMethod(f = "print", signature = c("globalfitSummary"),
          def = function(x) {
            show(x)
          }
          )

setMethod(f = "print", signature = c("globalfit"),
	  def = function(x) {
	    print(summary(x))
	  }
	  )


IC <- function(object, ic='AIC', count = NULL) {
  if(is.null(count))
    count <- Inf
  if(!is.natural(count))
    stop("Argument 'count'  must be positive integer.")
  object <- sort(object, ic=ic)
  count <- min(length(object@fits), count)
  object@fits <- object@fits[1:count]
  x <- sapply(object@fits, function(object) object %@% ic)
  ##eval(parse(text = paste0('object@', ic))))
  names(x) <- paste(sapply(object@fits, function(object) object@package), 
                    sapply(object@fits, function(object) object@family),
                    sep = "::")
  return(x)            
}



setMethod(f = "AIC", signature = c("globalfit"),
          def = function(object, count=Inf, k = 2) {
            if(is.null(k) || k!=2)
              stop("Not implemented. Argument 'k' must be set to 2.  ")
            IC(object, ic='AIC')
          })


setMethod(f = "BIC", signature = c("globalfit"),
          def = function(object, count=Inf) {
            IC(object, ic='BIC')
          })


setMethod(f = "hist", signature = c("globalfit"),
          def = function(x, which = 1, ic='AIC') {
            if(is.null(ic) || !(ic %in% c('AIC', 'BIC', 'AICc')))
              stop("Argument 'ic' must be 'AIC', 'BIC' or 'AICc'")
            if(is.null(which) || !is.natural(which))
              stop("Argument 'which'  must be positive integer.")
            
            x <- sort(x, ic=ic)
            if (which > length(x@fits)) stop(
              "value of 'which' larger than the number of available results")
             
            lower <- min(x@data) - 0.2 * (max(x@data)-min(x@data))
            upper <- max(x@data) + 0.2 * (max(x@data)-min(x@data))

            selected_fit <- x@fits[[which]]
            if(x@continuity) {
              supporting_point <-seq(lower, upper, length.out = 300)
            } else {
              supporting_point <- seq(floor(lower)+0.5, ceiling(upper)+0.5)
            }
            breaks <- if (x@continuity) sqrt(length(x@data)) else 
                      min(nclass.Sturges(x@data), length(unique(x@data)))
            
            fun <- get_fun_from_package(type="d", family = selected_fit)
            param_list <- split(selected_fit@estimatedValues, 
                                names(selected_fit@estimatedValues))
            param_list$x <- supporting_point
            density <- do.call(fun, param_list)
            
            h <- hist(x = x@data, xlim=range(lower,upper), freq = FALSE,
                 xlab = 'x', ylab = 'density', breaks=breaks,
                 main=paste0('Histogramm with density of \n',
                             selected_fit@package, '::', selected_fit@family))
            lines(supporting_point, density, col='green', lwd=2)
            
            h$estimation_points <- supporting_point
            h$estimated_density <- density
            invisible(h)
          }
        )
