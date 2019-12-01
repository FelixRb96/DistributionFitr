## Authors 
## Moritz Kern, mkern@mail.uni-mannheim.de
##
## Filter distributions where density obiviously does not fit
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



# AIM: Filter distributions where density obiviously does not fit
# but returned with good logliks (due to unkown reasons)

## bitte deshalb auf mich zukommen.
### AIM: Filter distributions where density obiviously does not fit
### but returned with good logliks (due to unkown reasons)


fitting_sanity_check <- function(object, data, continuity, plot=FALSE, sensitivity = 1) {
   if (!is(object, 'optimParams'))
      stop('Wrong input.')

  ## der nachfolgende Code ist zu 80% der gleiche wie in output.R
  ## lohnt sich Hilfsfunktion Funktion mit Argumen, dass zwischen x<-h$mids
  ## or supporting_points (z.B. TRUE/FALSE fuer x@continuity und NA fuer
  ## h$mids) unterscheidet?
  lower <- min(data) - 0.2 * (max(data) - min(data))
  upper <- max(data) + 0.2 * (max(data) - min(data))

  ## semantisch falsch. if (continuity)  sqrt(length(data)) else 
  # breaks <- if (continuity) sqrt(length(data)) else min(nclass.Sturges(data), length(unique(data)) + 1)
  breaks <- if (continuity) sqrt(length(data)) else (min(data) -1) : max(data)
  h <- suppressWarnings(hist(x = data, xlim=range(lower,upper), freq = FALSE,
                             xlab = 'x', ylab = 'density', breaks=breaks, include.lowest = FALSE,
                             plot=plot))
  
  fun <- get_fun_from_package(type="d", family = object)
  param_list <- split(object@estimatedValues, names(object@estimatedValues))
  #return(list(fun, param_list))
  density <- function(x) {
    param_list$x <- x
    y <- do.call(fun, param_list)
    y <- ifelse(is.na(y), 0, y)
    return(y)
  }
  
  if (continuity) {
    int_check <- tryCatch(integrate(density, lower = -Inf, upper = Inf),
                  error = function(e) {
                            message('Sanity Check. Calculate integral of density failed: ',e,'\n')
                            return(list(value=Inf))
                  }
        )
    hist_check <- sum(diff(h$breaks) * density(h$mids))
  } else {
    # in the discrete case the values represented in data will be all breaks apart from the first one
    hist_check <- sum(diff(h$breaks) * density(h$breaks[2:length(h$breaks)]))
    
    ## BG: Do we need an int check in discrete case???
    int_check <- list(value=1)
  }

  
  good <- (int_check$value > (1 - 0.05 * sensitivity)) & (hist_check > (1-0.5*sensitivity)) & 
          (int_check$value < (1 + 0.05 * sensitivity)) & (hist_check < (1+0.5*sensitivity))

  return(list(hist_check=hist_check, int_check=int_check$value, good=good))
}
