## Authors 
## Moritz Kern, mkern@mail.uni-mannheim.de
## Benedikt Geier, bgeier@mail.uni-mannheim.de
## Borui Niklas Zhu, bzhu@mail.uni-mannheim.de
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


### AIM: Filter distributions where density obiviously does not fit
### but returned with good logliks (due to unkown reasons)


fitting_sanity_check <- function(object, data, continuity, sensitivity = 1,
				 timeout = 15) {
   if (!is(object, 'optimParams'))
      stop('Wrong input.')

  lower <- min(data) - 0.2 * (max(data) - min(data))
  upper <- max(data) + 0.2 * (max(data) - min(data))

  # breaks <- if (continuity) sqrt(length(data)) 
  #           else min(nclass.Sturges(data), length(unique(data)) + 1)
  breaks <- if (continuity) sqrt(length(data)) else (min(data) - 1) : max(data)
  h <- suppressWarnings(hist(x = data, xlim = range(lower, upper), freq = FALSE,
                             xlab = 'x', ylab = 'density', breaks = breaks, 
                             include.lowest = FALSE, plot = FALSE))
  
  fun <- get_fun_from_package(type = "d", family = object)
  param_list <- split(object@estimatedValues, names(object@estimatedValues))

  density <- function(x) {
    param_list$x <- x
    y <- do.call(fun, param_list)
    y <- ifelse(is.na(y), 0, y)
    return(y)
  }

  hist_KDE <- function(x) {
    sapply(x, function(t) {
      if (length(which(h$breaks < t)) == 0) return(0) # left of histogram
      break_index <- max(which(h$breaks < t))
      if (break_index == length(h$breaks)) return(0) # right of histogram
      return(h$density[break_index])
    })
  }

  abs_dev <- function(x) {
    return(abs(density(x) - hist_KDE(x)))
  }
  
  if (continuity) {
    hist_check <- sum(diff(h$breaks) * density(h$mids))
    setTimeLimit(cpu = timeout, elapsed = timeout, transient = TRUE)
    int_check <- tryCatch(
      integrate(density, lower = -Inf, upper = Inf),
      error = function(e) {
        message('Sanity Check. Calculating integral of ',
                'the density of family ', object@family,' failed: ', e, '\n')
        return(list(value = Inf))
        }
      )
    gini_check <- tryCatch(
      integrate( abs_dev, lower = -Inf, upper = Inf, subdivisions = 2000),
      error = function(e) {
	message('Sanity Check. Calculating integral of ',
		'|density - histogram| of family ', object@family, ' failed: ',
		e, '\n')
        return(list(value = Inf))
        }
      )
    setTimeLimit(cpu = Inf, elapsed = Inf)
  } else {
    # in the discrete case the values represented in data will be all breaks 
    # apart from the first one
    hist_check <- sum(diff(h$breaks) * density(h$breaks[2:length(h$breaks)]))
    int_check <- list(value = 1)
    gini_check <- list(value = 0) 
  }
  
  good <- (int_check$value > (1 - 0.05 * sensitivity)) & 
                                      (hist_check > (1 - 0.5 * sensitivity)) & 
          (int_check$value < (1 + 0.05 * sensitivity)) & 
                                      (hist_check < (1 + 0.5 * sensitivity))

  return(list(hist_check = hist_check, int_check = int_check$value,
	      gini_check = gini_check$value, good = good))
}
