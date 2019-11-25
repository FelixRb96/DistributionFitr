## Authors 
## Moritz Lauff, mlauff@mail.uni-mannheim.de
## Kiril Dik, kdik@mail.uni-mannheim.de
## Moritz Kern, mkern@mail.uni-mannheim.de
## Nadine Tampe, ntampe@mail.uni-mannheim.de
##
## Fit multiple distribution families to a given univariate dataset
##
## Copyright (C) 2019 -- 2020 Moritz Lauff, Kiril Dik, Moritz Kern, Nadine Tampe
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





### 1)  benoetigte Hilfsfunktionen -------------------------------------------------------------------------

# Rundungsproblem mit .5 beheben
new_round <- function(x) {
  
  as.integer(x + 0.5)
  
}

# Dezimalstellen bestimmen
getDecimals <- function(x){
  
  return(round(x %% 1, 10))
  
}

#' some_percent: wenn fuer eine gegebene Anzahl an Dezimalstellen mindestens der Ante?l percent
#'               an moeglichen Dezimalen (von 10) auftreten, wird TRUE zurueckgegeben 
some_percent <- function(df, percent){
  for (i in min(df$numbers):max(df$numbers)){
    if (length(unique(df$decimals[df$numbers == i])) >= percent * 10){
      return(TRUE) ## TRUE
    }
  }
  
  return(FALSE) ## dito
}

# Testen, ob Daten diskret sind
is.discrete <- function(data, border = 0.35, percent = 0.8){
  
  # # Umwandeln zu vector
  if(is.data.frame(data)){
    data <- as.vector(data[,1])
  }
  
  # Entfernen der NA's
  data <- data[!is.na(data)]
  
  obs <- length(data)
  decs <- getDecimals(data)
  numbers <- nchar(as.character(decs)) - 2
  numbers[numbers == -1] <- 1
  n_unique_dec <- length(unique(decs))
  
  if (1 / border > obs){
    border <- 1 / obs
  }

  ## bitte nichts zu einem data.frame zusammenfassen, was
  ## intern einfach anders handhabbar ist
  percent_df <- data.frame(decimals = decs,
                           numbers = numbers)
  
  ## das sollte man alles (oder weitgehend) mit &&, || zu 1 Return
  ## zusammenfassen koennen
  if (n_unique_dec / obs <= border){
    if (any(numbers >= 4)){
      return(FALSE)
    } else {
       if (some_percent(percent_df, percent)){
        return(FALSE) 
      } else {
        return(TRUE)
      }
    }
  } else {
    return(FALSE)
  }
}

# Behandlung diskreter nicht-ganzzahliger Daten
disc_trafo <- function(data){
  
  if (is.discrete(data)){ # Trafo nur, wenn Daten diskret


    data_new <- sort(data) # Daten sortieren
    ## Keine data.frames! Die sind i.W. nur fuer user
    data_new <- data.frame(data_new = data_new,
                           decimals = getDecimals(data_new)) # Daten mit zugehoerigen Dezimalen
    
    unique_decimals <- sort(unique(data_new$decimals)) # auftretende verschiedene Dezimalen
    m <- length(unique_decimals) # Anzahl auftretender verschiedener Dezimalen
    
    decimals <- data.frame(original_decimals = unique_decimals,
                           new_decimals = round(seq(0, (m-1)/m, 1/m), 10)) # Dezimalen und transformierte Dezimalen
    
    data_new <- merge(data_new, decimals,  
                      by.x = "decimals", by.y = "original_decimals") # zusammenfuegen
    
    data_new <- (data_new$data_new - data_new$decimals + data_new$new_decimals) * m # neue Daten
    
    return(list(data = data_new,
                trafo_df = decimals,
                discrete = TRUE,
                trafo_decription = paste0("Divide the simulated data by ",
                                          m,
                                          " and replace the decimals c(", 
                                          paste(decimals$original_decimals, collapse=", "),
                                          ") of the simulated data by c(",
                                          paste(decimals$new_decimals, collapse=", "),
                                          ").")))
    
  } else {
    return(list(data = data,
                trafo_df = NULL,
                discrete = FALSE,
                trafo_decription = NULL))
  }
  
}



### 2) Main Function --------------------------------------------------------------------------

globalfit <- function(data, continuity = NULL, method = "MLE", progress = TRUE, cores = NULL, ...){

  families <- getFamilies()

  discrete_families <- sapply(families, function(x) x$family_info$discrete)
  discrete_families <- which(discrete_families) # Indizes zu diskreten Verteilungen
  
  if (is.null(continuity)){
    
    trafo_list <- disc_trafo(data)
    data <- trafo_list$data
    relevant_families <- if(trafo_list$discrete) families[discrete_families] else families[-discrete_families]
    continuity <- ifelse(trafo_list$discrete, FALSE, TRUE)
    
  } else if (continuity ){
    
    relevant_families <- families[-discrete_families]
    
  } else if(!continuity) { ## gibt es noch andere moeglichkeiten ?
    
    relevant_families <- families[discrete_families]
    
  } else { ## sicher dass das else jemals erreiht wird ?
    
    stop("The argument 'continuity' has to be either NULL, TRUE or FALSE.")
    
  }
  
  ## bitte obige Kommentare durcharbeiten, dann ersetzen durch
  ## relevant_families <- families[if (continuity) -discrete_families else discrete_families]
  
  
  # TODO: How do we handle not yet installed packages? Force install or warn and ignore?
  
  all_pkgs <- sapply(relevant_families, function(x) x$package)
  all_pkgs_unique <- unique(all_pkgs)
  missing_pkgs <- setdiff(all_pkgs_unique, rownames(installed.packages()))
  if (length(missing_pkgs) > 0) {
    message("The following packages are not installed, and are thus ignored during optimisation. ",
            "If you want to use them please install manually:", paste(missing_pkgs, collapse=", "))
    relevant_families <- relevant_families[!(all_pkgs %in% missing_pkgs)]
  }
  
  if(progress)
      message("Comparing the following distribution families:", paste(sapply(relevant_families, function(x) x$family), collapse = ", "))

  if(is.null(cores))
    cores <- detectCores()
  cl <- makeCluster(cores, outfile='log.txt')
  registerDoParallel(cl)
  
  output_liste <- foreach(i=1:length(relevant_families), .packages = c(), .errorhandling = 'remove') %dopar% {
  #for (fam in relevant_families) {
    source('private/source_all.R') ## keine Konstanten im Code
    fam <- relevant_families[[i]]
    if(progress)
      message("Current Family: ",  fam$family)
    
    output_liste <- optimParamsDiscrete(data = data,
                        family = fam[c('package', 'family')],
                        family_info = fam$family_info,
                        method = 'MLE', prior = NULL, log = fam$family_info$log,
                        optim_method = 'L-BFGS-B', n_starting_points = 1,
                        debug_error = FALSE, show_optim_progress=FALSE, on_error_use_best_result=TRUE, 
                        max_discrete_steps=100, plot=FALSE, discrete_fast = TRUE)
    if(!is.null(output_liste) && !is.na(output_liste$value) && !is.infinite(output_liste$value)) {
      output <- new('optimParams', family = fam$family,
                   package = fam$package,
                   estimatedValues = output_liste$par,
                   log_lik = output_liste$value,
                   AIC = output_liste$AIC,
                   BIC = output_liste$BIC,
                   AICc = output_liste$AICc,
                   continuousParams = NA, # hier muss noch was passieren
                   range = 'not_implemented') # hier muss noch was passieren
      # aim: check whether solution has good loglik but does not fit
      # experimental feature - please watch out!
      sanity_check <- fitting_sanity_check(output, data, continuity = continuity)
    } else {
      sanity_check <- list(good=FALSE, meanquot=NA)## FALSE
    }
    if(!sanity_check$good)
      output <- new('optimParams', 
                    family = fam$family,
                    package = fam$package,
                    log_lik = NA_integer_,
                    AIC = NA_integer_,
                    BIC = NA_integer_,
                    AICc = NA_integer_,
                    continuousParams = NA, # hier muss noch was passieren
                    range = 'not_implemented') # hier muss noch was passieren
    return(output)
  }
  stopCluster(cl)
  
  return(new('globalfit', data = data, 
             continuity = continuity,
             method = method,
             fits = output_liste))
}

