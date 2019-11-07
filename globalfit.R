source('optimParamsDiscrete.R')
source("get_families.R") 

globalfit <- function(data, continuity = NULL, method = "MLE"){
  
  # method als Nutzereingabe -> ist auch bei optimParam schon dabei
  
# benoetigte Hilfsfunktionen ----
  
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
      return(T)
    }
  }
  
  return(F)
}

# Testen, ob Daten diskret sind
is.discrete <- function(data, border = 0.35, percent = 0.8){
  
  # # Umwandeln zu vector
  if(is.data.frame(data)){
    data <- as.vector(data = data[,1])
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
  
  percent_df <- data.frame(decimals = decs,
                           numbers = numbers)
  
  
  if (n_unique_dec / obs <= border){
    if (any(numbers >= 4)){
      return(F)
    } else {
      if (some_percent(percent_df, percent)){
        return(F)
      } else {
        return(T)
      }
    }
  } else {
    return(F)
  }
}

# Behandlung diskreter nicht-ganzzahliger Daten
disc_trafo <- function(data){
  
  if (is.discrete(data)){ # Trafo nur, wenn Daten diskret
    
    data_new <- sort(data) # Daten sortieren
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
                discrete = T,
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
                discrete = F,
                trafo_decription = NULL))
  }
  
}
  
# main ----
  
  families <- get_families()
  
  fam_inds <- 1:length(families)
  
  loops <- array(NA, dim=length(families))
  for(i in 1:length(families)){
    
    loops[i] <- families[[i]]$family_info$discrete
    
  }
  loops <- which(loops==TRUE) # Indizes zu diskreten Verteilungen
  
  
  if (is.null(continuity)){
    
    trafo_list <- disc_trafo(data)
    data <- trafo_list$data
    
  } else if (continuity == T){
    
    loops <- fam_inds[-loops]
    
  } else {
    
    stop("The argument 'continuity' has to be either NULL, TRUE or FALSE.")
    
  }
  
  # output_liste <- replicate(loops, list())
  
  for (fam in loops){
    
      liste <- optimParamsDiscrete(data = data,
                          family = families[[fam]][c('package', 'family')],
                          family_info = families[[fam]]$family_info,
                          method = 'MLE', prior = NULL, log = families[[fam]]$family_info$log,
                          optim_method = 'L-BFGS-B', n_starting_points = 1,
                          debug_error = FALSE, show_optim_progress=FALSE, on_error_use_best_result=TRUE, 
                          max_discrete_steps= 100, plot=FALSE, discrete_fast = TRUE)
      
      output <- list(likfit = liste$value,
                     AIC = liste$AIC,
                     BIC = liste$BIC,
                     AICc = liste$AICc,
                     familyName = families[[fam]]$family,
                     packageName = families[[fam]]$package,
                     estimatedValues = liste$par,
                     continuousParams = NULL, # hier muss noch was passieren
                     range = NULL) # hier muss noch was passieren
      
    
    class(output) <- "globalfit"
    output_liste[fam] <- output
  }
  
}

globalfit(rnorm(n = 1000, mean=10, sd=1))
