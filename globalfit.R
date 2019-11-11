source('optimParamsDiscrete.R')
source("getFamilies.R") 

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
  
  families <- getFamilies()
  
  discrete_families <- sapply(families, function(x) x$family_info$discrete)
  discrete_families <- which(discrete_families==TRUE) # Indizes zu diskreten Verteilungen
  
  
  if (is.null(continuity)){
    
    trafo_list <- disc_trafo(data)
    data <- trafo_list$data
    relevant_families <- if(trafo_list$discrete) families[discrete_families] else families[-discrete_families]
    
  } else if (continuity == T){
    
    relevant_families <- families[-discrete_families]
    
  } else if(continuity==FALSE) {
    
    relevant_families <- families[discrete_families]
    
  } else {
    
    stop("The argument 'continuity' has to be either NULL, TRUE or FALSE.")
    
  }
  
  print(sapply(relevant_families, function(x) x$family))
  output_liste <- list()
  
  for (fam in relevant_families){
    cat("Current Family:",  fam$family, "\n")
    liste <- optimParamsDiscrete(data = data,
                        family = fam[c('package', 'family')],
                        family_info = fam$family_info,
                        method = 'MLE', prior = NULL, log = fam$family_info$log,
                        optim_method = 'L-BFGS-B', n_starting_points = 1,
                        debug_error = FALSE, show_optim_progress=FALSE, on_error_use_best_result=TRUE, 
                        max_discrete_steps= 100, plot=FALSE, discrete_fast = TRUE)
    
    output <- list(family = fam$family,
                   package = fam$package,
                   estimatedValues = liste$par,
                   log_lik = liste$value,
                   AIC = liste$AIC,
                   BIC = liste$BIC,
                   AICc = liste$AICc,
                   continuousParams = NA, # hier muss noch was passieren
                   range = NA) # hier muss noch was passieren
    
    class(output_liste) <- "globalfit"
  
    class(output) <- "optimParams"
    output_liste[[length(output_liste) + 1]] <- output
  }
  print(data.frame(matrix(unlist(lapply(output_liste, function(x) x[c("family", "log_lik", "AIC")])), nrow=length(output_liste), byrow=TRUE)))
  return(output_liste)
}



if (sys.nframe() == 0) {
  r <- globalfit(rnorm(n = 1000, mean=10, sd=1))
  summary(r)  
  
  r <- globalfit(rgamma(n = 1000, shape=3, rate = 4))
  summary(r)
  summary(r, which=2)
  summary(r, which=2, count=5)
  summary(r, which=6, count=5)
}

class(r)



##### 

summary.globalfit <- function(x, which=1, count=10) {
  if(is.null(which) | !is.numeric(which))
    stop('which parameter must be positive integer.')
  if(is.null(count) | !is.numeric(count) )
    stop('count parameter must be positive integer.')
  df <- do.call(rbind.data.frame, lapply(x, function(x) {
                            lapply(x[c('family', 'package', 'AIC')], function(x) ifelse(is.null(x), NA, x))
                            }))
  count <- min(nrow(df), count)
  which <- min(which, count)
  df <- df[order(df$AIC)[1:count],]
  selected_fit <- x[[as.numeric(rownames(df)[1])]]
  cat('Best fit: \n', selected_fit$family, 'distribution of', selected_fit$package, 'package. \n \n')
  cat('Estimated parameters: \n')
  print(selected_fit$estimatedValues)
  if(which!=1) {
    selected_fit <- x[[as.numeric(rownames(df)[which])]]
    cat('\n Fitted parameters for ', selected_fit$family, 'distribution of', selected_fit$package, 'package. \n \n')
    print(selected_fit$estimatedValues)
  }
  rownames(df) <- 1:count
  cat('\n Other good fits: \n')
  print(df)
}
