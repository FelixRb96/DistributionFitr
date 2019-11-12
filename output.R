summary.globalfit <- function(x, which=1, count=10, ic = c('AIC')) {
  if(is.null(ic) || !ic %in% c('AIC', 'BIC', 'AICc'))
    stop("Argument 'ic' must be 'AIC', 'BIC' or 'AICc'")
  if(is.null(which) || !is.natural(which))
    stop("Argument 'which' must vector of positive integers.")
  if(is.null(count) || !is.natural(count) )
    stop("Argument 'count'  must be positive integer.")
  
  cols <- c('family', 'package', ic)
  df <- do.call(rbind.data.frame, lapply(x, function(x) {
    lapply(x[cols], function(x) ifelse(is.null(x), NA, x))
  }))
  # somehow the rownames are messed up and need to be fixed
  rownames(df) <- 1:nrow(df)
  count <- min(nrow(df), count)
  which <- pmin(which, count)
  df <- df[order(df[,ic])[1:count],]
  if(length(which)==1 && (which[1]==1)) {
    selected_fit <- x[[as.numeric(rownames(df)[1])]]
    cat('Best fit: \n', selected_fit$family, 'distribution of', selected_fit$package, 'package. \n \n')
    cat('Estimated parameters: \n')
    print(selected_fit$estimatedValues)
  } else {
    for(i in 1:length(which)) {
      selected_fit <- x[[as.numeric(rownames(df)[which[i]])]]
      cat('\nFitted parameters for', selected_fit$family, 'distribution of', selected_fit$package, 'package. \n')
      print(selected_fit$estimatedValues)
    }
  }
  rownames(df) <- 1:count
  cat('\nOther good fits: \n')
  print(df)
}

setGeneric(name = "summary",
           def = function(x, ...) standardGeneric("summary"))


setMethod(f = "summary", signature = c("globalfit"),
          def = function(x, which=1, count=10, ic = c('AIC')) {
              if(is.null(ic) || !ic %in% c('AIC', 'BIC', 'AICc'))
                stop("Argument 'ic' must be 'AIC', 'BIC' or 'AICc'")
            if(is.null(which) || !is.natural(which))
              stop("Argument 'which' must vector of positive integers.")
            if(is.null(count) || !is.natural(count) )
              stop("Argument 'count'  must be positive integer.")
            
            cols <- c('family', 'package', ic)
            df <- do.call(rbind, lapply(x@fits, function(x) {
              return(data.frame(family =x@family, package = x@package, ic = eval(parse(text = paste0('x@', ic))),
                                stringsAsFactors = F))
            }))
            # somehow the rownames are messed up and need to be fixed
            count <- min(nrow(df), count)
            which <- pmin(which, count)
            df <- df[order(df[,'ic'])[1:count],]
            colnames(df)[3] <- ic
            if(length(which)==1 && (which[1]==1)) {
              selected_fit <- x@fits[[as.numeric(rownames(df)[1])]]
              cat('Best fit: \n', selected_fit@family, 'distribution of', selected_fit@package, 'package. \n \n')
              cat('Estimated parameters: \n')
              print(selected_fit@estimatedValues)
            } else {
              for(i in 1:length(which)) {
                selected_fit <- x@fits[[as.numeric(rownames(df)[which[i]])]]
                cat('\nFitted parameters for', selected_fit@family, 'distribution of', selected_fit@package, 'package. \n')
                print(selected_fit@estimatedValues)
              }
            }
            cat('\nOther good fits: \n')
            print(df)
          })


summary(r)
