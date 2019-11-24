#setGeneric(name = "summary",		
#           def = function(object, ...) standardGeneric("summary"))		


## sind die is.null() wirklich notwendig?? (i) sind defaults gesetzte
## (ii) wirft R eh Fehler

## braucht ihr wirklich (ueberall) die Abfrage is.natural?
## warum reicht nicht if (x != as.integer(x)) ?

setMethod(f = "summary", signature = c("globalfit"),
          def = function(object, which=1, count=10, ic = 'AIC') {
            if(is.null(ic) || !(ic %in% c('AIC', 'BIC', 'AICc')))
              stop("Argument 'ic' must be 'AIC', 'BIC' or 'AICc'")
            if(is.null(which) || !is.natural(which))
              stop("Argument 'which' must vector of positive integers.")
            if(is.null(count) || !is.natural(count) )
              stop("Argument 'count'  must be positive integer.")
            
            ## MS:  besser:
            ## familiy <- sapply(object@fits, function(object) object@family)
            ## iclist <- ...
            ## df <- data.frame(family=family, ...)
            df <- do.call(rbind, lapply(object@fits, function(object) {
              return(data.frame(family =object@family,
                                package = object@package,
                                ic = eval(parse(text = paste0('object@', ic))),
                                stringsAsFactors = F))
            }))
            # somehow the rownames are messed up and need to be fiobjected
            count <- min(nrow(df), count)
            which <- pmin(which, count) ## which[which <= count] ??
            df <- df[order(df[,'ic'])[1:count],] ##  df[iclist ...
            colnames(df)[3] <- ic
            if(length(which)==1 && (which[1]==1)) {
              selected_fit <- object@fits[[as.numeric(rownames(df)[1])]]
              cat('Best fit: \n', selected_fit@family, 'distribution of', selected_fit@package, 'package. \n \n')
              cat('Estimated parameters: \n')
              print(selected_fit@estimatedValues)
            } else {
              for(i in 1:length(which)) {
                selected_fit <- object@fits[[as.numeric(rownames(df)[which[i]])]]
                ## besser vielleicht nur die for schleife
                ## und hier die if (length(which)==1 && (which[1]==1)) else
                cat('\nFitted parameters for', selected_fit@family, 'distribution of', selected_fit@package, 'package. \n')
                print(selected_fit@estimatedValues)
              }
            }
            rownames(df) <- 1:nrow(df)
            cat('\nOther good fits: \n')
            print(df)
          })



## nie Code doppeln. AIC und BIC sind der gleiche Code bis auf
## 'AIC' und $AIC
## Also: neue Funktion schreiben mit zusaetzlicher Variablen, die den
## Wert "AIC" udn "BIC" annimmt

setMethod(f = "AIC", signature = c("globalfit"),
          def = function(object, ..., k = 2, ## count = Inf
                         ) {
            ls <- list(...) ## loeschen ?!
            if(is.null(k) || k!=2)
              stop("Not implemented. Argument 'k' must be set to 2.  ")
            ## falls obiges uebernommen, dann naechste Zeilen loeschen
            ## ansonsten: count <- if(is.null(ls$count)) Inf else ls$count
            if(is.null(ls$count))
              count <- Inf
            else 
              count <- ls$count
            if(!is.natural(count))
              stop("Argument 'count'  must be positive integer.")
            df <- do.call(rbind, lapply(object@fits, function(x) {
              return(data.frame(family =x@family, package = x@package,
                                AIC = x@AIC, stringsAsFactors = F))
            }))
            count <- min(nrow(df), count)
            df <- df[order(df[,'AIC'])[1:count],]
            x <- df$AIC
            names(x) <- paste(df$package, df$family, sep = "::")
            return(x)
          })


setMethod(f = "BIC", signature = c("globalfit"),
          def = function(object, ..., ## count = Inf 
                         ) {
            ## ff: dito
            ls <- list(...)
            if(is.null(ls$count))
              count <- Inf
            else 
              count <- ls$count
            if(!is.natural(count))
              stop("Argument 'count'  must be positive integer.")
            df <- do.call(rbind, lapply(object@fits, function(x) {
              return(data.frame(family =x@family, package = x@package, BIC = x@BIC,
                                stringsAsFactors = F))
            }))
            count <- min(nrow(df), count)
            df <- df[order(df[,'BIC'])[1:count],]
            x <- df$BIC
            names(x) <- paste(df$package, df$family, sep = "::")
            return(x)
          })


setMethod(f = "hist", signature = c("globalfit"),
          def = function(x, ic='AIC', which=1, ...) {
            if(is.null(ic) || !(ic %in% c('AIC', 'BIC', 'AICc')))
              stop("Argument 'ic' must be 'AIC', 'BIC' or 'AICc'")
            if(is.null(which) || !is.natural(which))
              stop("Argument 'which'  must be positive integer.")
            
            lower <- min(x@data) - 0.2 * (max(x@data)-min(x@data))
            upper <- max(x@data) + 0.2 * (max(x@data)-min(x@data))
            
            df <- do.call(rbind, lapply(x@fits, function(x) {
              return(data.frame(family =x@family, package = x@package, ic = eval(parse(text = paste0('x@', ic))),
                                stringsAsFactors = F))
            }))
            rownames(df) <- 1:nrow(df)
            which <- min(which, nrow(df))
            # print(which)
            
            df <- df[order(df[,'ic']),]
            selected_fit <- x@fits[[as.numeric(rownames(df)[which])]]
            selected_fit@estimatedValues
            if(x@continuity) {
              supporting_point <-seq(lower, upper, length.out = 300)
            } else {
              supporting_point <- seq(floor(lower)+0.5, ceiling(upper)+0.5)
            }
            ## ifelse ist hier semantisch falsch 
            ## breaks <- if (x@continuity)  sqrt(length(x@data)) else ...
            breaks <- ifelse(x@continuity, sqrt(length(x@data)), min(nclass.Sturges(x@data), length(unique(x@data))))
            ## geht nachfolgendes nicht einfacher ueber direkte Aufrufe
            ## und/oder do.call ?
            density <- eval(parse(text = paste0("get_fun_from_package(fam = '", selected_fit@family, "', '", selected_fit@package, "', 'd')(supporting_point, ",
                                     paste(names(selected_fit@estimatedValues),  selected_fit@estimatedValues, sep=" = ", collapse =", "), ')')))

            # print(density)
            #plot(supporting_point, density, col='green', lwd=2)
            #Sys.sleep(2)
            hist(x = x@data, xlim=range(lower,upper), freq = FALSE,
                 xlab = 'x', ylab = 'density', breaks=breaks,
                 main=paste0('Histogramm with density of \n',
                             selected_fit@package, '::', selected_fit@family))
            lines(supporting_point, density, col='green', lwd=2)
            return(invisible(density))
          }
        )

