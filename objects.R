setClass(Class = "globalfit",
         slots = c(data = "numeric",
                   continuity = 'logical',
                   method = 'character',
                   fits = 'list'))

setClass(Class = "optimParams",
         slots = c(family = "character",
                                    package = "character",
                                    estimatedValues = "numeric",
                                    log_lik = "numeric",
                                    AIC = "numeric",
                                    BIC = "numeric",
                                    AICc= "numeric",
                                    continuousParams = "logical",
                                    range = 'character')
        )


new()

if (sys.nframe() == 0) {
  new('test123', x = c(1,2,3))
  new('globalfit', data = 1, continuity = T, method = 'MLE', fits = list(1,2,3))
}
