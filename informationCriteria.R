# Information criteria calculation

informationCriteria <- function(ll, k, n) {
  aic = 2*k - 2 *  ll
  bic = log(n) * k - 2 *  ll
  aicc = aic + (2*k^2+2*k)/(n-k-1)
  return(list(aic,bic,aicc))
}
