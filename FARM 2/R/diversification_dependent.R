# Diversification dependent
DivDep <- function(mytree, myWorld) {
  myWorld <- myWorld[!is.na(myWorld[, 6]), ]
  traits <- setNames(myWorld[, 6], myWorld[, 8])
  musse <- make.musse(mytree, traits, 2)
  p <- try(starting.point.musse(mytree, k = 2), silent = TRUE)
  if (class(p) == "try-error") {
    fit.musse <- try(find.mle(musse, x.init = c(rep(.1, 4), rep(0.01, 2))),
                     silent = TRUE)
  } else {
    fit.musse <- find.mle(musse, x.init = p[argnames(musse)])
  }
  if(any(class(fit.musse) == "try-error")) {
    return(rep(NA, 6))
  } else{
    return(fit.musse$par)
  }
}
