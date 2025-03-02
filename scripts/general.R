

mmP <- 30.97376
mmS <- 32.065
mmFe <- 55.845
mmAl <- 26.981
mmCa <- 40.078
mmMn <- 54.938
mmTi <- 47.867
mmMg <- 24.30506
mmsulfate <- 96.06

volume_cylinder <- function(r,l) {
  v <- pi*r^2*l
  return(v)
}

equal_breaks <- function(n = 3, s = 0.05, r = 0, ...){
  function(x){
    # rescaling
    d <- s * diff(range(x)) / (1+2*s)
    seq = seq(min(x)+d, max(x)-d, length=n)
    if(seq[2]-seq[1] < 10^(-r)) seq else signif(seq, digits = 1)}
}