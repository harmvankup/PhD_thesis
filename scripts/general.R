

mmP <- 30.97376
mmS <- 32.065
mmFe <- 55.845
mmAl <- 26.981
mmCa <- 40.078
mmMn <- 54.938
mmTi <- 47.867
mmMg <- 24.30506
mmsulfate <- 96.06

get_volume <- function(fraction) {
  case_when(
    fraction  %in% c("H2O", "HCl")   ~ 0.02,
    fraction %in% c("BD") ~ 0.0400,
    fraction == "NaOH"  ~ 0.0303,
    fraction == "Bipy" ~ 0.04,
    TRUE                ~ 0
  )
}

get_mm <- function(parameter) {
  case_when(
    parameter %in% c("SRP","TP", "NRP","NE")  ~ mmP,
    parameter %in% c("Fe") ~ mmFe,
    parameter == "S"  ~ mmS,
    parameter == "Mn" ~ mmMn,
    parameter == "Al" ~ mmAl,
    parameter == "Ti" ~ mmTi,
    parameter == "Ca" ~ mmCa,
    parameter == "Mg" ~ mmMg,
    TRUE                ~ 0
  )
}

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