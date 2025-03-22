volume_cylinder <- function(r,l) {
  v <- pi*r^2*l
  return(v)
}

calc_test <- function(s,a,w,r) {
  t <- w*(a/s)*10
  err <- (1- (t/r))*100
  print(paste(round(t,3), round(err,3)))
}

calc_test(58.44,35.45,1.1685,7.25)
calc_test(246.47,96.06,4.1942,16.9     )
volume_cylinder(3,5)

3*90
60*volume_cylinder(3,20)/1000/0.3
mmsulfide <- 32.07
mmsulfate <- 96.06
mmnasulfate <- 142.04
mmP <- 30.97376
mmFe <- 55.845

100*mmP
11/mmsulfide
(10/mmsulfate)*142.04
((14.6826/mmnasulfate)*mmsulfate*1000)

stock <- (14.284/mmnasulfate)*1000*1000

tapw <- c(915, 930, 918)
tapws <- c(1187,1194,1176)
mean(tapw)
mean(tapw) + 0.005*stock/2.005
mean(tapws)
sd(tapws)


# sulfide production in mol/day for a 6cm diametert column
sulfide_production <- function(srr) {
  p <- (srr*pi*(3/100)^2)/(mmsulfate*365)
  return(p)
}



# 15g == 100g; g/cm 

(20*volume_cylinder(3,1)/(1/0.15))/(sulfide_production(100)*1000*mmFe*(1) )

0040/mmFe-14/mmsulfide
4/mmP

# diffiusion in mol/daym2
diffusion <- function(C,l,p,D,f){
  dcdt <- (D/(f*p))*((p*C)/l)
  return(dcdt)
}

diffusion(0.5,0.01,0.9,0.018/365,2.2)

# DET solutions
mmmolyb <- 1235.86
mmSbtart <- 667.87
mmasc <- 176.124

((25/mmasc)/0.25)*0.5*0.4
((1/mmasc)/0.025)*0.5
(52.0/mmmolyb)/4
(1.4/mmSbtart)/4
10.7/2

0.4

(0.1*15)/100
(0.01*5)/100
30/100* 0.1
2.5

# DGT equation
(0.7*0.08)/(21600*1.45*10^-5)
0.2*mmsulfide
