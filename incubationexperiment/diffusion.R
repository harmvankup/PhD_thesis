# Set the length and time parameters for the simulation
length <- 100 # length of the porous medium in millimeters
time <- 1 # time of the simulation in days

# Set the diffusion coefficient and the porosity of the medium
D <- 60*1.24*10^-9 # diffusion coefficient in m^2/min
porosity <- 0.9 # porosity of the medium
a <- 0.8
m <- 2.1
ffactor <- a*porosity^(-m) # Formation factor

# Set the initial and boundary conditions
concentration <- rep(0, length) # initial concentration of the substance is 0 at all points
concentration[1] <- 1 # concentration of the substance at the left boundary is 1

# Set the time step and the number of iterations
dt <- 1 # time step in minutes
iterations <- 24*60*time/dt # number of iterations

# Set the reaction rate at the left boundary
k <- 0.1 # reaction rate in mol/min^-1

# Initialize the time and concentration arrays
times <- c() # time array
concentrations <- list() # concentration array

# Loop through the iterations and update the concentration at each point
for (i in 1:iterations){
  times <- c(times, (i-1)*(dt/60)) # add the current time to the time array
  concentrations[[i]] <- concentration # add the current concentration to the concentration array
  concentration[1] <- concentration[1] + dt*k + D*dt*(concentration[2] - concentration[1])/((1/1000)^2*ffactor*porosity) # update concentration at the left boundary
  for (j in 2:(length-1)){
    concentration[j] <- concentration[j] + D*dt*(concentration[j+1] - 2*concentration[j] + concentration[j-1])/((1/1000)^2*ffactor*porosity)
  }
}

# Plot the concentration profiles at different time steps in the same figure
plot(concentrations[[1]], type="l", xlab="Position (mm)", ylab="Concentration", col="red", main="Concentration Profile")
legend_text <- c() # initialize the legend text array
for (i in seq(from=1, to=length(times), by=((iterations)/10))){
  lines(concentrations[[i]], col=rgb(0,0,i/length(times)))
  legend_text <- c(legend_text, paste(times[i], "h")) # add the time label to the legend text array
}

# Add the legend
legend("topright", legend=legend_text, col=rgb(0,0,seq(from=1, to=length(times), by=(iterations)/10)/length(times)), lty=1)

testlength = seq(0.01,10,by = 0.01)
#test <-  testlength + k*dt*testlength + 
test <-   D*dt*(0 - testlength)/((1/1000)^2*ffactor)

plot(test, testlength, type="l")
