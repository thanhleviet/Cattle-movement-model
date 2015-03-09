library(lattice) # for plotting
library(latticeExtra) # for plotting

n <- 100 # total number of cows
a <- 500 # size of the feeding region in x-direction
b <- 500 # size of the feeding region in y-direction
t_max <- 100 # number of time steps
speed <- 70 # speed of cow movement (from papers it is 5 km/h)
prox <- 30 # max distance at which cow can be infected
prob <- 0.9 # probability of infection when distance is =<prox



x <- numeric(0) # vector of x-coordinates
y <- numeric(0) # vector of y-coordinates
SIR <- vector() # vector of infection status
herd <- numeric(0) # identifies herd cow belongs to


# Initial position of each cow is random, all susceptible
# Herd 1 is in top-left corner, herd 2 is in bottom-right
for(i in 1:n/2)
{
 x[i] <- runif(1, min = 0, max = a/2)
 y[i] <- runif(1, min = b/2, max = b)
 SIR[i] <- "S" # S=0, I=1, R=2
 herd[i] <- 1 # Herd 1
}

for(i in 51:n)
{
  x[i] <- runif(1, min = a/2, max = a)
  y[i] <- runif(1, min = 0, max = b/2)
  SIR[i] <- "S" # S=0, I=1, R=2
  herd[i] <- 2 # Herd 2
}

H <- data.frame(x,y,SIR,herd) # organise initial info in a data frame
H$SIR <- factor(SIR, levels=c("S","I","R")) #set SIR to be a factor

H[1:2,3] <- "I" # introducing 2 infected animals in herd 1

par(ask=TRUE) # press Enter for every next plot,
# this can be amended to produce GIF or other animation/video 

for (k in 1: t_max) # main loop that updates x at each time step
{
 for(i in 1:n) # loop that updates cow position
{
  angle <- runif(1, min = 0, max = 2*pi) # choose random angle
  x_new <- H[i,1] + cos(angle)*speed
  y_new <- H[i,2] + sin(angle)*speed
  
  while (x_new<0 | x_new>a | y_new<0 | y_new>b)
  { # if travelling in this direction cow will collide with the 
    # fence, cow will not choose this direction, choose another
    # random angle and keep checking that cow stays in feeding region
    angle <- runif(1, min = 0, max = 2*pi)
    x_new <- H[i,1] + cos(angle)*speed
    y_new <- H[i,2] + sin(angle)*speed
  }
   
  H[i,1] <- x_new # update x 
  H[i,2] <- y_new # and y coordinates
  
  if (H[i,3] == "I") # for each infected cow
  {
    for (j in 1:n) # check each susceptible cow 
    {
      if (H[j,3] == "S" & sqrt((H[i,1]-H[j,1])^2 + (H[i,2]-H[j,2])^2)<prox)
      { # and if distance between them is <prox
        p <- runif(1, min = 0, max = 1)
        if (p<prob) # then cow gets infected with probability p
        {
          H[j,3] = "I"
        }
      }
    }
  } 
 } # plot location of the cows (susceptible - empty, infected - colored)
s <- xyplot(y~x, groups=herd, data=H[(H$SIR=="S"),], pch=1, xlim=c(0,a), ylim=c(0,b))
i <- xyplot(y~x, groups=herd, data=H[(H$SIR=="I"),], pch=15, xlim=c(0,a), ylim=c(0,b))
print(s+i)
}




