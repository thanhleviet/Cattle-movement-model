library(lattice) # for plotting
library(latticeExtra) # for plotting

n <- 40 # total number of cows
a <- 500 # size of the feeding region in x-direction
b <- 500 # size of the feeding region in y-direction
t_max <- 100 # number of time steps
speed <- 20 # speed of cow movement (from papers it is 5 km/h)
prox <- 20 # max distance at which cow can be infected
prob <- 0.9 # probability of infection when distance is =<prox
distance <- 180 # average distance to the alpha cow
a_prob <- 0.8 # probability of following alpha cow

x <- numeric(0) # vector of x-coordinates
y <- numeric(0) # vector of y-coordinates
SIR <- vector() # vector of infection status
herd <- numeric(0) # identifies herd cow belongs to
alpha_x <- rep(0,n) # position of relevant alpha cow
alpha_y <- rep(0,n) # position of relevant alpha cow

# Initial position of each cow is random, all susceptible
# Herd 1 is in top-left corner, herd 2 is in bottom-right
for(i in 1:n/2)
{
 x[i] <- runif(1, min = a/4, max = a/2)
 y[i] <- runif(1, min = b/2, max = 3/4*b)
 SIR[i] <- "S" # S=0, I=1, R=2
 herd[i] <- 1 # Herd 1
}

for(i in 21:n)
{
  x[i] <- runif(1, min = a/2, max = 3/4*a)
  y[i] <- runif(1, min = b/4, max = b/2)
  SIR[i] <- "S" # S=0, I=1, R=2
  herd[i] <- 2 # Herd 2
}

H <- data.frame(x,y,SIR,herd,alpha_x,alpha_y) # organise initial info in a data frame
H$SIR <- factor(SIR, levels=c("S","I","R")) #set SIR to be a factor

H[2:3,3] <- "I" # introducing 2 infected animals in herd 1

par(ask=TRUE) # press Enter for every next plot,
# this can be amended to produce GIF or other animation/video 

### Let animal 1 and animal 21 be alpha cows ###

for (k in 1: t_max) # main loop that updates H at each time step
{ # update first alpha cow
  angle <- runif(1, min = 0, max = 2*pi) # choose random angle
  x_new <- H[1,1] + cos(angle)*speed
  y_new <- H[1,2] + sin(angle)*speed
  
  while (x_new<0 | x_new>a | y_new<0 | y_new>b)
  { # if travelling in this direction cow will collide with the 
    # fence, cow will not choose this direction, choose another
    # random angle and keep checking that cow stays in feeding region
    angle <- runif(1, min = 0, max = 2*pi)
    x_new <- H[1,1] + cos(angle)*speed
    y_new <- H[1,2] + sin(angle)*speed
  }
  H[1,1] <- x_new # update x 
  H[1,2] <- y_new # and y coordinates
  H[which(H[,4]==1),5] <- H[1,1] # and alpha cow position for 
  H[which(H[,4]==1),6] <- H[1,2] # cows in relevant herd
  
  # update second alpha cow
  angle <- runif(1, min = 0, max = 2*pi) # choose random angle
  x_new <- H[21,1] + cos(angle)*speed
  y_new <- H[21,2] + sin(angle)*speed
  
  while (x_new<0 | x_new>a | y_new<0 | y_new>b)
  { # if travelling in this direction cow will collide with the 
    # fence, cow will not choose this direction, choose another
    # random angle and keep checking that cow stays in feeding region
    angle <- runif(1, min = 0, max = 2*pi)
    x_new <- H[21,1] + cos(angle)*speed
    y_new <- H[21,2] + sin(angle)*speed
  }
  H[21,1] <- x_new # update x 
  H[21,2] <- y_new # and y coordinates
  H[which(H[,4]==2),5] <- H[21,1] # and alpha cow position for 
  H[which(H[,4]==2),6] <- H[21,2] # cows in relevant herd
    
 for(i in c(2:20,22:n)) # loop that updates non-alpha cow position
 {
   angle <- runif(1, min = 0, max = 2*pi) # choose random angle
   x_new <- H[i,1] + cos(angle)*speed # update x 
   y_new <- H[i,2] + sin(angle)*speed # and y coordinates
   if (sqrt((x_new-H[i,5])^2 + (y_new-H[i,6])^2)>distance)
   { # if distance to the alpha cow is > than average
     q <- runif(1, min = 0, max = 1)
     if (q<a_prob) # then with a_probability cow might stay in proximity
     {
       while (sqrt((x_new-H[i,5])^2 + (y_new-H[i,6])^2)>distance)
       { 
         angle <- runif(1, min = 0, max = 2*pi)
         x_new <- H[i,1] + cos(angle)*speed 
         y_new <- H[i,2] + sin(angle)*speed 
       }
     }
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
        if (p<prob) # then cow gets infected with certain probability
        {
          H[j,3] = "I"
        }
      }
    }
  } 
 } # plot location of the cows (susceptible - empty, infected - colored)
susceptible <- xyplot(y~x, groups=herd, data=H[(H$SIR=="S"),], pch=1, xlim=c(0,a), ylim=c(0,b))
infected <- xyplot(y~x, groups=herd, data=H[(H$SIR=="I"),], pch=15, xlim=c(0,a), ylim=c(0,b))
print(susceptible+infected)
}


