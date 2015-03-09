n <- 50 # number of cows in the herd
a <- 100 # size of the feeding region in x-direction
b <- 100 # size of the feeding region in y-direction
t_max <- 100 # number of time steps
speed <- 5 # speed of cow movement (from papers it is 5 km/h, 
# though I am not saying that a,b are in km and t_max is in hours,
# it is all yet to be discussed)
prox <- 3 # max distance at which cow can be infected
prob <- 0.6 # probability of infection when distance is =<prox

# Matrix x contains coordinates of each cow and 
# it's infectious status
x <- matrix(, nrow = n, ncol = 3) 
# Initial position of each cow is random, all susceptible
for(i in 1:n)
{
 x[i,1] <- runif(1, min = 0, max = a)
 x[i,2] <- runif(1, min = 0, max = b)
 x[i,3] <- 0 # S=0, I=1, R=2
}

x[1:2,3] <- 1 # introducing 2 infected animals

par(ask=TRUE) # press Enter for every next plot,
# this can be amended to produce GIF or other animation/video 

for (k in 1: t_max) # main loop that updates x at each time step
{
 for(i in 1:n) # loop that updates cow position
{
  angle <- runif(1, min = 0, max = 2*pi) # choose random angle
  x_new <- x[i,1] + cos(angle)*speed
  y_new <- x[i,2] + sin(angle)*speed
  
  while (x_new<0 | x_new>a | y_new<0 | y_new>b)
  { # if travelling in this direction cow will collide with the 
    # fence, cow will not choose this direction, choose another
    # random angle and keep checking that cow stays in feeding region
    angle <- runif(1, min = 0, max = 2*pi)
    x_new <- x[i,1] + cos(angle)*speed
    y_new <- x[i,2] + sin(angle)*speed
  }
   
  x[i,1] <- x_new # update x 
  x[i,2] <- y_new # and y coordinates
  
  if (x[i,3] == 1) # for each infected cow
  {
    for (j in 1:n) # check each susceptible cow 
    {
      if (x[j,3] == 0 & sqrt((x[i,1]-x[j,1])^2 + (x[i,2]-x[j,2])^2)<prox)
      { # and if distance between them is <prox
        p <- runif(1, min = 0, max = 1)
        if (p<prob) # then cow gets infected with probability p
        {
          x[j,3] = 1
        }
      }
    }
  } 
 } # plot location of the cows (susceptible - green, infected - red)
plot(x[,1], x[,2], type="p", xlab="x", ylab="y", xlim=c(0,a), 
     ylim=c(0,b), col = ifelse(x[,3] == 0,'green','red'), pch=16)
}

