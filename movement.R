############################
# Cows move in herds, get infected, recover, a GIF of it is produced 
############################

library(lattice) # for plotting
library(latticeExtra) # for plotting
library(animation)

n <- 40 # total number of cows
m <- 2 # number of herds
a <- 500 # size of the feeding region in x-direction
b <- 500 # size of the feeding region in y-direction
t_max <- 100 # number of time steps
speed <- 40 # speed of cow movement (from papers it is 5 km/h)
prox <- 30 # max distance at which cow can be infected
prob <- 0.8 # probability of infection when distance is <prox
distance <- 120 # comfortable distance from your herd
a_prob <- 0.8 # probability of following your herd
infection_length <- 5 # for how long cow stays infected before it recovers

x <- numeric(0) # vector of x-coordinates
y <- numeric(0) # vector of y-coordinates
SIR <- vector() # vector of infection status
herd <- numeric(0) # identifies herd cow belongs to
x_median <- rep(0,n) # median x-coordinates of your herd
y_median <- rep(0,n) # median y-coordinates of your herd
infection <- rep(0,n) # infected for how many time steps

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

H <- data.frame(x,y,SIR,herd,x_median,y_median,infection) # organise initial info in a data frame
H$SIR <- factor(SIR, levels=c("S","I","R")) #set SIR to be a factor
H$herd <- factor(herd) #set SIR to be a factor

H[1:2,3] <- "I" # introducing 2 infected animals in herd 1

par(ask=FALSE)
# par(ask=TRUE) # press Enter for every next plot,
# this can be amended to produce GIF or other animation/video 

png(file="infection%03d.png", width=300, height=300)

for (i in 1: t_max) # main loop that updates H at each time step
{
  for (j in 1:m) # for each herd find the median coordinates
  {
   H[which(H[,4]==j),5] <- median(H[which(H[,4]==j),1])
   H[which(H[,4]==j),6] <- median(H[which(H[,4]==j),2])
  }
  for(k in 1:n) # update the location of each cow
  {
   q <- runif(1, min = 0, max = 1)
   if (sqrt((H[k,1]-H[k,5])^2+(H[k,2]-H[k,6])^2)>distance & q<a_prob)
   { # if a cow is too far, with prob. q it will come back
     ax <- H[k,5] - H[k,1]
     ay <- H[k,6] - H[k,2]
     epsilon <- pi/12 * runif(1, min=-1, max=1)
     angle <- atan2(ay,ax) + epsilon
     H[k,1] <- H[k,1] + cos(angle)*speed
     H[k,2] <- H[k,2] + sin(angle)*speed
   } else
   {
     angle <- runif(1, min = -pi, max = pi) # choose random angle
     x_new <- H[k,1] + cos(angle)*speed
     y_new <- H[k,2] + sin(angle)*speed
     while (x_new<0 | x_new>a | y_new<0 | y_new>b)
     { # if travelling in this direction cow will collide with the 
       # fence, cow will not choose this direction, choose another
       # random angle and keep checking that cow stays in feeding region
       angle <- runif(1, min = -pi, max = pi)
       x_new <- H[k,1] + cos(angle)*speed
       y_new <- H[k,2] + sin(angle)*speed
     }
     H[k,1] <- x_new # update x 
     H[k,2] <- y_new # and y coordinates
   }
  }
  for(l in 1:n) {
    if (H[l,3] == "I") # for each infected cow
    {
      for (m in 1:n) { # check each susceptible cow 
        p <- runif(1, min = 0, max = 1)
        if (H[m,3]=="S" & sqrt((H[m,1]-H[l,1])^2+(H[m,2]-H[l,2])^2)<prox & p<prob) {
          H[m,3] <- "I" # and if distance between them is <prox
        } # then cow gets infected with probability p
      } 
    } 
  } 
  H[which(H[,3]=="I"),7] <- H[which(H[,3]=="I"),7] + 1 # also update the infection time
  H[which(H[,7]==infection_length),3] <- "R" # and recover cows
  # plot location of the cows (susceptible - empty, infected - colored)
  susceptible <- xyplot(y~x, groups=herd, data=H[(H$SIR=="S"),], pch=1, xlim=c(0,a), ylim=c(0,b))
  infected <- xyplot(y~x, groups=herd, data=H[(H$SIR=="I"),], pch=15, xlim=c(0,a), ylim=c(0,b))
  recovered <- xyplot(y~x, groups=herd, data=H[(H$SIR=="R"),], pch=2, xlim=c(0,a), ylim=c(0,b))
  ani.record(reset = FALSE, replay.cur = FALSE)

  print(susceptible+infected+recovered)
  # print(summary(H[,3]))
}    

#dev.off()

ani.options(convert = 'C:/Program Files/ImageMagick-6.8.9-Q16/convert.exe')
# convert pngs to one gif using ImageMagick
im.convert('*.png', output = 'infection.gif', convert = "convert",
           cmd.fun = if (.Platform$OS.type == "windows") shell else system, clean = TRUE)

# cleaning up
file.remove(list.files(pattern=".png"))
