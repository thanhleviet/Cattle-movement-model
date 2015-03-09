############################
# Cows move in herds, get infected, recover, a GIF of it is produced 
# Daily cycle of activities
############################

library(lattice) # for plotting
library(latticeExtra) # for plotting
library(animation) # for generating gif

#### Initial set up #####
field_length <- 100 # size of the feeding region in x-direction
field_width <- 100 # size of the feeding region in y-direction
herd_spacing <- 10 # min space between two herds in x or y direction
herd_num <- 5 # number of herds
cow_num <- 20 # number of cows in each herd
total_num <- herd_num*cow_num # total number of cows
simulation_days <- 10 # number of days to be simulated
mean_day_speed <- 10 # mean day speed of cows (from papers it is 5 km/h)
sd_day_speed <- 3 # standard deviation of day speed of cows
mean_night_speed <- 5 # mean night speed of cows
sd_night_speed <- 1 # standard deviation of night speed of cows
prox <- 10 # max distance at which cow can be infected
inf_prob <- 0.8 # probability of infection when distance is <prox
radius <- 10 # the radius of a herd (at night) and initial position 
herd_prob <- 0.6 # probability of following your herd
infection_length <- 50 # how many hours cow stays infected before it recovers
day <- 16 # Number of daytime hours
night <- 24-day # Number of nighttime hours
angle_change <- (2*pi)/day

#### Randomly select the locations of herds ####
x_sample <- seq(0, field_length, herd_spacing)
y_sample <- seq(0, field_width, herd_spacing)
x_coord <- sample(x_sample, size=herd_num, replace = TRUE) 
y_coord <- sample(y_sample, size=herd_num, replace = TRUE)
## NOTE that two or more herds could have the same location

#### Creat data frame with animal information ####
x <- numeric(total_num) # vector of x-coordinates
y <- numeric(total_num) # vector of y-coordinates
SIR <- rep("S",total_num) # vector of infection status
herd <- rep(1:herd_num, times=1, each=cow_num) # herds
x_median <- numeric(total_num) # median x-coordinates of herds
y_median <- numeric(total_num) # median y-coordinates of herds
infection <- numeric(total_num) # cow infected for how many time steps
clockwise <- numeric(total_num) # 1, or 0 if anticlockwise
herd_angle <- numeric(total_num) # angle at which the herd is moving

H <- data.frame(x,y,SIR,herd,x_median,y_median,
                infection,clockwise,herd_angle) 
H$SIR <- factor(SIR, levels=c("S","I","R")) #set SIR to be a factor
H$herd <- factor(herd) #set herd to be a factor

# input herd locations
for (i in 1:herd_num){
    H$x_median[which(H$herd==i)] <- x_coord[i]
    H$y_median[which(H$herd==i)] <- y_coord[i]
}

# Initial position of each cow is random around the herd center
for(i in 1:total_num)
{
 distance <- runif(1, min = 0, max = radius)
 angle <- runif(1, min = -pi, max = pi)
 H$x[i] <- H$x_median[i] + (distance * cos(angle))
 H$y[i] <- H$y_median[i] + (distance * sin(angle))
}

#### Introducing infected animals ####
H$SIR[1:2] <- "I"

#### THE MAIN LOOP ####

# png(file="infection%03d.png", width=300, height=300)
 par(ask=TRUE)

for (i in 1:simulation_days){ # a loop for each day
  for (j in 1:herd_num){ # select random direction for each herd
    H$herd_angle[which(H$herd==j)] <- runif(1, min = -pi, max = pi)
    H$clockwise[which(H$herd==j)] <- sample(c(0,1),1)
  }
  for (j in 1:day){ # a subloop for each daytime hour
    for(k in 1:herd_num){ # for each herd update median
      if(H$clockwise[which(H$herd==k)[1]]==1){
        angle <- H$herd_angle[which(H$herd==k)[1]] 
        + angle_change + runif(1, min = -pi/30, max = pi/30)  
      }else{
        angle <- H$herd_angle[which(H$herd==k)[1]] 
        - angle_change + runif(1, min = -pi/30, max = pi/30)  
      }
      speed <- rnorm(1, mean = mean_day_speed, sd = sd_day_speed)
      H$x_median[which(H$herd==k)] <- H$x_median[which(H$herd==k)[1]]
      + (speed * cos(angle))
      H$y_median[which(H$herd==k)] <- H$y_median[which(H$herd==k)[1]]
      + (speed * sin(angle))
      H$herd_angle[which(H$herd==k)] <- angle    
    }
    for(k in 1:total_num){ # for each cow update location
      if (runif(1, min = 0, max = 1) < herd_prob){
        ax <- H$x_median[k] - H$x[k]
        ay <- H$y_median[k] - H$y[k]
        angle <- atan2(ay,ax) + (pi/30 * runif(1, min=-1, max=1))
        speed <- rnorm(1, mean = mean_day_speed, sd = sd_day_speed)
        H$x[k] <- H$x[k] + (speed * cos(angle))
        H$y[k] <- H$y[k] + (speed * sin(angle))       
      } else {
        angle <- runif(1, min=-pi, max=pi)
        speed <- rnorm(1, mean = mean_day_speed, sd = sd_day_speed)
        H$x[k] <- H$x[k] + (speed * cos(angle))
        H$y[k] <- H$y[k] + (speed * sin(angle)) 
      }
    }
    for(k in 1:total_num){ # for each infected cow check if infection transmits
      if(H$SIR[k]=="I"){
        for(l in 1:total_num){
          distance <- sqrt((H$x[l]-H$x[k])^2+(H$y[l]-H$y[k])^2)
          if(H$SIR[l]=="S" & distance<prox & runif(1, min = 0, max = 1)<inf_prob){
            H$SIR[l] <- "I"
          }
        } 
      }
    }
    # update the infection time
    H$infection[which(H$SIR=="I")] <- H$infection[which(H$SIR=="I")] + 1
    # recover cows
    H$SIR[which(H$infection==infection_length)] <- "R"
    # produce a plot (S-empty circles, I-colored circles, R-empty triangles)
    susceptible <- xyplot(y~x, groups=herd, data=H[which(H$SIR=="S"),], 
                          pch=1, xlim=c(0,field_length), ylim=c(0,field_width))
    infected <- xyplot(y~x, groups=herd, data=H[which(H$SIR=="I"),], 
                       pch=2, xlim=c(0,field_length), ylim=c(0,field_width))
    recovered <- xyplot(y~x, groups=herd, data=H[which(H$SIR=="R"),], 
                        pch=3, xlim=c(0,field_length), ylim=c(0,field_width))
    # ani.record(reset = FALSE, replay.cur = FALSE)
    print(susceptible+infected+recovered)
    # print(summary(H$SIR))
  }
  for (j in 1:night){ # a subloop for each nighttime hour
    for(k in 1:herd_num){ # for each herd update median
      H$x_median[which(H$herd==k)] <- x_coord[k]
      H$y_median[which(H$herd==k)] <- y_coord[k] 
    }
    for(k in 1:total_num){ # for each cow update location
      if (runif(1, min = 0, max = 1) < herd_prob){
        ax <- H$x_median[k] - H$x[k]
        ay <- H$y_median[k] - H$y[k]
        angle <- atan2(ay,ax) + (pi/30 * runif(1, min=-1, max=1))   
      } else {
        angle <- runif(1, min=-pi, max=pi)
      }
      speed <- rnorm(1, mean = mean_night_speed, sd = sd_night_speed)
      H$x[k] <- H$x[k] + (speed * cos(angle))
      H$y[k] <- H$y[k] + (speed * sin(angle))
    }
    for(k in 1:total_num){ # for each infected cow check if infection transmits
      if(H$SIR[k]=="I"){
        for(l in 1:total_num){
          distance <- sqrt((H$x[l]-H$x[k])^2+(H$y[l]-H$y[k])^2)
          if(H$SIR[l]=="S" & distance<prox & runif(1, min = 0, max = 1)<inf_prob){
            H$SIR[l] <- "I"
          }
        } 
      }
    }
    # update the infection time
    H$infection[which(H$SIR=="I")] <- H$infection[which(H$SIR=="I")] + 1
    # recover cows
    H$SIR[which(H$infection==infection_length)] <- "R"
    # produce a plot (S-empty circles, I-colored circles, R-empty triangles)
    susceptible <- xyplot(y~x, groups=herd, data=H[which(H$SIR=="S"),], 
                          pch=1, xlim=c(0,field_length), ylim=c(0,field_width))
    infected <- xyplot(y~x, groups=herd, data=H[which(H$SIR=="I"),], 
                       pch=1, xlim=c(0,field_length), ylim=c(0,field_width))
    recovered <- xyplot(y~x, groups=herd, data=H[which(H$SIR=="R"),], 
                        pch=1, xlim=c(0,field_length), ylim=c(0,field_width))
    # ani.record(reset = FALSE, replay.cur = FALSE)
    print(susceptible+infected+recovered)
    # print(summary(H$SIR))
  }
}


# #### Make a gif ####
# #dev.off()
# ani.options(convert = 'C:/Program Files/ImageMagick-6.8.9-Q16/convert.exe')
# # convert pngs to one gif using ImageMagick
# im.convert('*.png', output = 'infection.gif', convert = "convert",
#            cmd.fun = if (.Platform$OS.type == "windows") 
#              shell else system, clean = TRUE)
# # cleaning up
# file.remove(list.files(pattern=".png"))
