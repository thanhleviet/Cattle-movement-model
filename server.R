shinyServer(function(input, output) {

plot_it<-function(n,a,b,speed,distance,prox,infection_length,t_max,prob,a_prob){
  m <- 2 # number of herds 
  l <- m*n # number of rows in data set H
  x1 <- numeric(0) # vector of x-coordinates
  y1 <- numeric(0) # vector of y-coordinates
  SIR1 <- vector() # vector of infection status
  herd1 <- numeric(0) # identifies herd cow belongs to
  x2 <- numeric(0) # vector of x-coordinates
  y2 <- numeric(0) # vector of y-coordinates
  SIR2 <- vector() # vector of infection status
  herd2 <- numeric(0) # identifies herd cow belongs to
  x_median <- rep(0,l) # median x-coordinates of your herd
  y_median <- rep(0,l) # median y-coordinates of your herd
  infection <- rep(0,l) # infected for how many time steps
  
  # Initial position of each cow is random, all susceptible
  # Herd 1 is in top-left corner, herd 2 is in bottom-right
  for(i in 1:n)
  {
    x1[i] <- runif(1, min = a/4, max = a/2)
    y1[i] <- runif(1, min = b/2, max = 3/4*b)
    SIR1[i] <- "S" # S=0, I=1, R=2
    herd1[i] <- 1 # Herd 1
  }
  
  for(i in 1:n)
  {
    x2[i] <- runif(1, min = a/2, max = 3/4*a)
    y2[i] <- runif(1, min = b/4, max = b/2)
    SIR2[i] <- "S" # S=0, I=1, R=2
    herd2[i] <- 2 # Herd 2
  }
  
  x<-c(x1,x2)
  y<-c(y1,y2)
  SIR<-c(SIR1,SIR2)
  herd<-c(herd1,herd2)
  
  H <- data.frame(x,y,SIR,herd,x_median,y_median,infection) # organise initial info in a data frame
  H$SIR <- factor(SIR, levels=c("S","I","R")) #set SIR to be a factor
  H$herd <- factor(herd) #set SIR to be a factor
  
  H[1:2,3] <- "I" # introducing 2 infected animals in herd 1

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
  }
    susceptible <- xyplot(y~x, groups=herd, data=H[(H$SIR=="S"),], pch=1, xlim=c(0,a), ylim=c(0,b), aspect="iso")
    infected <- xyplot(y~x, groups=herd, data=H[(H$SIR=="I"),], pch=15, xlim=c(0,a), ylim=c(0,b), aspect="iso")
    recovered <- xyplot(y~x, groups=herd, data=H[(H$SIR=="R"),], pch=2, xlim=c(0,a), ylim=c(0,b), aspect="iso")
    final_plot <- susceptible+infected+recovered
  return(final_plot)
}  
  
  # the function that should diplay the plot

  output$plot1 <- renderPlot({
    plot_it(input$n,input$a,input$b,input$speed,input$distance,input$prox,input$infection_length,input$t_max,input$prob,input$a_prob)
    })
  
}
)
