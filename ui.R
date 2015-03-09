library(shiny)
library(lattice) # for plotting
library(latticeExtra) # for plotting

shinyUI(fluidPage(
  titlePanel(h4("Model for strongly immunizing diseases transmitted by direct contact"),
             h6("Susceptible animals are mapped with circles, infected with colored squares and recovered with triangles")),
  
  sidebarLayout(
    sidebarPanel( 
      numericInput("n", 
                   label = h6("Number of cows in one herd"), 
                   value = 30),
      
      numericInput("a", 
                   label = h6("Field size (x-axis)"), 
                   value = 500),
      
      numericInput("b", 
                   label = h6("Field size (y-axis)"), 
                   value = 500),
      
      numericInput("speed", 
                   label = h6("Cow speed"), 
                   value = 40),
      
      numericInput("distance", 
                   label = h6("Comfortable distance from the herd"), 
                   value = 120),
      
      numericInput("prox", 
                   label = h6("Max transmission distance"), 
                   value = 30),

      numericInput("infection_length", 
                   label = h6("Infection length (in time steps)"), 
                   value = 5),
      
      numericInput("t_max", 
                   label = h6("Number of time steps"), 
                   value = 5),
      
      
      sliderInput("prob", label = h6("Probability of being infected"),
                  min = 0, max = 1, value = 0.8, step = 0.05),
      
      sliderInput("a_prob", label = h6("Probability of following your herd"),
                  min = 0, max = 1, value = 0.8, step = 0.05),
      
      submitButton("Submit")
      ),

    
    mainPanel(
      plotOutput(outputId = "plot1",  width = "100%")
    )

  )
))




