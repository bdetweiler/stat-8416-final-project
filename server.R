library(shiny)
library(dplyr)

shinyServer(function(input, output) {
  
  output$h1b <- renderPlot({
    hist(visas[which(visas$normalized_wage > input$x_min & visas$normalized_wage < input$x_max), ]$normalized_wage, 
         breaks=500,
         main="H1B Wage Distribution",
         xlab="Wage",
         freq=F)
    dens <- density(visas[which(visas$normalized_wage > input$x_min & visas$normalized_wage < input$x_max), ]$normalized_wage,
                    kernel="optcosine")
    lines(dens, col="blue")
  })
  
  #output$perms <- renderPlot({
    #hist(perm[which(perm$normalized_wage > input$x_min & perm$normalized_wage < input$x_max), ]$normalized_wage, 
         #breaks=500,
         #main="PERM Wage Distribution",
         #xlab="Wage",
         #freq=F)
    #dens <- density(perm[which(perm$normalized_wage > input$x_min & perm$normalized_wage < input$x_max), ]$normalized_wage,
                    #kernel="optcosine")
    #lines(dens, col="blue")
  #})
})