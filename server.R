library(shiny)
library(ggplot2)
library(choroplethr)
library(choroplethrMaps)
library(dplyr)
library(plotly)


shinyServer(function(input, output) {
  
  #output$h1b <- renderPlot({
    #hist(visas[which(visas$normalized_wage > input$x_min & visas$normalized_wage < input$x_max), ]$normalized_wage, 
         #breaks=500,
         #main="H1B Wage Distribution",
         #xlab="Wage",
         #freq=F)
    #dens <- density(visas[which(visas$normalized_wage > input$x_min & visas$normalized_wage < input$x_max), ]$normalized_wage,
                    #kernel="optcosine")
    #lines(dens, col="blue")
  #})
  
  output$choro <- renderPlotly({

    visa_classifications <- c()
    
    if (input$show_PERM) {
      print("PERM!")
      visa_classifications <- c(visa_classifications, 'PERM')
    }
    if (input$show_H1B) {
      print("H1b!")
      visa_classifications <- c(visa_classifications, 'H-1B')
    }
    if (input$show_H1B1_Singapore) {
      print("H1b Singapore!")
      visa_classifications <- c(visa_classifications, 'H-1B1 Singapore')
    }
    if (input$show_H1B1_Chile) {
      print("H1b Chile!")
      visa_classifications <- c(visa_classifications, 'H-1B1 Chile')
    }
    if (input$show_E3) {
      print("E3!")
      visa_classifications <- c(visa_classifications, 'E-3 Australia')
    }
    print(visa_classifications)
    
    final.shiny.map <- select(final.shiny, fy,
                                           visa_class,
                                           normalized_wage,
                                           normalized_prevailing_wage,
                                           employer_state) %>%
                       filter(!is.na(employer_state)) %>%
                       filter(!is.na(normalized_wage)) %>%
                       filter(!is.na(visa_class)) %>%
                       #filter(visa_class %in% visa_classifications) %>%
                       filter(normalized_wage > input$x_range[1]) %>%
                       filter(normalized_wage < input$x_range[2]) %>%
                       group_by(employer_state) %>%           #  We'll also allow grouping by fy and visa_class
                       summarise(med = median(normalized_wage), 
                                 mean = mean(normalized_wage), 
                                 min = min(normalized_wage),
                                 max = max(normalized_wage))
    
    print(final.shiny.map[1:10, ])

    final.shiny.map$employer_state_abb <- final.shiny.map$employer_state
    for (state in state.name) {
      final.shiny.map$employer_state_abb[which(tolower(final.shiny.map$employer_state) == tolower(state))]  <- 
        state.abb[tolower(state.name) %in% tolower(state)]
    }

    
    final.shiny.map$hover <- with(final.shiny.map, paste(employer_state, "<br>",
                                                         "Mean wage", nfd(mean), "<br>", 
                                                         "Median wage", nfd(med), "<br>",
                                                         "Minimum wage", nfd(min), "<br>",
                                                         "Maximum wage", nfd(max)))
    
    # give state boundaries a white border
    l <- list(color = toRGB("white"), width = 2)
    # specify some map projection/options
    g <- list(scope = 'usa',
              projection = list(type = 'albers usa'),
              showlakes = TRUE,
              lakecolor = toRGB('white'))
    
    plot_geo(final.shiny.map, locationmode = 'USA-states') %>%
      add_trace(z = ~med, 
                text = ~hover, 
                locations = ~employer_state_abb,
                color = ~med, 
                colors = 'Purples') %>%
      colorbar(title = "Median wage USD") %>%
      layout(title = 'Foreign Worker Wages by State<br>(Hover for breakdown)',
             geo = g)

  })
})