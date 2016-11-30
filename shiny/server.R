shinyServer(function(input, output) {
 
  print("START=========================================================")

    
  # US Map 
  output$choro <- renderPlotly({
    
    print("years in dataset:") 
    print(unique(wages$fy))
   
    print("filtering on year:") 
    print(input$year)
    
    # Input conditionals
    wages.map <- wages %>%
       dplyr::filter(fy == as.numeric(input$year))
    
    print(dim(wages.map))
    print(unique(wages.map$visa_class))
    print(unique(wages.map$fy))
   
    print("filtering on visa classification:") 
    print(input$visaClassification)
    
    wages.map <- wages.map %>%
       dplyr::filter(visa_class == input$visaClassification)
 
    print(dim(wages.map))
    print(wages.map[1,])
    
    if (dim(wages.map)[1] > 0) {
      # give state boundaries a white border
      l <- list(color = toRGB("white"), width = 2)
      # specify some map projection/options
      g <- list(scope = 'usa',
                projection = list(type = 'albers usa'),
                showlakes = TRUE,
                lakecolor = toRGB('white'))
      
      plot_geo(wages.map, locationmode = 'USA-states') %>%
        add_trace(z = ~med_wage, 
                  text = ~hover, 
                  locations = ~employer_state_abb,
                  color = ~med_wage, 
                  colors = 'Blues') %>%
        colorbar(title = "Median wage USD") %>%
        layout(title = 'Median Foreign Worker Wage by State<br />(Hover for breakdown)',
               geo = g)
    } else {
      renderPrint({ "No Data" })
    }

  })
  
  output$dist <- renderPlot({
   
    states <- tolower(state.name) 
    codes <- c( 4,   9,  14,  19,  24,  29,  34,
               39,  44,  49,  54,  59,  64,  69,
               73,  78,  83,  88,  93,  98,  103,
               108, 113, 118, 123, 127, 131, 136,
               141, 146, 151, 156, 161, 165, 170,
               175, 180, 185, 190, 195, 199, 204,
               209, 214, 219, 224, 229, 234, 239,
               243)
    
    d <- event_data("plotly_click")
    
    if (is.null(d)) {
      title <- "Wage distribution for US"
    } else {
      print(d$pointNumber)
      print(d)
      states <- states[which(codes == d$pointNumber)]
      s <- properCase(states)
      #s <- paste(toupper(substring(states, 1, 1)), 
                         #substring(states, 2),
                         #sep = "", 
                         #collapse = "")
      title <- paste0("Wage distribution for ", s)
    }
    
    print(title)
    
    # Input conditionals
    #final.shiny.dist <-final.shiny %>%
                       #dplyr::filter(employer_state %in% states) %>%
                       #dplyr::filter(visa_class %in% PermSelect() |
                                     #visa_class %in% H1BSelect() | 
                                     #visa_class %in% H1B1ChileSelect() |
                                     #visa_class %in% H1B1SingaporeSelect() |
                                     #visa_class %in% E3Select())
   
    
    #ggplot(final.shiny.dist, aes(normalized_wage, color=visa_class, fill=visa_class)) +
      #geom_density(alpha = 0.1) +
       #scale_x_continuous(labels = comma) +
       #scale_y_continuous(labels = comma) +
      #labs(title=title, x="Wage", y="Density")
    print("DONE")
  })
})