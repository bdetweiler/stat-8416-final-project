shinyServer(function(input, output) {
 
    PermSelect <- reactive({ 
      if(input$show_PERM){
        print("PERM!")
        "PERM"
      }
      else { NULL }  
    })
    
    H1BSelect <- reactive({ 
      if(input$show_H1B){
        "H-1B"
      }
      else { NULL }  
    })
    
    H1B1SingaporeSelect <- reactive({ 
      if(input$show_H1B1_Singapore){
        "H-1B1 Singapore"
      }
      else { NULL }  
    })
    
    H1B1ChileSelect <- reactive({ 
      if(input$show_H1B1_Chile){
        "H-1B1 Chile"
      }
      else { NULL }  
    })
    
    E3Select <- reactive({ 
      if(input$show_E3){
        "E-3 Australian"
      }
      else { NULL }  
    })
    
    print("START=========================================================")
    
    # Select just the columns we want 
    final.shiny.plot <- select(final.shiny, fy,
                                           visa_class,
                                           normalized_wage,
                                           normalized_prevailing_wage,
                                           employer_state) %>%
                       dplyr::filter(!is.na(visa_class))  %>%
                       dplyr::filter(!is.na(employer_state)) %>%
                       dplyr::filter(!is.na(normalized_wage)) #%>%
                        
    print(final.shiny.plot[1:10, ])
    print(unique(final.shiny.plot$visa_class))
    
  # US Map 
  output$choro <- renderPlotly({
    
    # Input conditionals
    final.shiny.map <- final.shiny.plot %>%
                       dplyr::filter(normalized_wage > input$x_range[1]) %>%
                       dplyr::filter(normalized_wage < input$x_range[2]) %>%
                       dplyr::filter(visa_class %in% PermSelect() |
                                     visa_class %in% H1BSelect() | 
                                     visa_class %in% H1B1ChileSelect() |
                                     visa_class %in% H1B1SingaporeSelect() |
                                     visa_class %in% E3Select())
   
    # Final aggregation 
    final.shiny.map <- final.shiny.plot %>% 
                       group_by(employer_state, visa_class) %>%           #  We'll also allow grouping by fy and visa_class
                       summarise(med = median(normalized_wage), 
                                 mean = mean(normalized_wage), 
                                 min = min(normalized_wage),
                                 max = max(normalized_wage))
    
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
  
  output$dist <- renderPlotly({
    
    # Input conditionals
    final.shiny.dist <-final.shiny.plot %>%
                       dplyr::filter(normalized_wage > input$x_range[1]) %>%
                       dplyr::filter(normalized_wage < input$x_range[2]) %>%
                       dplyr::filter(visa_class %in% PermSelect() |
                                     visa_class %in% H1BSelect() | 
                                     visa_class %in% H1B1ChileSelect() |
                                     visa_class %in% H1B1SingaporeSelect() |
                                     visa_class %in% E3Select())
   
    plot_ly(alpha = 0.6) %>%
      add_histogram(x = ~final.shiny.dist$normalized_wage) %>%
      add_histogram(x = ~final.shiny.dist$normalized_prevailing_wage) %>%
      layout(barmode = "overlay")
  })
})