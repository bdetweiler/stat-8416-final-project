shinyUI(fluidPage(
  
  titlePanel("Filters"), 
  
  sidebarLayout(position="left",
    # Specification of range within an interval
    sidebarPanel(
      sliderInput(inputId = "x_range", 
                  label = "Wage range:",
                  min = 0, max = 500000, value = c(0, 250000)),
      helpText("Visa Classification"),
      checkboxInput(inputId = "show_PERM", "PERM", value = TRUE, width = NULL),
      checkboxInput(inputId = "show_H1B", "H-1B", value = TRUE, width = NULL),
      checkboxInput(inputId = "show_H1B1_Singapore", "H-1B1 Singapore", value = TRUE, width = NULL),
      checkboxInput(inputId = "show_H1B1_Chile", "H-1B1 Chile", value = TRUE, width = NULL),
      checkboxInput(inputId = "show_E3", "E-3 Australia", value = TRUE, width = NULL)
    ),
   
    mainPanel(
      plotlyOutput(outputId = "choro", height = "365px")#,
      #plotOutput(outputId = "dist", height = "300px")
    )
  ) 
))