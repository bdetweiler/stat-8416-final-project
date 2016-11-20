# The idea is to place a spinner image in the same container as the plot
# and center it, but give it a below-default z-index
# Whenever the plot is recalculting, make the plot's z-index even lower
# so that the spinner will show

mycss <- "
#plot-container {
  position: relative;
}
#loading-spinner {
  position: absolute;
  left: 50%;
  top: 50%;
  z-index: -1;
  margin-top: -33px;  /* half of the spinner's height */
  margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
  z-index: -2;
"

shinyUI(fluidPage(
 
  headerPanel("Foreign Workers in the US", windowTitle = "Foreign Workers in the US"),
  
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
      # TODO: Get spinner working
      #tags$head(tags$style(HTML(mycss))), 
      #div(id = "plot-container",
          #tags$img(src = "21.gif",
                   #id = "loading-spinner")),
      plotlyOutput(outputId = "choro", height = "365px"),
      plotOutput(outputId = "dist", height = "300px")
      # TODO: Year over year
      # TODO: Stat breakdown
      #, verbatimTextOutput("click")
    )
  ) 
))