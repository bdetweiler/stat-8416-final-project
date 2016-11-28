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
      # Copy the line below to make a select box 
      selectInput("year", label = "Year", 
                  choices = list("2016" = 16,
                                 "2015" = 15,
                                 "2014" = 14,
                                 "2013" = 13,
                                 "2012" = 12,
                                 "2011" = 11, 
                                 "2010" = 10,
                                 "2009" = 9,
                                 "2008" = 8),
                  selected = 16),
      hr(),
      #sliderInput(inputId = "x_range", 
                  #label = "Wage range:",
                  #min = 0, max = 250000, value = c(0, 250000)),
      #hr(),
      selectInput("visaClassification", label = "Visa Classification", 
                  choices = list("PERM" = "E",
                                 "H-1B1 Chile" = "B",
                                 "H-1B1 Singapore" = "C",
                                 "E-3 Australia" = "D",
                                 "H-1B" = "A"),
                  selected = "E")
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