shinyUI(bootstrapPage(
  
  #options(scipen=999),
  
  sliderInput(inputId = "x_min",
              label = "Adjustment min wage:",
              min = 0,
              max = 500000, 
              value = 0,
              step = 25000),
  
  sliderInput(inputId = "x_max",
              label = "Adjustment max wage:",
              min = 25000,
              max = 500000, 
              value = 200000,
              step = 25000),
  
  plotOutput(outputId = "h1b", height = "300px")
  
  #plotOutput(outputId = "perms", height = "300px")
))