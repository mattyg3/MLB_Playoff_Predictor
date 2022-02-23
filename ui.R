
library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("MLB Playoff Predictor"),
  
  fluidRow(
      column(3, 
             sliderInput("OBP",
                         h4("Projected Team OBP:", align = "center"),
                         min = 0,
                         max = 1,
                         value = 0.317, round=FALSE, step = 0.001)),
      column(3, 
             sliderInput("ERA",
                         h4("Projected Team ERA:", align = "center"),
                         min = 0,
                         max = 10,
                         value = 4.15, round=FALSE, step = 0.01)),
      column(3, 
             sliderInput("WHIP",
                         h4("Projected Team WHIP:", align = "center"),
                         min = 0,
                         max = 7,
                         value = 1.297, round=FALSE, step = 0.01))
  ),
  
  fluidRow(
      column(3,
             p("OBP - On Base Percentage is the ratio of the batter's times-on-base (TOB) 
               (the sum of hits, walks, and times hit by pitch) to their number of plate appearances. "))
  ),
  
  fluidRow(
      column(4,
             h4("Predicted Number of Wins:"),
             textOutput("pred_W")),
      column(4,
             h4("Predicted to Make Playoffs?"),
             textOutput("pred_P"))
  )

))
