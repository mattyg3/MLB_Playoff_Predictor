
library(shiny)
library(shinythemes)

shinyUI(fixedPage(
    #Theme
    theme = shinytheme("superhero"),
    
  # Application title
  titlePanel(h2("MLB Playoff Predictor", style = "text-align:center; font-weight:bold;
                margin-top:0; margin-bottom:0; margin-left:0; margin-right:0"),
             windowTitle = "MLB Playoff Predictor"),
  
  br(),
  br(),
  
  #User inputs for OBP, ERA, WHIP
  fluidRow(
      column(4, 
             sliderInput("OBP",
                         h4("Projected Team OBP:", style = "text-align:center; font-weight:bold;
                margin-top:0; margin-bottom:0; margin-left:0; margin-right:0"),
                         min = 0.25,
                         max = 0.4,
                         value = 0.317, round=FALSE, step = 0.001)),
      column(4, 
             sliderInput("ERA",
                         h4("Projected Team ERA:", style = "text-align:center; font-weight:bold;
                margin-top:0; margin-bottom:0; margin-left:0; margin-right:0"),
                         min = 1,
                         max = 8,
                         value = 4.15, round=FALSE, step = 0.01)),
      column(4, 
             sliderInput("WHIP",
                         h4("Projected Team WHIP:", style = "text-align:center; font-weight:bold;
                margin-top:0; margin-bottom:0; margin-left:0; margin-right:0"),
                         min = 0.1,
                         max = 2,
                         value = 1.297, round=FALSE, step = 0.001))
  ),
  
  #Quick description for each metric
  fluidRow(
      column(4,
             p("OBP - On Base Percentage is the ratio of the batter's times-on-base 
               (the sum of hits, walks, and times hit by pitch) to their number of plate appearances. ",
               style = "font-family: 'times'; color:lightgrey")),
      column(4, 
             p("ERA - Earned Run Average is the average of earned runs allowed by a pitcher per nine innings pitched",
               style = "font-family: 'times'; color:lightgrey")),
      column(4, 
             p("WHIP - Walks plus Hits per Inning Pitched is calculated by adding the number of walks and 
               hits allowed and dividing this sum by the number of innings pitched.",
               style = "font-family: 'times'; color:lightgrey"))
  ),
  
  #Note about starting values
  fluidRow(
      column(12,
             p("*Starting values are MLB League Average for 2021 season",
               style = "font-family: 'times'; color:lightgrey; font-size:8pt"))
  ),
  
  
  br(),
  br(),
  br(),
  
  #Output results and plot
  fluidRow(
      column(4,
             fluidRow(
                 h4("Predicted to Make Playoffs?", 
                    style = "text-align:center; font-weight:bold;
                    margin-top:0; margin-bottom:0; margin-left:0; margin-right:0"),
                 
                 h1(textOutput("pred_P"), align = "center",
                    style = "display: block; text-align:center; font-weight: bold")
             ),
             
             br(),
             br(),
             br(),

             
             fluidRow(
                 h4("Predicted Number of Wins:",
                    style = "text-align:center; font-weight:bold;
                margin-top:0; margin-bottom:0; margin-left:0; margin-right:0"),
                 
                 h1(textOutput("pred_W"), align = "center",
                    style = "display: block; text-align:center; font-weight: bold")
             ),
             
             br(),
             br(),
             br(),

             
             fluidRow(
                 h4("Likely Minimum Wins Needed:",
                    style = "text-align:center; font-weight:bold;
                    margin-top:0; margin-bottom:0; margin-left:0; margin-right:0"),
                 h1("92", align = "center",
                    style = "display: block; text-align:center; font-weight: bold")
             )
             
             ),
      column(8,
             plotOutput("plot1"))

  )

))
