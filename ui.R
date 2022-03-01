
library(shiny)
library(shinythemes)

shinyUI(fixedPage(
    #Theme
    theme = shinytheme("superhero"),
    
    tags$style(
        HTML('
             #buttons {
                    padding-top: 0px

             }
             

             
             ')
        ),
    
# ##HTLM
# #fluidrow0 {
# height: 20px;
# width: 100%
# }
# 
# #fluidrow1 {
# height: 50px;
# width: 100%
# }
# 
# #fluidrow2 {
# height: 50px;
# width: 100%
# }
# 
# #fluidrow3 {
# height: 20px;
# width: 100%
# }
# 
# #fluidrow4 {
# height: 50px;
# width: 100%
# }
# 
# #fluidrow5 {
# height: 50px;
# width: 100%
# }
# 
# #fluidrow6 {
# height: 50px;
# width: 100%
# }
# 
# #fluidrow7 {
# height: 150px;
# width: 100%
# }
  # Application title
  titlePanel(h2("MLB Playoff Predictor", style = "text-align:center; font-weight:bold;
                margin-top:0; margin-bottom:0; margin-left:0; margin-right:0"),
             windowTitle = "MLB Playoff Predictor"),
  
  br(),
  br(),
  
  #mobile users note
  fluidRow(id="fluidrow0",
           column(12,
                        p("Tip for MOBILE USERS: Use device horizontally!",
                            style = "font-family: 'times'; color:lightgrey; 
                          font-size:12pt; font-weight:bold; text-decoration:underline"))
           ),
  
  #User inputs for OBP, ERA, WHIP
  fluidRow(id="fluidrow1",
      column(4, 
             sliderInput("OBP",
                         h4("Projected Team OBP:", style = "text-align:center; font-weight:bold;
                margin-top:0; margin-bottom:0; margin-left:0; margin-right:0"),
                         min = 0.25,
                         max = 0.4,
                         value = 0.317, round=FALSE, step = 0.001)), #0.317
      column(4, 
             sliderInput("ERA",
                         h4("Projected Team ERA:", style = "text-align:center; font-weight:bold;
                margin-top:0; margin-bottom:0; margin-left:0; margin-right:0"),
                         min = 1,
                         max = 8,
                         value = 4.15, round=FALSE, step = 0.01)), #4.15
      column(4, 
             sliderInput("WHIP",
                         h4("Projected Team WHIP:", style = "text-align:center; font-weight:bold;
                margin-top:0; margin-bottom:0; margin-left:0; margin-right:0"),
                         min = 0.1,
                         max = 2,
                         value = 1.297, round=FALSE, step = 0.001)) #1.297
  ),
  
  #Quick description for each metric
  fluidRow(id="fluidrow2",
      column(4,
             p("OBP - On Base Percentage is the ratio of the batter's times-on-base 
               (the sum of hits, walks, and times hit by pitch) to their number of plate appearances. ",
               style = "font-family: 'times'; color:lightgrey")),
      column(4, 
             p("ERA - Earned Run Average is the average of earned runs allowed by a pitcher per nine innings pitched.",
               style = "font-family: 'times'; color:lightgrey")),
      column(4, 
             p("WHIP - Walks plus Hits per Inning Pitched is calculated by adding the number of walks and 
               hits allowed and dividing this sum by the number of innings pitched.",
               style = "font-family: 'times'; color:lightgrey"))
  ),
  
  #Note about starting values
  fluidRow(id="fluidrow3",
      column(12,
             p("*Starting values are MLB League Average for 2021 season",
               style = "font-family: 'times'; color:lightgrey; font-size:8pt"))
  ),
  
  fluidRow(id="fluidrow3",
           column(12,
                  p("*Wins = Wins Adjusted to 162 Game Season",
                    style = "font-family: 'times'; color:lightgrey; font-size:8pt"))
  ),
  br(),

  
  #Output results and plot
  fluidRow(id="fluidrow4",
      column(4, align="center", id="buttons",
             h4("Reset Sliders: ", align = "center",
                style = "text-align:center; font-weight:bold;
                margin-top:0; margin-bottom:10px; margin-left:0; margin-right:0"),
             actionButton("update", "RESET", width='100%')
      ),
      column(4, align="center", 
             
             selectInput("team", width='100%', h4("Choose a team:", 
                    align = "center", style = "text-align:center; font-weight:bold;
                    margin-top:0; margin-bottom:5%; margin-left:0; margin-right:0"),
                            choices = list(' ' = ' ',
                                'AL Central' = c(team_df$team_name[team_df$Lg=="AL Central"]),
                                'AL East' = c(team_df$team_name[team_df$Lg=="AL East"]),
                                'AL West' = c(team_df$team_name[team_df$Lg=="AL West"]),
                                
                                'NL Central' = c(team_df$team_name[team_df$Lg=="NL Central"]),
                                'NL East' = c(team_df$team_name[team_df$Lg=="NL East"]),
                                'NL West' = c(team_df$team_name[team_df$Lg=="NL West"])),
                    multiple = FALSE, selected=' ')),
      column(4, align="center",
             
             selectInput('year', width='100%', h4("Choose a year:", align = "center",
                                    style = "text-align:center; font-weight:bold;
                    margin-top:0; margin-bottom:5%; margin-left:0; margin-right:0"), multiple = FALSE, choices=NULL))
  ),
  
  
  br(),
  br(),

  fluidRow(id="fluidrow5",
      column(4,
             fluidRow(
                 h4("Made the Playoffs?", 
                    style = "text-align:center; font-weight:bold;
                    margin-top:0; margin-bottom:0; margin-left:0; margin-right:0"),
                 
                 h1(textOutput("actual_P"), align = "center",
                    style = "display: block; text-align:center; font-weight: bold")
             )
             
      ),
      column(4,
             fluidRow(
                 h4("Actual Number of Wins*:",
                    style = "text-align:center; font-weight:bold;
                    margin-top:0; margin-bottom:0; margin-left:0; margin-right:0"),
                 
                 h1(textOutput("actual_W"), align = "center",
                    style = "display: block; text-align:center; font-weight: bold")
             )
      ),
      column(4,
             fluidRow(
                 h4("Selected Team Stats:",
                    style = "text-align:center; font-weight:bold;
                    margin-top:0; margin-bottom:0; margin-left:0; margin-right:0"),
                 column(2),
                 column(4, 
                        h5("OBP: ", style = "text-align:center; font-weight:bold"),
                        h5("ERA: ", style = "text-align:center; font-weight:bold"),
                        h5("WHIP: ", style = "text-align:center; font-weight:bold")),
                 column(4, 
                        h5(textOutput("OBP"), align = "center",
                           style = "display: block; text-align:center; font-weight: bold"),
                        h5(textOutput("ERA"), align = "center",
                           style = "display: block; text-align:center; font-weight: bold"),
                        h5(textOutput("WHIP"), align = "center",
                           style = "display: block; text-align:center; font-weight: bold")),
                 column(2)
             )
      )
  ),
  
  br(),
  br(),
  
  fluidRow(id="fluidrow6",
      column(4,
             fluidRow(
             h4("Predicted to Make Playoffs?", 
                style = "text-align:center; font-weight:bold;
                    margin-top:0; margin-bottom:0; margin-left:0; margin-right:0"),
             
             h1(textOutput("pred_P"), align = "center",
                style = "display: block; text-align:center; font-weight: bold")
                    )
             
             ),
      column(4,
             fluidRow(
                 h4("Predicted Number of Wins*:",
                    style = "text-align:center; font-weight:bold;
                margin-top:0; margin-bottom:0; margin-left:0; margin-right:0"),
                 
                 h1(textOutput("pred_W"), align = "center",
                    style = "display: block; text-align:center; font-weight: bold")
                    )
             ),
      column(4,
             fluidRow(
                 h4("Likely Min Wins Needed:",
                    style = "text-align:center; font-weight:bold;
                    margin-top:0; margin-bottom:0; margin-left:0; margin-right:0"),
                 h1("92", align = "center",
                    style = "display: block; text-align:center; font-weight: bold")
                     )
             )
  ),


  br(),
  br(),


  
  br(),
  br(),
  fluidRow(id="fluidrow7",
      column(12,
             plotOutput("plot1"))

  ),
  br(),
  br(),
  
  br(),
  br()
))  
  
  
  
  
  
 