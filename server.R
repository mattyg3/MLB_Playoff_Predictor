
library(shiny)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(dplyr)


load("SavedModels/RA_model.rda")
load("SavedModels/RS_model.rda")
load("SavedModels/W_model.rda")
load("SavedModels/P_model.rda")
load('SavedModels/dataImport2.rda')
load('SavedModels/team_df.rda')




shinyServer(function(input, output, session) {
    

    
    test_data <- reactive({
        test_data <- data.frame(OBP = input$OBP,
                                ERA = input$ERA,
                                WHIP = input$WHIP)
        test_data
    })
    



    trainModels <- function(input){
        pred_lm1 <- predict(lm1, newdata = input, type = "response")
        
        pred_lm2 <- predict(lm2, newdata = input, type = "response")
        
        test_W <- data.frame(RA.G = pred_lm1, RS.G = pred_lm2)
        #colnames(test_W) <- c("RA.G", "RS.G")
        
        pred_lm3 <- predict(lm3, newdata = test_W, type = "response")
        pred_lm3.2 <- if(pred_lm3 > 162) {
            162
        } else if (pred_lm3 < 0) {
            0
        } else round(pred_lm3)

        #predictionW <- paste("Wins: ", pred_lm3.2, sep="")
        predictionW <- pred_lm3.2
        
        test_P <- data.frame(win_adj = pred_lm3.2)
        #colnames(test_P) <- "win_adj"
        
        pred_glm1 <- predict(glm1, newdata = test_P, type = "response")
        #predictionP <- paste("Playoffs: ",ifelse(pred_glm1 > 0.5, "Yes", "No"), sep="")
        predictionP <- ifelse(pred_glm1 > 0.5, "YES", "NO")
        predictions <- c(predictionW, predictionP)
        #predictions <- paste(predictionW, predictionP, sep="  ")
        
        return(predictions)
    }
    
    pred_vector <- reactive(trainModels(test_data()))
    
    

    gplot_render <- reactive({
        gplot1 <- ggplot(data=dataImport2, aes(round(win_adj), fill = factor(Playoffs.x))) + 
            #geom_freqpoly(binwidth=6, size=1, colour="black") +
            #geom_area(aes(y=..count.., fill=Playoffs.x, group=Playoffs.x), stat="bin", binwidth=1) +
            geom_dotplot(stackgroups=TRUE, binpositions="all", binwidth=1, dotsize=0.8) +
            scale_fill_brewer(labels = c("No","Yes"), palette = "Set1") +
            guides(fill=guide_legend(title="Playoffs")) +
            ggtitle("Distribution of Team Wins: 1962-2012") +
            xlab("Number of Wins") +
            ylab("Count of MLB Teams") +
            xlim(c(0,162)) +
            ylim(c(0,1)) +
            scale_y_continuous(NULL, breaks = NULL) +
            geom_vline(xintercept=as.numeric(pred_vector()[1]), color="purple", size=2) +
            #geom_vline(xintercept=92, color="black", size=2, linetype=2) +
            geom_vline(xintercept=as.numeric(wins()[[1]]), color="black", size=2) +
            # geom_label(aes(x=as.numeric(pred_vector()[1]),
            #                y=60,label="Predicted Wins"), fill = "purple") +
            annotate(x=as.numeric(pred_vector()[1]),y=90,
                     label=paste0("Predicted Wins: ", as.numeric(pred_vector()[1])),
                     vjust=2,geom="label") +
            #annotate(x=92,y=100,label="Likely Wins Needed",vjust=2,geom="label") +
            #geom_label() +
            annotate(x=as.numeric(wins()[[1]]),y=100,
                     label=paste0(input$year,"-",input$team,": ",as.numeric(wins()[[1]])),
                     vjust=2,geom="label")
            
            
        gplot1 + theme_economist() #+ theme(legend.position="bottom")
        #gplot1
    }) 
    
    
    wins <- reactive({
        round(unname(dataImport2 %>%
            filter(Year.x == input$year, Tm.x == input$team) %>%
            select(win_adj)))
    })

    playoffs <- reactive({
        unname(dataImport2 %>%
            filter(Year.x == input$year, Tm.x == input$team) %>%
            select(Playoffs2))
    })
    
    OBP <- reactive({
        unname(dataImport2 %>%
                   filter(Year.x == input$year, Tm.x == input$team) %>%
                   select(OBP))
    })
    
    ERA <- reactive({
        unname(dataImport2 %>%
                   filter(Year.x == input$year, Tm.x == input$team) %>%
                   select(ERA))
    })
    
    WHIP <- reactive({
        unname(dataImport2 %>%
                   filter(Year.x == input$year, Tm.x == input$team) %>%
                   select(WHIP))
    })


    
    ############Selective Lists
    team <- reactive({
        filter(dataImport2, Tm.x == input$team)
    })
    observeEvent(team(), {
        choices <- unique(team()$Year.x)
        updateSelectInput("year", choices = choices, session=session)

    })
    
    year <- reactive({
        filter(dataImport2, Year.x == input$year)
    })
    
    observeEvent(year(), {
        if(input$team==' '){
            updateSliderInput(inputId = "OBP", value = 0.317 ,session=session)
            updateSliderInput(inputId = "ERA", value = 4.15 ,session=session)
            updateSliderInput(inputId = "WHIP", value = 1.297 ,session=session)
        } else {
            updateSliderInput(inputId = "OBP", value = dataImport2$OBP[dataImport2$Tm.x==input$team &
                                                                           dataImport2$Year.x==input$year],
                              session=session)
            updateSliderInput(inputId = "ERA", value = dataImport2$ERA[dataImport2$Tm.x==input$team &
                                                                           dataImport2$Year.x==input$year],
                              session=session)
            updateSliderInput(inputId = "WHIP", value = dataImport2$WHIP[dataImport2$Tm.x==input$team &
                                                                             dataImport2$Year.x==input$year],
                              session=session)
        }
    })
    
    ##########Action Button
    observeEvent(input$update,{
            updateSelectInput(inputId = "team", selected=' ', session=session)
            updateSliderInput(inputId = "OBP", value = 0.317 ,session=session)
            updateSliderInput(inputId = "ERA", value = 4.15 ,session=session)
            updateSliderInput(inputId = "WHIP", value = 1.297 ,session=session)

    })

    
    
    #########OUTPUTS
    output$pred_W <- renderText({
        pred_vector()[1]
    })
    
    output$pred_P <- renderText({
        pred_vector()[2]
    })
    
    
    # output$data_table <- renderTable({
    #     tb1 <- team() %>%
    #         filter(Year.x == input$year) %>%
    #         select(OBP, ERA, WHIP)
    #     tb <- cbind(c("OBP","ERA", "WHIP"),t(tb1))
    #     tb
    #     
    # }, colnames= FALSE, spacing = "l", align = 'c')
    
    output$actual_W <- renderText({
        wins()[[1]]
    })
    
    output$actual_P <- renderText({
        playoffs()[[1]]
    })
    
    output$OBP <- renderText({
        OBP()[[1]]
    })
    
    output$ERA <- renderText({
        ERA()[[1]]
    })
    
    output$WHIP <- renderText({
        WHIP()[[1]]
    })
    
    ###create ggplot
    output$plot1 <- renderPlot({
        gplot_render()
        
    })
    
    
})
