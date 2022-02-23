
library(shiny)

load("SavedModels/RA_model.rda")
load("SavedModels/RS_model.rda")
load("SavedModels/W_model.rda")
load("SavedModels/P_model.rda")




shinyServer(function(input, output) {

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
        
        test_P <- data.frame(win_adj = pred_lm3)
        #colnames(test_P) <- "win_adj"
        
        pred_glm1 <- predict(glm1, newdata = test_P, type = "response")
        #predictionP <- paste("Playoffs: ",ifelse(pred_glm1 > 0.5, "Yes", "No"), sep="")
        predictionP <- ifelse(pred_glm1 > 0.5, "Yes", "No")
        predictions <- c(predictionW, predictionP)
        #predictions <- paste(predictionW, predictionP, sep="  ")
        
        return(predictions)
    }
    
    pred_vector <- reactive(trainModels(test_data()))
    
    output$pred_W <- renderText({
        pred_vector()[1]
    })

    output$pred_P <- renderText({
        pred_vector()[2]
    })
    

})
