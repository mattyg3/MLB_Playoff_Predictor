### Model Train for shiny

dataImport <- read.csv("D:/surff/Desktop/MLB_Playoff_Predictor/CleanData/final_set.csv")

set.seed(3743)
par(mfrow=c(2,2))

# + tSho.G
f_RA <- "RA.G ~ (ERA + WHIP)" #linear model for RUNS ALLOWED
lm1 <- lm(f_RA, data=dataImport)
summary(lm1)
plot(lm1)
save(lm1, file="SavedModels/RA_model.rda")
# + BatAge
f_RS <- "RS.G ~ (OBP)" #linear model for RUNS SCORED
lm2 <- lm(f_RS, data=dataImport)
summary(lm2)
plot(lm2)
save(lm2, file="SavedModels/RS_model.rda")

f_W <- "win_adj ~ RS.G + RA.G" #linear model for WINS
lm3 <- lm(f_W, data=dataImport)
summary(lm3)
plot(lm3)
save(lm3, file="SavedModels/W_model.rda")

f_P <- "Playoffs.x ~ win_adj" #logistic model for making the playoffs
glm1 <- glm(f_P, data=dataImport, family="binomial")
summary(glm1)
save(glm1, file="SavedModels/P_model.rda")



#Predictions
test_data <- dataImport[50, c(1, 5, 10, 12, 22)]

pred_lm1 <- predict(lm1, newdata = test_data, type = "response")
pred_lm2 <- predict(lm2, newdata = test_data, type = "response")
test_W <- as.data.frame(cbind(pred_lm1, pred_lm2))
colnames(test_W) <- c("RA.G", "RS.G")
pred_lm3 <- predict(lm3, newdata = test_W, type = "response")
test_P <- as.data.frame(pred_lm3)
colnames(test_P) <- "win_adj"
pred_glm1 <- predict(glm1, newdata = test_P, type = "response")
predictionW <- paste("Wins: ", round(pred_lm3,0), sep="")
predictionP <- paste("Playoffs: ",ifelse(pred_glm1 > 0.5, "Yes", "No"), sep="")

