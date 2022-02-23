
##DATA FORMATTING
{
    data1 <- read.csv("D:/surff/Desktop/TAMU Class Folders/STAT 684/Final Project Data/baseballdata.csv", stringsAsFactors = F)
    data2 <- read.csv("D:/surff/Desktop/TAMU Class Folders/STAT 684/Final Project Data/MoneyBall_baseball.csv", stringsAsFactors = F)
    pitch_data <- read.csv("D:/surff/Desktop/TAMU Class Folders/STAT 684/Final Project Data/Pitching_Data.csv", stringsAsFactors = F)
    pitch_data_no_lg <- read.csv("D:/surff/Desktop/TAMU Class Folders/STAT 684/Final Project Data/Pitching_Data_No_LG.csv", stringsAsFactors = F)
    
    library(tidyverse)
    
    data2$Playoffs2 <- ifelse(data2$Playoffs==1, "YES", "NO")
    #ggplot(data2, aes(Playoffs, RS, group=Playoffs)) + geom_boxplot() + facet_grid(.~Playoffs)
    #ggplot(data2, aes(OBP, W)) + geom_point(aes(color=Playoffs2)) + geom_smooth(model=lm) + 
    #    ggtitle("On Base Percentage VS Win Total") + theme_classic()
    
    
    #table(data2$Team)
    #table(data1$current)
    #str(data1)
    #str(data2)
    
    
    teams <- as.data.frame(cbind(
        c("ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET", "HOU", "KCR", "LAA", "LAD", "MIA","MIL", 
          "MIN", "NYY", "NYM", "OAK", "PHI", "PIT", "SDP", "SEA", "SFG", "STL", "TBR", "TEX", "TOR", "WSN"),
        c("Arizona Diamondbacks", "Atlanta Braves", "Baltimore Orioles", "Boston Red Sox", "Chicago Cubs", "Chicago White Sox",
          "Cincinnati Reds", "Cleveland Indians", "Colorado Rockies", "Detroit Tigers", "Houston Astros", "Kansas City Royals",
          "Los Angeles Angels of Anaheim", "Los Angeles Dodgers", "Miami Marlins", "Milwaukee Brewers", "Minnesota Twins",
          "New York Yankees", "New York Mets", "Oakland Athletics", "Philadelphia Phillies", "Pittsburgh Pirates", 
          "San Diego Padres", "Seattle Mariners", "San Francisco Giants", "St. Louis Cardinals", "Tampa Bay Rays", 
          "Texas Rangers", "Toronto Blue Jays", "Washington Nationals")))
    colnames(teams) <- c("Abv", "current")
    teams$Abv <- as.character(teams$Abv)
    teams$current <- as.character(teams$current)
    data2$current <- ifelse(data2$Team=="ANA", "LAA", 
                            ifelse(data2$Team=="CAL", "LAA",
                                   ifelse(data2$Team=="FLA", "MIA",
                                          ifelse(data2$Team=="KCA", "OAK",
                                                 ifelse(data2$Team=="MLN", "ATL",
                                                        ifelse(data2$Team=="MON", "WSN",
                                                               ifelse(data2$Team=="SEP", "MIL",
                                                                      ifelse(data2$Team=="TBD", "TBR",
                                                                             ifelse(data2$Team=="WSA", "TEX", 
                                                                                    as.character(data2$Team))))))))))
    
    #table(data2$current)
    data2$join_val <- paste(data2$current,"-",data2$Year)
    
    pitch_data$current <- ifelse(pitch_data$Tm=="ANA", "LAA", 
                                 ifelse(pitch_data$Tm=="CAL", "LAA",
                                        ifelse(pitch_data$Tm=="FLA", "MIA",
                                               ifelse(pitch_data$Tm=="KCA", "OAK",
                                                      ifelse(pitch_data$Tm=="MLN", "ATL",
                                                             ifelse(pitch_data$Tm=="MON", "WSN",
                                                                    ifelse(pitch_data$Tm=="SEP", "MIL",
                                                                           ifelse(pitch_data$Tm=="TBD", "TBR",
                                                                                  ifelse(pitch_data$Tm=="WSA", "TEX", 
                                                                                         as.character(pitch_data$Tm))))))))))
    pitch_data$RA.G <- as.numeric(pitch_data$RA.G)
    pitch_data$ERA <- as.numeric(pitch_data$ERA)
    pitch_data$CG <- as.numeric(pitch_data$CG)
    pitch_data$tSho <- as.numeric(pitch_data$tSho)
    pitch_data$cSho <- as.numeric(pitch_data$cSho)
    pitch_data$SV <- as.numeric(pitch_data$SV)
    pitch_data$H <- as.numeric(pitch_data$H)
    pitch_data$R <- as.numeric(pitch_data$R)
    pitch_data$ER <- as.numeric(pitch_data$ER)
    pitch_data$HR <- as.numeric(pitch_data$HR)
    pitch_data$BB <- as.numeric(pitch_data$BB)
    pitch_data$IBB <- as.numeric(pitch_data$IBB)
    pitch_data$SO <- as.numeric(pitch_data$SO)
    pitch_data$HBP <- as.numeric(pitch_data$HBP)
    pitch_data$BK <- as.numeric(pitch_data$BK)
    pitch_data$WP <- as.numeric(pitch_data$WP)
    pitch_data$ERAP <- as.numeric(pitch_data$ERA.)
    pitch_data$FIP <- as.numeric(pitch_data$FIP)
    pitch_data$WHIP <- as.numeric(pitch_data$WHIP)
    pitch_data$H9 <- as.numeric(pitch_data$H9)
    pitch_data$HR9 <- as.numeric(pitch_data$HR9)
    pitch_data$BB9 <- as.numeric(pitch_data$BB9)
    pitch_data$SO9 <- as.numeric(pitch_data$SO9)
    pitch_data$SO.W <- as.numeric(pitch_data$SO.W)
    pitch_data$LOB <- as.numeric(pitch_data$LOB)
    
    
    pitch_data_no_lg$current <- ifelse(pitch_data_no_lg$Tm=="ANA", "LAA", 
                                       ifelse(pitch_data_no_lg$Tm=="CAL", "LAA",
                                              ifelse(pitch_data_no_lg$Tm=="FLA", "MIA",
                                                     ifelse(pitch_data_no_lg$Tm=="KCA", "OAK",
                                                            ifelse(pitch_data_no_lg$Tm=="MLN", "ATL",
                                                                   ifelse(pitch_data_no_lg$Tm=="MON", "WSN",
                                                                          ifelse(pitch_data_no_lg$Tm=="SEP", "MIL",
                                                                                 ifelse(pitch_data_no_lg$Tm=="TBD", "TBR",
                                                                                        ifelse(pitch_data_no_lg$Tm=="WSA", "TEX", 
                                                                                               as.character(pitch_data_no_lg$Tm))))))))))
    pitch_data_no_lg$RA.G <- as.numeric(pitch_data_no_lg$RA.G)
    pitch_data_no_lg$ERA <- as.numeric(pitch_data_no_lg$ERA)
    pitch_data_no_lg$CG <- as.numeric(pitch_data_no_lg$CG)
    pitch_data_no_lg$tSho <- as.numeric(pitch_data_no_lg$tSho)
    pitch_data_no_lg$cSho <- as.numeric(pitch_data_no_lg$cSho)
    pitch_data_no_lg$SV <- as.numeric(pitch_data_no_lg$SV)
    pitch_data_no_lg$H <- as.numeric(pitch_data_no_lg$H)
    pitch_data_no_lg$R <- as.numeric(pitch_data_no_lg$R)
    pitch_data_no_lg$ER <- as.numeric(pitch_data_no_lg$ER)
    pitch_data_no_lg$HR <- as.numeric(pitch_data_no_lg$HR)
    pitch_data_no_lg$BB <- as.numeric(pitch_data_no_lg$BB)
    pitch_data_no_lg$IBB <- as.numeric(pitch_data_no_lg$IBB)
    pitch_data_no_lg$SO <- as.numeric(pitch_data_no_lg$SO)
    pitch_data_no_lg$HBP <- as.numeric(pitch_data_no_lg$HBP)
    pitch_data_no_lg$BK <- as.numeric(pitch_data_no_lg$BK)
    pitch_data_no_lg$WP <- as.numeric(pitch_data_no_lg$WP)
    pitch_data_no_lg$ERAP <- as.numeric(pitch_data_no_lg$ERA.)
    pitch_data_no_lg$FIP <- as.numeric(pitch_data_no_lg$FIP)
    pitch_data_no_lg$WHIP <- as.numeric(pitch_data_no_lg$WHIP)
    pitch_data_no_lg$H9 <- as.numeric(pitch_data_no_lg$H9)
    pitch_data_no_lg$HR9 <- as.numeric(pitch_data_no_lg$HR9)
    pitch_data_no_lg$BB9 <- as.numeric(pitch_data_no_lg$BB9)
    pitch_data_no_lg$SO9 <- as.numeric(pitch_data_no_lg$SO9)
    pitch_data_no_lg$SO.W <- as.numeric(pitch_data_no_lg$SO.W)
    pitch_data_no_lg$LOB <- as.numeric(pitch_data_no_lg$LOB)   
    pitch_data_no_lg$IP <- as.numeric(pitch_data_no_lg$IP)
    pitch_data_no_lg$G <- as.numeric(pitch_data_no_lg$G)
    
    pitch_data_no_lg$join_val <- paste(pitch_data_no_lg$current,"-",pitch_data_no_lg$Year)
    
    data1_trim <- data1[data1$Year>=1962,]
    data1_trim <- data1_trim[data1_trim$Year<=2012,]
    
    right = function(text, num_char) {
        substr(text, nchar(text) - (num_char-1), nchar(text))
    }
    data1_trim$WAR <- right(data1_trim$Top.Player, 6)
    data1_trim$WAR <- gsub(" ","",data1_trim$WAR)
    data1_trim$WAR <- gsub("[()]","",data1_trim$WAR)
    data1_trim$WAR <- as.numeric(data1_trim$WAR)
    table(data1_trim$current)
    data1_merge <- left_join(data1_trim, teams, by="current")
    data1_merge$join_val <- paste(data1_merge$Abv,"-",data1_merge$Year)
    
    data1_merge$Attendance <- gsub(",","",data1_merge$Attendance)
    data1_merge$Attendance <- as.numeric(data1_merge$Attendance)
    
    join_data <- left_join(data2, data1_merge, by="join_val")
    #[,c(4,8:9,12:16,29:41)]
    join_data2 <- left_join(join_data, pitch_data_no_lg, by="join_val")
    join_data2$SV.G <- (join_data2$SV / join_data2$G)
    join_data2$CG.G <- (join_data2$CG / join_data2$G)
    join_data2$tSho.G <- (join_data2$tSho / join_data2$G)
    join_data2$cSho.G <- (join_data2$cSho / join_data2$G)
    join_data2$LOB <- (join_data2$LOB/ join_data2$G)
    join_data2$RS.G <- (join_data2$RS/ join_data2$G)
    join_data2$win_adj <- (join_data2$W.x/join_data2$G.x)*162
    
    #RA.G  ERA + WHIP + H9 + HR9 + BB9 + SO9 + SO.W + LOB + ERAP + SV.G + CG.G + tSho.G + cSho.G, Num.P + FIP, 
    #RS.G OBP   +  BatAge + Num.Bat + WAR, BA + SLG
    #win_adj, playoffs.x
    final_set <- join_data2[,c(7:10,36,38,39,43,49,53,74:81,84:90)]
}

##Prediction Functions GLM
predict_test <- function(model) {
    pred2 <- predict(model, newdata = test, type = "response")
    
    cutoff <- seq(from=0.001, to=.8, by=.001)
    cutoff_pred <- as.data.frame(NULL)
    count <- 1
    for (i in cutoff) {
        pred2_check <- ifelse(pred2 >= i, 1, 0)
        correct <- ifelse(pred2_check==test$Playoffs.x, 1, 0)
        cutoff_pred[count,1] <- i
        cutoff_pred[count,2] <- sum(correct)/length(correct)
        count <- count + 1
    }
    
    cutoff_pred_sort <- cutoff_pred[order(cutoff_pred$V2, decreasing = TRUE),]
    colnames(cutoff_pred_sort) <- c("Cutoff", "Accuracy")
    pred2_check <- ifelse(pred2 >= cutoff_pred_sort[1,1], 1, 0)
    correct <- ifelse(pred2_check==test$Playoffs.x, 1, 0)
    print(sum(correct)/length(correct))
    print(head(cutoff_pred_sort))
}

predict_test_cutoff <- function(model, cutoff) {
    pred2 <- predict(model, newdata = test, type = "response")
    pred2_check <- ifelse(pred2 >= cutoff, 1, 0)
    correct <- ifelse(pred2_check==test$Playoffs.x, 1, 0)
    sum(correct)/length(correct)
}



#####K-FOLD CV: MODEL with RS, RA, W, Playoffs calculated individually####
#Initail Model, only run once for independent variables
{
    set.seed(787)
    x <- sample(nrow(join_data2), nrow(join_data2)*.8, replace=F)
    train <- join_data2[x,]
    test <- join_data2[-x,]
    
    #+ Num.P + FIP
    train_RA <- train
    f_RA <- "RA.G ~ (ERA + WHIP + H9 + HR9 + BB9 + SO9 + SO.W + LOB + ERAP + SV.G + CG.G + tSho.G + cSho.G )^2"
    RA_lm1 <- lm(f_RA, train_RA)
    summary(RA_lm1)
    
    backward_BIC_RA <- step(RA_lm1, k=log(nrow(train_RA)))
    summary(backward_BIC_RA)
    #plot(backward_BIC_RA)
    RA_lm_simple <- lm(RA.G ~ (ERA + WHIP + tSho.G), train_RA)
    summary(RA_lm_simple)
    
    #Attendance + + BA + SLG
    train_RS <- train
    f <- "RS.G ~ (OBP   +  BatAge + Num.Bat + WAR)^2"
    RS_lm1 <- lm(f, train_RS)
    summary(RS_lm1)
    RS_lm_simple <- lm(RS.G ~ (OBP   +  BatAge + Num.Bat), train_RS)
    summary(RS_lm_simple)
    
    backward_BIC_RS <- step(RS_lm1, k=log(nrow(train_RS)))
    summary(backward_BIC_RS)
    #plot(backward_BIC_RS)
}
#K-Fold
{
    set.seed(272727)
    k_folds <- split(join_data2, c("set1","set2","set3","set4","set5"))
    accuracy_list <- as.list(NULL)
    accuracy_list2 <- as.list(NULL)
    W_list <- as.list(NULL)
    playoff_list <- as.list(NULL)
    RA.G_list <- as.list(NULL)
    RS.G_list <- as.list(NULL)
    playoff_list_py <- as.list(NULL)
    accuracy_list_py <- as.list(NULL)
    accuracy_list2_py <- as.list(NULL)
    for(i in 1:5){
        
        test <- k_folds[[i]]
        train <- join_data2[-as.numeric(rownames(test)),]
        
        
        ##Predict RA.G##
        {
            train_RA <- train
            #+ H9 + (ERA*HR9) + (SO9*CG.G) + (HR9*CG.G)
            f_RA <- "RA.G ~ ERA  + HR9 + BB9 + SO9 + ERAP + SV.G + CG.G + tSho.G   "
            RA_lm2 <- lm(f_RA, train_RA)
            summary(RA_lm2)
            RA.G_list[[i]] <- RA_lm2
        }
        
        ##Predict RS.G##
        {
            train_RS <- train
            f_RS <- "RS.G ~ OBP + Num.Bat"
            RS_lm2 <- lm(f_RS, train_RS)
            summary(RS_lm2)   
            RS.G_list[[i]] <- RS_lm2
        }
        
        ##Predict W##
        {
            pred_final_RS <- predict(RS_lm2, newdata=train_RS, type='response')
            pred_final_RA <- predict(RA_lm2, newdata=train_RA, type='response')
            
            
            train_W <- as.data.frame(cbind(train$win_adj, pred_final_RS, pred_final_RA))
            colnames(train_W) <- c("win_adj", "RS.G", "RA.G")
            
            f <- "win_adj ~ RS.G + RA.G"
            W_lm1 <- lm(f, data=train_W)
            summary(W_lm1)
            W_list[[i]] <- W_lm1
            
            
            #pythagorean wins
            win_percent <- (162*pred_final_RS)^2 / ((162*pred_final_RS)^2 + (162*pred_final_RA)^2)
            win_est <- win_percent*162
        }
        
        ##Predict Playoffs##
        {
            pred_final_W <- predict(W_lm1, newdata=train, type="response")
            
            train_Playoff <- as.data.frame(cbind(train$Playoffs.x, pred_final_W, train$Attendance, train$Num.P))
            colnames(train_Playoff) <- c("Playoffs.x","win_adj","Attendance", "Num.P")
            
            # + Attendance + Num.P
            f <- "Playoffs.x ~ win_adj "
            log_lm1 <- glm(f, data=train_Playoff, family = "binomial")
            summary(log_lm1)
            playoff_list[[i]] <- log_lm1
            
            accuracy_list[[i]] <- predict_test(log_lm1)
            accuracy_list2[[i]] <- predict_test_cutoff(log_lm1, 0.5)
            
        }
        
    }
    #mean(accuracy_list2[[1]], accuracy_list2[[2]], accuracy_list2[[3]], accuracy_list2[[4]], accuracy_list2[[5]])
    results <- as.data.frame(cbind(rbind(accuracy_list2[[1]], accuracy_list2[[2]], accuracy_list2[[3]], 
                                   accuracy_list2[[4]], accuracy_list2[[5]])),c("Set 1","Set 2","Set 3","Set 4","Set 5"))
    mean(results$V1)
}

#Accuracy K-fold
{
    ####FINAL MODEL
    #RA.G ~ ERA  + HR9 + BB9 + SO9 + ERAP + SV.G + CG.G + tSho.G 
    #RS.G ~ OBP + Num.Bat
    #win_adj ~ RS.G + RA.G
    #Playoffs.x ~ win_adj
    #90.6% accurate
    mean(.346, .333, .429, .590, .542)
    
    
    
    
    ##full: .9312
    mean(.557, .385, .665, .577, .444)
    
    ##no attendance, number of pitchers: .9109
    mean(.323, .306, .387, .514, .451)
    
    ##no number of pitch: .919
    mean(.546, .387, .687, .574, .460)
    
    ##no BA in RS.G: .9312
    mean(.557, .385, .661, .578, .445)
    
    ##no BA, SLG in RS.G: .9231
    mean(.554, .423, .698, .574, .511)
    
    ##no BA, SLG in RS.G, no FIP in RA.G: .9231
    mean(.553, .421, .699, .575, .514)
    
    ##no BA, SLG in RS.G, no FIP, ERA in RA.G: .911
    mean(.562, .434, .744, .599, .521)
    
    ##ERA + H9 + HR9 + BB9 + SO9 + ERAP + SV.G + CG.G + tSho.G + (ERA*HR9) + (HR9*CG.G) + (SO9*CG.G)
    ##OBP + Num.Bat
    ##.9231
    mean(.557, .422, .698, .566, .510)
    
    ##ERA + HR9 + BB9 + SO9 + ERAP + SV.G + CG.G + tSho.G + (ERA*HR9) + (HR9*CG.G) + (SO9*CG.G)
    ##OBP + Num.Bat
    ##W.x + Attendance
    ##.911
    mean(.378, .43, .68, .392, .52)
    
    ##ERA + HR9 + BB9 + SO9 + ERAP + SV.G + CG.G + tSho.G + (ERA*HR9) + (HR9*CG.G) + (SO9*CG.G)
    ##OBP + Num.Bat
    ##W.x
    ##.9028
    mean(.350, .336, .424, .553, .496)
    
    
    
    
    
    
    
    ####BEST MODEL####
    ##ERA + HR9 + BB9 + SO9 + ERAP + SV.G + CG.G + tSho.G + (ERA*HR9) + (HR9*CG.G) + (SO9*CG.G)
    ##OBP + Num.Bat
    ##W.x + Attendance + Num.P
    ##.9271
    mean(.552, .423, .721, .585, .504)
    
    
    
    
    ##91.1%
}


#PYTHAGOREAN
{
    set.seed(272727)
    k_folds <- split(join_data2, c("set1","set2","set3","set4","set5"))
    W_list <- as.list(NULL)
    playoff_list <- as.list(NULL)
    RA.G_list <- as.list(NULL)
    RS.G_list <- as.list(NULL)
    playoff_list_py <- as.list(NULL)
    accuracy_list_py <- as.list(NULL)
    accuracy_list2_py <- as.list(NULL)
    for(i in 1:5){
        
        test <- k_folds[[i]]
        train <- join_data2[-as.numeric(rownames(test)),]
        
        
        ##Predict RA.G##
        {
            train_RA <- train
            #+ H9 + (ERA*HR9) + (HR9*CG.G) + (SO9*CG.G)
            f_RA <- "RA.G ~ ERA  + HR9 + BB9 + SO9 + ERAP + SV.G + CG.G + tSho.G "
            RA_lm2 <- lm(f_RA, train_RA)
            summary(RA_lm2)
            RA.G_list[[i]] <- RA_lm2
        }
        
        ##Predict RS.G##
        {
            train_RS <- train
            f_RS <- "RS.G ~ OBP + Num.Bat"
            RS_lm2 <- lm(f_RS, train_RS)
            summary(RS_lm2)   
            RS.G_list[[i]] <- RS_lm2
        }
        
        ##Predict W##
        {
            pred_final_RS <- predict(RS_lm2, newdata=train_RS, type='response')
            pred_final_RA <- predict(RA_lm2, newdata=train_RA, type='response')
            
            train_W <- as.data.frame(cbind(train$win_adj, pred_final_RS, pred_final_RA))
            colnames(train_W) <- c("win_adj", "RS.G", "RA.G")
            
            f <- "win_adj ~ RS.G + RA.G"
            W_lm1 <- lm(f, data=train_W)
            summary(W_lm1)
            W_list[[i]] <- W_lm1
            
            
            #pythagorean wins
            win_percent <- (162*pred_final_RS)^2 / ((162*pred_final_RS)^2 + (162*pred_final_RA)^2)
            win_est <- win_percent*162
        }
        
        ##Predict Playoffs##
        {
            pred_final_W <- predict(W_lm1, newdata=train, type="response")
            
            
            #pythagorean wins
            train_Playoff_py <- as.data.frame(cbind(train$Playoffs.x, win_est, train$Attendance, train$Num.P))
            colnames(train_Playoff_py) <- c("Playoffs.x","win_est","Attendance", "Num.P")
            
            # + Attendance + Num.P
            f <- "Playoffs.x ~ win_est "
            log_lm1_py <- glm(f, data=train_Playoff_py, family = "binomial")
            summary(log_lm1_py)
            playoff_list_py[[i]] <- log_lm1_py
            
            accuracy_list_py[[i]] <- predict_test(log_lm1_py)
            accuracy_list2_py[[i]] <- predict_test_cutoff(log_lm1_py, 0.765)
            
        }
        
    }
    mean(accuracy_list2_py[[1]], accuracy_list2_py[[2]], accuracy_list2_py[[3]], accuracy_list2_py[[4]], accuracy_list2_py[[5]])
}

mean(.765, .787, .799, .797, .794)
#77.2%


library(car)
vif(RA_lm2)
vif(RS_lm2)
vif(W_lm1)
#vif(log_lm1)






library(corrplot)

corrplot(cor(cbind(join_data2$ERA, join_data2$H9, join_data2$HR9, join_data2$BB9, join_data2$SO9,
                   join_data2$ERAP, join_data2$SV.G, join_data2$CG.G, join_data2$tSho.G)))
cor(cbind(join_data2$ERA, join_data2$HR9, join_data2$BB9, join_data2$SO9,
          join_data2$ERAP, join_data2$SV.G, join_data2$CG.G, join_data2$tSho.G))


corrplot(cor(cbind(join_data2$OBP, join_data2$Num.Bat)))




f_RS <- "RA.G ~ (ERA + FIP + WHIP + H9 + HR9 + BB9 + SO9 + SO.W + LOB + ERAP + SV.G + CG.G + tSho.G + cSho.G )^2"

f_RA <- "RS.G ~ (OBP + SLG + BA +  BatAge + Num.Bat + WAR)^2"

RA_corr <- train[,c(53,74:81,84:88)]
corrplot(cor(RA_corr))
cor(RA_corr)

RS_corr <- train[,c(7:8,36,38,43)]
corrplot(cor(RS_corr))
cor(RS_corr)



f <- "RS.G ~ (OBP + SLG + BA +  BatAge + Num.Bat + WAR)^2"
RS_lm1 <- lm(f, train_RS)
summary(RS_lm1)
backward_BIC_RS <- step(RS_lm1, k=log(nrow(train_RS)))
summary(backward_BIC_RS)

f <- "RS.G ~ (OBP + SLG + BatAge + Num.Bat + WAR)^2"
RS_lm2 <- lm(f, train_RS)
summary(RS_lm2)
backward_BIC_RS2 <- step(RS_lm2, k=log(nrow(train_RS)))
summary(backward_BIC_RS2)


















##########No Longer Need to Run#########

##MODEL with RS, RA, W, Playoffs calculated individually##
{
    set.seed(787)
    x <- sample(nrow(join_data2),0.8*nrow(join_data2),replace = F)
    train <- join_data2[x,]
    test <- join_data2[-x,]
    
    
    ##Predict RA.G##
    {
        #[,c(39,45:46,53:60,63:67)]
        train_RA <- train
        
        
        #corr1 <- cor(train_RA)
        #corrplot::corrplot(corr1)
        
        #+ Num.P
        f <- "RA.G ~ (ERA + FIP + WHIP + H9 + HR9 + BB9 + SO9 + SO.W + LOB + ERAP + SV.G + CG.G + tSho.G + cSho.G )^2"
        RA_lm2 <- lm(f, train_RA)
        summary(RA_lm2)
        
        #backward_AIC_RA <- step(RA_lm2)
        backward_BIC_RA <- step(RA_lm2, k=log(nrow(train_RA)))
        #summary(backward_AIC_RA)
        summary(backward_BIC_RA)
        
        #plot(backward_AIC_RA)
        #plot(backward_BIC_RA)
        
        pred_RA <- predict(RA_lm2, newdata=test, type="response")
        mean(abs(pred_RA-test$RA.G))*162
        
        pred_RA_BIC <- predict(backward_BIC_RA, newdata=test, type='response')
        mean(abs(pred_RA_BIC-test$RA.G))*162
        
        #pred_RA_AIC <- predict(backward_AIC_RA, newdata=test, type='response')
        #mean(abs(pred_RA_AIC-test$RA.G))*162
    }
    
    ##Predict RS.G##
    {
        #[,c(7:9,35,36,38,43,68)]
        train_RS <- train
        #Attendance +
        f <- "RS.G ~ (OBP + SLG + BA +  BatAge + Num.Bat + WAR)^2"
        RS_lm1 <- lm(f, train_RS)
        summary(RS_lm1)
        
        #backward_AIC_RS <- step(RS_lm1)
        backward_BIC_RS <- step(RS_lm1, k=log(nrow(train_RS)))
        #summary(backward_AIC_RS)
        summary(backward_BIC_RS)
        
        #plot(backward_AIC_RS)
        #plot(backward_BIC_RS)
        
        pred_RS <- predict(RS_lm1, newdata=test, type="response")
        mean(abs(pred_RS-test$RS.G))*162
        
        pred_RS_BIC <- predict(backward_BIC_RS, newdata=test, type='response')
        mean(abs(pred_RS_BIC-test$RS.G))*162
        
        #pred_RS_AIC <- predict(backward_AIC_RS, newdata=test, type='response')
        #mean(abs(pred_RS_AIC-test$RS.G))*162
    }
    
    ##Predict W##
    {
        pred_final_RS <- predict(backward_BIC_RS, newdata=train, type='response')
        pred_final_RA <- predict(backward_BIC_RA, newdata=train, type='response')
        
        train_W <- as.data.frame(cbind(train$W.x, pred_final_RS, pred_final_RA))
        colnames(train_W) <- c("W.x", "RS.G", "RA.G")
        
        f <- "W.x ~ (RS.G + RA.G)^2"
        W_lm1 <- lm(f, data=train_W)
        summary(W_lm1)
        #plot(W_lm1)
        
        pred_W <- predict(W_lm1, newdata=test, type="response")
        mean(abs(pred_W-test$W.x))
    }
    
    ##Predict Playoffs
    {
        pred_final_W <- predict(W_lm1, newdata=train, type="response")
        
        train_Playoff <- as.data.frame(cbind(train$Playoffs.x, pred_final_W, train$Attendance, train$Num.P))
        colnames(train_Playoff) <- c("Playoffs.x","W.x","Attendance", "Num.P")
        
        #
        f <- "Playoffs.x ~ W.x + Attendance + Num.P"
        log_lm1 <- glm(f, data=train_Playoff, family = "binomial")
        summary(log_lm1)
        #plot(log_lm1)
        
        predict_test(log_lm1)
    }
    
}

#Initial Models (Logistic, stepwise logistic)
{
    
    f <- "Playoffs.x ~ RS + RA.x + OBP + SLG + BA + BatAge + PAge + Num.Bat + Num.P + Attendance + WAR"
    m1 <- glm(f, data=train, family="binomial")
    summary(m1)
    
    backward <- step(m1)
    summary(backward)
    
    empty <- glm(Playoffs.x ~ 1, data=train, family="binomial")
    forward <- step(empty, scope=list(lower=formula(empty), 
                                      upper=formula(m1)),direction = "forward")
    summary(forward)
    

    
    ##Stepwise Model Test
    #+ RS + RA.x + OBP
    f_final <- "Playoffs.x ~ W.x + Attendance + Num.P"
    mf <- glm(f_final, data=train, family="binomial")
    summary(mf)
    anova(mf)
    
    predict_test(mf)

    
    
    ##Full Model Test
    
    predict_test(m1)
    predict_test(forward)
    predict_test(backward)
}



##COOL PLOT##
join_data3 <- join_data2
join_data3$run_diff <- join_data2$RS - join_data2$RA.x
ggplot(join_data3, aes(run_diff, W.L..x, color=Playoffs2)) + geom_point() +
    xlab("Total Run Differential") + ylab("Win Percentage") + ggtitle("MLB Playoffs") 
    
sum(join_data3$run_diff<0 & join_data3$Playoffs.x==1)
    












