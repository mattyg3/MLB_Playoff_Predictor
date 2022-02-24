
##DATA FORMATTING
{
    data1 <- read.csv("baseballdata.csv", stringsAsFactors = F)
    data2 <- read.csv("MoneyBall_baseball.csv", stringsAsFactors = F)
    pitch_data <- read.csv("Pitching_Data.csv", stringsAsFactors = F)
    pitch_data_no_lg <- read.csv("Pitching_Data_No_LG.csv", stringsAsFactors = F)
    
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
    
    write.csv(final_set, file="final_set.csv", row.names = F)
    write.csv(join_data2, file="total_join_set.csv", row.names = F)
}