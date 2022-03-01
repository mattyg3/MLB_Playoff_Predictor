### Model Train for shiny

dataImport <- read.csv("final_set.csv")
dataImport2 <- read.csv("total_join_set.csv")

dataImport2 <- dataImport2[dataImport2$Year.x>1964,]

#save dataImport2 for meta data
save(dataImport2,file='SavedModels/dataImport2.rda')

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



Team <- c(unique(dataImport2$Team))
team_name <- unique(dataImport2$Tm.x)
tdf <- data.frame(cbind(Team,team_name))
df <- left_join(tdf, dataImport2[,c(1,22,23)], by="Team")
team_df <- df[!duplicated(df[c('Tm.x')]),]
team_df <- team_df[,-3]

#fix formatting for selectinput
team_df$Lg[team_df$Team=='KCA'] = 'AL Central'
team_df$Lg[team_df$Team=='MLN'] = 'NL Central'


##Save team_df
save(team_df, file='SavedModels/team_df.rda')

