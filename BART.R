options(java.parameters = "-Xmx5000m")
library("bartMachine")
set_bart_machine_num_cores(4)

library(ggplot2)

#Training----
#Final <- read.csv("H:/NBA DRAFT/Final.csv")

Final <- Final %>%
  dplyr::select(player, Per.Cap, percentile, RSCI, adjOE, adjDE, Age, HT, Pos2, Numb, G, X3PA, FTA, Poss,
                Ast.Pos, Tov.Pos, Stl.G, Blk.G, TS, FTA.G, Three.G, Two.G, 
                Three.Per, Two.Per, FTA.FGA, Three.FGA, OR.G, DR.G, PF.G, adjTempo)

Final$RSCI <- as.factor(Final$RSCI)

#Divide into test and train set
set.seed(78695690)
trainingRows <- sample(1:nrow(Final), 0.7*nrow(Final))
training <- Final[trainingRows, ]
test <- Final[-trainingRows, ]
wgt <- 2
prior <- c(rep(wgt, times = 2), rep(1, times = 23), rep(wgt,8))

bart_machine_Cv2 <- bartMachine( training[,c(4:30)], training$Per.Cap, cov_prior_vec = prior)

summary(bart_machine_Cv2)
#k_fold_cv(Final[,c(4:30)], Final$Per.Cap, k_folds = 10)

train.rf <- randomForest(Per.Cap ~ RSCI + adjOE + adjDE + Age + HT + Pos2 + Numb + G + X3PA + FTA + Poss +
                           Ast.Pos + Tov.Pos + Stl.G + Blk.G + TS + FTA.G + Three.G + Two.G + 
                           Three.Per + Two.Per + FTA.FGA + Three.FGA + OR.G + DR.G + PF.G + adjTempo, training, ntree = 500, importance = T)
print(train.rf)

#plot(train.rf)


test$bart <- predict(bart_machine_Cv2, test[,c(4:30)])
test$rf <- predict(train.rf, test[,c(4:30)])
test$rating <- .9*test$rf + .1*test$bart

rmse.bart <- sqrt(mean((test$percentile - test$bart)^2))
rmse.rf <- sqrt(mean((test$percentile - test$rf)^2))
rmse.rating <- sqrt(mean((test$percentile - test$rating)^2))
rmse.bart
rmse.rf
rmse.rating


#RMSE
df <- data.frame(b = seq(0,1,0.05))
df$r <- 1 - df$b
df$rmse <- NA

for(i in 1:nrow(df)){
  a <- df$b[i]
  b <- df$r[i]
  dd <- test %>%
    dplyr::mutate(rating4 = a*bart + b*rf)
  dd$help <- (dd$percentile - dd$rating4)^2
  df$rmse[i] <- sqrt(mean(dd$help))
}

#Fitted
Final$bart <- predict(bart_machine_Cv2, Final[,c(4:30)])
Final$rf <- predict(train.rf, Final[,c(4:30)])
Final$rating <- .6*Final$rf + .4*Final$bart

Final.Board <- Final %>%
  dplyr::select(player, percentile, Per.Cap, bart, rf, rating)

#Bart Testing ----
bart_machine_Cv <- bartMachine( Final[,c(4:30)], Final$Per.Cap, cov_prior_vec = prior)
summary(bart_machine_Cv)

draft.rf <- randomForest(Per.Cap ~ RSCI + adjOE + adjDE + Age + HT + Pos2 + Numb + G + X3PA + FTA + Poss +
                           Ast.Pos + Tov.Pos + Stl.G + Blk.G + TS + FTA.G + Three.G + Two.G + 
                           Three.Per + Two.Per + FTA.FGA + Three.FGA + OR.G + DR.G + PF.G + adjTempo, training, ntree = 500, importance = T)

#Predicting 2018 Draft Class ----
Prospects <- read.csv("Prospects.csv")
Prospects$RSCI <- as.factor(Prospects$RSCI)
Prospects$Age <- Prospects$AGE
Prospects$AGE <- NULL
Prospects <- na.omit(Prospects)

Prospects <- Prospects %>%
  dplyr::select(PLAYER, TEAM, YEAR, RSCI, adjOE, adjDE, Age, HT, Pos2, Numb, G, X3PA, FTA, Poss,
                  Ast.Pos, Tov.Pos, Stl.G, Blk.G, TS, FTA.G, Three.G, Two.G, 
                  Three.Per, Two.Per, FTA.FGA, Three.FGA, OR.G, DR.G, PF.G, adjTempo)

Prospects$bart <- predict(bart_machine_Cv, Prospects[,c(4:30)])
Prospects$rf <- predict(draft.rf, Prospects[,c(4:30)])
Prospects$rating <- .5*Prospects$rf + .5*Prospects$bart

BigBoard <- Prospects %>%
  dplyr::select(PLAYER, TEAM, Pos2, bart)

write.csv(BigBoard, "H:/NBA DRAFT/BigBoard2.csv")

BB <- bart_machine_get_posterior(bart_machine_Cv, Prospects[,c(4:30)])$y_hat_posterior_samples
check = 138 #Trae Young
j <- BB[check,]
df2 <- data.frame(X=j)
check2 = 127 #Collin Sexton
k <- BB[check2,]
df3 <- data.frame(X=k)

#qplot(df2$X, geom="histogram") 
Prospects$bart[check]
mean(df2$X)
Prospects$PLAYER[check]

ggplot() +
  geom_density(data=df2, aes(X), alpha = 0.5, fill = "red", color = "red") + geom_density(data=df3, aes(X), alpha = 0.5, fill = "blue", color = "blue") +
  annotate("text", x = .18, y = 14, label = "Trae Young", color = "red") +
  annotate("text", x = .21, y = 7.5, label = "Shai Gilgeous-Alexander", color = "blue") +
  ggtitle("Posterior Distributions: Trae Young and Shai Gilgeous-Alexander") + xlab("PMVP") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#Function for pulling player distribution
player_distr <- function(PlayerName){
  simulations <- BB[which(grepl(PlayerName, Prospects$PLAYER)),]
  df <- data.frame(X=simulations)
  title <- paste(PlayerName)
  plot.distr <- ggplot() +
    geom_density(data=df, aes(X), alpha = 0.75, fill = "blue", color = "blue") +
    annotate("text", x = mean(simulations), y = 7.5, label = PlayerName, color = "white") +
    annotate("text", x = mean(simulations), y = 6.5, label = round(mean(simulations),2), color = "white") +
    ggtitle(title) + xlab("PMVP") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    scale_x_continuous(limits = c(0,.3))
  return(plot.distr)
}
player_distr("Trae Young")




#Function for comparing player distributions
player_distr2 <- function(PlayerName, PlayerName2){
  simulations <- BB[which(grepl(PlayerName, Prospects$PLAYER)),]
  df <- data.frame(X=simulations)
  simulations2 <- BB[which(grepl(PlayerName2, Prospects$PLAYER)),]
  df2 <- data.frame(X=simulations2)
  title <- paste(PlayerName, "&", PlayerName2, sep = " ")
  plot.distr <- ggplot() +
    geom_density(data=df, aes(X), alpha = 0.75, fill = "blue", color = "blue") +
    annotate("text", x = .25, y = 12.5, label = PlayerName, color = "blue") +
    annotate("text", x = .25, y = 11.5, label = round(mean(simulations),4), color = "blue") +
    geom_density(data=df2, aes(X), alpha = 0.75, fill = "red", color = "red") +
    annotate("text", x = .25, y = 10, label = PlayerName2, color = "red") +
    annotate("text", x = .25, y = 9, label = round(mean(simulations2),4), color = "red") +
    ggtitle(title) + xlab("PMVP") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    scale_x_continuous(limits = c(0,.3)) + theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24, hjust=0)) +
    theme(axis.title = element_text(color="black", face="bold", size=12))
  return(plot.distr)
}
player_distr2("Trae Young", "DeAndre Ayton")
