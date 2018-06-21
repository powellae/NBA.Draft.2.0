#PMVP Shiny App
#June 8, 2018
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

#BART----
options(java.parameters = "-Xmx5000m")
library("bartMachine")
set_bart_machine_num_cores(4)
Final <- read.csv("Final.csv")
Final <- Final %>%
  dplyr::select(player, Per.Cap, percentile, RSCI, adjOE, adjDE, Age, HT, Pos2, Numb, G, X3PA, FTA, Poss,
                Ast.Pos, Tov.Pos, Stl.G, Blk.G, TS, FTA.G, Three.G, Two.G, 
                Three.Per, Two.Per, FTA.FGA, Three.FGA, OR.G, DR.G, PF.G, adjTempo)

Final$RSCI <- as.factor(Final$RSCI)

wgt <- 2
prior <- c(rep(wgt, times = 2), rep(1, times = 23), rep(wgt,8))

bart_machine_Cv <- bartMachine( Final[,c(4:30)], Final$Per.Cap, cov_prior_vec = prior)

Prospects <- read.csv("Prospects.csv")
Prospects$RSCI <- as.factor(Prospects$RSCI)
Prospects$Age <- Prospects$AGE
Prospects$AGE <- NULL
Prospects <- na.omit(Prospects)

Prospects <- Prospects %>%
  dplyr::select(PLAYER, TEAM, YEAR, RSCI, adjOE, adjDE, Age, HT, Pos2, Numb, G, X3PA, FTA, Poss,
                Ast.Pos, Tov.Pos, Stl.G, Blk.G, TS, FTA.G, Three.G, Two.G, 
                Three.Per, Two.Per, FTA.FGA, Three.FGA, OR.G, DR.G, PF.G, adjTempo)

Prospects <- Prospects[!(Prospects$PLAYER=="Jacob Evans" & Prospects$Numb == 1),]
Prospects <- Prospects[!(Prospects$PLAYER=="Jerome Robinson" & Prospects$Numb == 1),]
Prospects <- Prospects[!(Prospects$PLAYER=="Justin Jackson" & Prospects$RSCI == "Top25"),]

Prospects$bart <- predict(bart_machine_Cv, Prospects[,c(4:30)])


BB <- bart_machine_get_posterior(bart_machine_Cv, Prospects[,c(4:30)])$y_hat_posterior_samples

for(i in 1:nrow(Prospects)){
  simulations <- BB[i,]
  Prospects$Ceiling[i] <- quantile(simulations, .95)
  Prospects$Floor[i] <- quantile(simulations, .05)
}

BigBoard <- read.csv("BigBoard.csv")
Prospects.Board <- merge(Prospects, BigBoard, all.y = TRUE, by.y = "Player", by.x = "PLAYER")
Prospects.Board$TEAM <- as.character(Prospects.Board$TEAM)
Prospects.Board$X <- as.character(Prospects.Board$X)

for(j in 1:nrow(Prospects.Board)){
  Prospects.Board$TEAM[is.na(Prospects.Board$TEAM)] <- "None"
  Prospects.Board$TEAM[j] <- ifelse(Prospects.Board$TEAM[j] == "None", Prospects.Board$X[j], Prospects.Board$TEAM[j])
}

Prospects.Board$Model.Rank <- 51-rank(Prospects.Board$bart, ties.method = "average")
Prospects.Board$Model.Rank <- round(as.numeric(ifelse(Prospects.Board$Model.Rank <= 0, "", Prospects.Board$Model.Rank)),0)

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
    ggtitle(title) + xlab("PMVP: 5th year contract value as percentage of salary cap") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    scale_x_continuous(limits = c(0,.3)) + theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24, hjust=0)) +
    theme(axis.title = element_text(color="black", face="bold", size=12))
  return(plot.distr)
}
player_distr2("Trae Young", "Collin Sexton")


#Statistics----
CYS <- read.csv("br_college.csv")
#CYS$Season <- as.numeric(as.character(CYS$Season))

PlayerStats <- function(PlayerName){
  Team <- Prospects$TEAM[which(grepl(PlayerName, Prospects$PLAYER))]
  CYS.Player <- CYS %>%
    filter(Season == "2017-18") %>%
    filter(Player == PlayerName) %>%
    filter(School == Team)
  
  Board <- Prospects.Board %>%
    filter(PLAYER == PlayerName)
  
  PLYR <- data.frame("Rank" = Board$Rank[1], "BART-Rank" = Board$Model.Rank[1], "Player" = PlayerName, 
                     "School" = Board$TEAM[1], "Year" = as.character(CYS.Player$Class[1]), "Pos" = as.character(CYS.Player$Pos[1]), "Age" = Board$Age[1], "Height" = Board$HT[1], "Games" = CYS.Player$G[1], 
           "MPG" = round(CYS.Player$MP[1]/CYS.Player$G[1],1), "PPG" = round(CYS.Player$PTS[1]/CYS.Player$G[1],1), "RPG" = round(CYS.Player$TRB[1]/CYS.Player$G[1],1),
           "APG" = round(CYS.Player$AST[1]/CYS.Player$G[1],1), "TS" = 100*round(CYS.Player$PTS[1]/(2*(CYS.Player$FGA[1] + .44*CYS.Player$FTA[1])),3),
           "Ceiling" = round(Board$Ceiling[1],4), "BART" =  round(Board$bart[1],4), "Floor" = round(Board$Floor[1],4))
  return(PLYR)
}
PlayerStats("Trae Young")

Prospects.Board$PLAYER <- as.character(Prospects.Board$PLAYER)
x0 <- PlayerStats((Prospects.Board$PLAYER[1]))
for(i in 2:nrow(Prospects.Board)){
  x <- PlayerStats(Prospects.Board$PLAYER[i])
  x0 <- rbind(x0, x)
}

PlayerInd <- function(PlayerName, PlayerName2, option){
  PLYR <- x0 %>%
    filter(Player == PlayerName | Player == PlayerName2)
  
  if(option == 1){
    return(PLYR[,c(1:6,11:13,15:17)])
  } else {
    return(PLYR)
  }
}
j<-PlayerInd("Trae Young", "Marvin Bagley III", 1)

brks <- quantile(x0[,15:17], probs = seq(.05,.95,.05), na.rm = TRUE)
clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
  {paste0("rgb(255,", ., ",", ., ")")}


#User Interface----
ui <- fluidPage(
  tags$h2("Player Market Value Projection (PMVP) NBA Draft Model 2.0"),
  tags$h4("By Alexander Powell"),
  tags$p("Find a detailed explanation of the PMVP output & methodology at ", tags$a(href="https://medium.com/ninety-four-by-fifty", "94x50.")),
  
  tabsetPanel(
    tabPanel("Player Comparison", tags$p(),
      tags$p("Comparing players is an important part of draft preparation. Below you can find the player comparison tool to allow you to compare the posterior distributions from my Bayesian Additive Regression Tree (BART) model for each collegiate player
             in this years NBA Draft (Sorry, the international players & those without a college career are not included yet). This tool enables you too see
             a player's projection beyond a singular number. You can see the risk and variance involved with particular players. The below posterior distributions are 
             built from 10,000 simulations of a player's career.", tags$b("Note: "), "every time you open this page the model will be rebuilt so a player's results
             can vary slightly becasue the model is built using simulation."), 
      tags$p("Check out the 'The Model' tab to see the full model rankings, my rankings, player statistics, and a full glossary."),
      wellPanel(
      fluidRow(
      column(6, selectInput(inputId = "name", label = "Select Player", choices = Prospects.Board$PLAYER)),
      column(6, selectInput(inputId = "name2", label = "Select Player", choices = Prospects.Board$PLAYER))),
      actionButton(inputId = "go", label = "Update")),
      plotOutput("gg"),
      tableOutput("p1")
    ),
    tabPanel("The Model",
          tags$h2("2018 NBA Draft Board"),
          tags$p("Welcome to the ", tags$b("PMVP NBA Draft Model")," ahead of the 2018 NBA Draft. The model is a Bayesian Additive Regression Tree Model (BART), that predicts
                 a prospects 'market value'. A player's 'market value' is measured by their 5th year contract value (first non-rookie contract), as this best
                 illustrates what teams think of the player in a relatively free market. This value is viewed as a proportion of the salary cap as an adjustment for
                 inflation. For example, a player above 0.2 (20% of the salary cap) is a likely all-star caliber player, while a player less than 0.1 is generally a
                 bench rotation player or at the end of an NBA bench."),
          tags$p(), tags$p(tags$h4(tags$b("Glossary:")), tags$b(" Rank-"), " my subjective player ranking based on video scouting, statistical projection, and other aspects such as injury history and 
                           character. ", tags$b("Bart-Rank: "), "the ranking of the BART PMVP model based on 10,000 simulations of a players career. ", tags$b("TS-"), " this is a 
                           player's true shooting percentage in their most recent college season, a measure of how efficient a player is shooting the basketball. ", tags$b("Ceiling-"), " the 95th percentile
                           of the simulations of a player's career. This is a measure of what a player's reasonable 'peak' might be. ", tags$b("BART-"), " the quantitative estimate of a player's 5th year market value
                           as a percentage of the salary cap. This is the expected value from the posterior distribution. ", tags$b("Floor-"), " the 5th percentile of the simulations of a player's career. 
                           This is a measure of what a player's reasonable 'worst case' career senario might be. The higher this is the safer of a pick that player will be."),
          tags$p("Note that players like Michael Porter Jr. who did not play more than 15 collegiate games are not included in the model rankings. The same is true for international players like Luka Doncic. Additionally, 
                 De'Anthony Melton was forced to sit out his entire sophomore year so the model only reflects his freshman season. Remember that the model is not a catch-all so it misses on understanding injuries, character issues, etc."),
                 dataTableOutput("p2"), tags$p(),
          tags$p("If you have any comments or questions please reach out to me on twitter at ", tags$a(href="https://twitter.com/aptigers", "@aptigers"),
          " or via email at aepowell95@gmail.com. Also please let me know what suggestions you might have to add to this project -- be it the addition of 
                 other players or other tools, I would love to make this useful and interesting for everyone.")
             
    )
  )
)
  

#Server---- 
server <- function(input, output) {
  data <- eventReactive(input$go, {
    player_distr2(input$name, input$name2)
  })
  data2 <- eventReactive(input$go, {
    PlayerInd(input$name, input$name2, 1)
  })
  output$gg <- renderPlot({
    data()
  })
  output$p1 <- renderTable(digits = c(0,0,0,0,0,0,0,0,0,1,1,1,1,1,4,4,4),{
    data2()
  })
  output$p2 <- renderDataTable(
    DT::datatable(x0, rownames = FALSE) %>% formatStyle(names(x0[,15:17]), backgroundColor = styleInterval(brks, clrs))
  )
}
  
  
shinyApp(ui = ui, server = server)