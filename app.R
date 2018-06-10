#PMVP Shiny App
#June 8, 2018
library(shiny)
library(ggplot2)
library(dplyr)

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

Prospects$bart <- predict(bart_machine_Cv, Prospects[,c(4:30)])

BB <- bart_machine_get_posterior(bart_machine_Cv, Prospects[,c(4:30)])$y_hat_posterior_samples

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



#Statistics----
CYS <- read.csv("br_college.csv")
#CYS$Season <- as.numeric(as.character(CYS$Season))

PlayerStats <- function(PlayerName){
  Team <- Prospects$TEAM[which(grepl(PlayerName, Prospects$PLAYER))]
  CYS.Player <- CYS %>%
    filter(Season == "2017-18") %>%
    filter(Player == PlayerName) %>%
    filter(School == Team)
  
  return(c(PlayerName, as.character(Team), as.character(CYS.Player$Class[1])
           , as.character(CYS.Player$Pos[1])))
}
PlayerStats("Trae Young")
#User Interface----
ui <- fluidPage(
  tags$h2("Player Market Value Projection (PMVP) NBA Draft Model 2.0"),
  tags$p("Find a detailed explanation of the PMVP methodology at ", tags$a(href="medium.com/ninety-four-by-fifty", "94x50.")),
  
  tabsetPanel(
    tabPanel("Tab1",
      wellPanel(
      fluidRow(
      column(6, selectInput(inputId = "name", label = "Select Player", choices = Prospects$PLAYER)),
      column(6, selectInput(inputId = "name2", label = "Select Player", choices = Prospects$PLAYER))),
      actionButton(inputId = "go", label = "Update")),
      plotOutput("gg"),
      dataTableOutput("p1")
    ),
    tabPanel("Tab2",
          tags$h2("Big Board Displayed Here.")   
             
    )
  )
)
  

#Server---- 
server <- function(input, output) {
  data <- eventReactive(input$go, {
    player_distr2(input$name, input$name2)
  })
  data2 <- eventReactive(input$go, {
    PlayerStats(input$name)
  })
  output$gg <- renderPlot({
    data()
  })
  output$p1 <- renderDataTable({
    data2()
  })
  
}
  
  
shinyApp(ui = ui, server = server)