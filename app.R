library(shiny)
ui <- fluidPage(
  #textInput(inputId = "name", label = "Enter player name", value = "Trae Young"),
  selectInput(inputId = "name", label = "Select Player", choices = Prospects$PLAYER),
  actionButton(inputId = "go", label = "Update"),
  plotOutput("gg")
)
  

  
server <- function(input, output) {
  data <- eventReactive(input$go, {
    player_distr(input$name)
  })
  output$gg <- renderPlot({
    data()
  })
  
}
  
  
shinyApp(ui = ui, server = server)