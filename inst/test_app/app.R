#test shiny app
ui<- fluidPage(
  sidebarLayout(
    sidebarPanel(
    sliderInput("obs",
                "Number of observations:",
                min = 0,
                max = 1000,
                value = 500)
  ),
  mainPanel(
    plotOutput("distPlot")
    )
  )
)

server<- function(input, output) {
  output$distPlot <- renderPlot(
    hist(rnbinom(n= input$obs, size= 300, prob= 0.3))
  )
}

if (interactive()){
  shinyApp(ui, server)
}
