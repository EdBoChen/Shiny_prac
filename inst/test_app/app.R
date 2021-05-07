#test shiny app
icd_tbl<- readRDS("/data/testdataset.rds")
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
  ),
  fluidPage(tableOutput("tbl"))
)

server<- function(input, output) {
  output$distPlot <- renderPlot(
    hist(rnbinom(n= input$obs, size= 300, prob= 0.3))
  )
  output$tbl<- renderTable(
    head(icd_tbl, n= 10))
}

if (interactive()){
  shinyApp(ui, server)
}
