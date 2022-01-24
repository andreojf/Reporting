library(plotly)
library(shiny)

ui <- shinyUI(fluidPage(
  plotlyOutput("myPlot"),
  tableOutput("tab"),
  actionButton("regen","Generate New Points")
))

server <- shinyServer(function(input, output) {
  
  n <- 100  
  rv <- reactiveValues(m=data.frame(x=rnorm(n),y=rnorm(n)))
  
  output$tab <- renderTable(rv$m)
  
  observeEvent(input$regen,{
    rv$m <- data.frame(x=rnorm(n),y=rnorm(n))
  })
  
  output$myPlot <- renderPlotly({
    plot_ly() %>%  add_markers(data=rv$m,x=~x,y=~y  )
  })
})
shinyApp(ui, server)