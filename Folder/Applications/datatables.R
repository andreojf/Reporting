library(shiny)
library(shinydashboard)
library(DT)


####/UI/####
header <- dashboardHeader()

sidebar <- dashboardSidebar()

body <- dashboardBody(
  DT::dataTableOutput("test")
)

ui <- dashboardPage(header, sidebar, body)

####/SERVER/####
server <- function(input, output, session) {
  
  data <- as_tibble(matrix(runif(200000), ncol=30))
  
  output$test <- DT::renderDataTable({
    
    DT::datatable(
      data,
      filter = 'top', extensions = c('Buttons', 'Scroller'),
      options = list(scrollY = 650,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     # paging = TRUE,
                     # pageLength = 25,
                     buttons = list('excel',
                                    list(extend = 'colvis', targets = 0, visible = FALSE)),
                     dom = 'lBfrtip',
                     fixedColumns = TRUE), 
      rownames = FALSE)
  })  
  
}

shinyApp(ui, server)