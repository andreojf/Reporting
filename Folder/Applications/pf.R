library(shiny)

ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      # ### IMPORTATION DE LA TABLE
      fileInput("upload", "Upload your file", accept = c(".csv")),
      hr(),
      p("Attention:Le fichier à télécharger doit être rempli en totalité avant l'importation."),
      width = 3,
    ),
    
    mainPanel(
      h2("Situation du portefeuille", align = "center"),
      hr(),
      fluidRow(
        column(4,
               h4("Portefeuille sain"),
               textOutput("prtfSain")),
        column(4,
               h4("Portefeuille restructuré"),
               textOutput("prtfRest")),
        column(4,
               h4("Portefeuille Douteux et litigieux"),
               textOutput("prtfDouteuxetLitig"))
      ),
      tableOutput("tab")
    )
  )
)

server <- function(input, output, session) {
  pf <- reactiveValues(
    portefeuille = data.frame(
      intitule = c("PORTEFEUILLE D'EFFETS",
                   "CREDITS CCT",
                   "CREDITS M&LT",
                   "COMPTES DEBITEURS",
                   "ECHEANCES IMPAYEES",
                   "PORTEFEUILLE  RESTRUCTURE",
                   "CREDITS PARTICULIERS",
                   "CREDITS ENTREPRISES"),
      Montant = c(0,0,0,0,0,0,0,0)
    )
  )
  
  output$tab <- renderTable(
    pf$portefeuille
  )
}

shinyApp(ui, server)