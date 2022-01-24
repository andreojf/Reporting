library(shiny)
library(data.table)
library(DT)

ui <- fluidPage(
  
  
  ui <- navbarPage("Reporting",
                   
    tabPanel("Données",
             sidebarLayout(
               sidebarPanel(
                 # ### IMPORTATION DE LA TABLE
                 fileInput("upload", "Upload your file", accept = c(".csv")),
                 hr(),
                 p("Attention:Le fichier à télécharger doit être rempli en totalité avant l'importation."),
                 width = 2
               ),
             mainPanel(dataTableOutput("donnees"), width = "100%"))),
    
    
    tabPanel("Portefeuille",
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
                      textOutput("prtfDoutEtLitig"))
             ),
             fluidRow(
              hr(),
              tableOutput("portef")
             )
           ),
    
    navbarMenu("Engagements",
      tabPanel("Notation"),
      tabPanel("Segment"),
      tabPanel("BCEAO"),
      tabPanel("Hors bilan",
               
               # Montant aval des traites
               fluidRow(
                 column(4,
                        wellPanel(
                          h4("Engagements hors bilan"),
                          "petite definition...")
                 ),
                 column(4, 
                        numericInput("aval",
                                  "Montant des avals de traite",
                                  value = NULL, 
                                    width = "100%")
                        ),
                 column(2,
                        actionButton("avalValide", "Valider"))
               )
               ),
      tabPanel("50 plus engagements")
    ),
    
    
    tabPanel("CDL par Age",
             
             # Ajouter le bouton de sélection de date
             fluidRow(
               column(3,
                      wellPanel(
                        h4("CDL par age"),
                        "petite definition...")
                      ),
               column(8, 
                      dateInput("date",
                                "Entrer une date de référence",
                                value = "2000/01/01"))
             )
             ),
    tabPanel("Holding"),
    tabPanel("Indicateurs Appetence"),
    tabPanel("Synthèse")
  )
  
  
  
  
  # ### IMPORTATION DE LA TABLE
  # fileInput("upload", "Upload your file", accept = c(".csv")),
  # tableOutput("files")
)

























server <- function(input, output, session){
  
  ##################### IMPORT DES DONNEES #########################################
  # transformer les données en reactivesvalues
  data <- reactive({

    # s'assurer que le fichier ait bien été uploadé
    req(input$upload)
    #print(input$upload$name)

    ext<- tools::file_ext(input$upload$name)
    switch(ext,
           csv = fread(input$upload$datapath),
           validate("Invalidate file, please upload csv file")
    )
  })

  output$donnees <- renderDataTable({
    
    DT::datatable(
      data(),
      #filter = 'top', extensions = c('Buttons', 'Scroller'),
      options = list(scrollY = "100%",
                     scrollX = "100%",
                     deferRender = TRUE,
                     scroller = TRUE,
                     # paging = TRUE,
                     # pageLength = 25,
                     # buttons = list('excel',
                     #               list(extend = 'colvis', targets = 0, visible = FALSE)),
                     dom = 'lBfrtip',
                     fixedColumns = TRUE), 
      rownames = FALSE)
    
  })
  
  ###################### SITUATION DU PORTEFEUILLE ############################
  
  # table du portefeuille
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
      Montant = c(4,32,0,0,0,0,0,0)
    )
  )
  
  output$portef <- renderTable(
    pf$portefeuille
    )
  
  output$prtfSain <- renderText({
    r <- pf$portefeuille %>% 
      filter(intitule %in% c("PORTEFEUILLE D'EFFETS",
                             "CREDITS CCT",
                             "CREDITS M&LT",
                             "COMPTES DEBITEURS")) %>%
      summarise(val = sum(Montant))
    paste0(r$val)
    }
  )
}

shinyApp(ui, server)
