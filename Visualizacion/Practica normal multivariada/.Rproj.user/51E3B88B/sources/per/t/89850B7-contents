#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(MASS)
# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(
    
      sidebarPanel(
        textInput("corr", label = "Correlación de Pearson", value = "0.8"),
        textInput("n", label = "Tamaño de la muestra", value = "150"),
        actionButton('norm', label = 'Generar muestra aleatoria normal')

      ),
      mainPanel(
        plotOutput('grafica', click = "plot_click"),
        verbatimTextOutput('summary')
        
      )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  misDatos <- reactiveValues(muestra = NULL)
  
  observe({
    input$norm #Quiero que se ejecute cada vez que pulses el boton
    media <- c(0,0)
    corel <- isolate(as.numeric(input$corr))
    sig <- matrix(c(1,corel,corel,1),2)
    valor <- isolate(as.numeric(input$n))
    misDatos$muestra <- mvrnorm(valor,media,sig)
  })
  
  observeEvent(input$plot_click,{
    misDatos$muestra <- rbind(misDatos$muestra,c(input$plot_click$x,input$plot_click$y))
  })
  
  output$grafica<-renderPlot({
    plot(misDatos$muestra)
  })

  output$summary <- renderText({
    paste0('La correlación de Pearson es: ',cor(misDatos$muestra)[1,2])
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

