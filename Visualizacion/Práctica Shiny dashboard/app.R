library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(MASS)
ui <- shinyUI({
  dashboardPage(
    dashboardHeader(title = 'Prácticas'),
    dashboardSidebar(
      sidebarMenu(
        menuItem('Práctica 1',tabName = 'tab1',icon = icon('area-chart')),
      
        menuItem('Práctica 2',tabName = 'tab2',icon = icon('car'))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = 'tab1',
                fluidRow(
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
        ),
        tabItem(tabName = 'tab2',
                fluidRow(
                  plotOutput('grafica2',click = 'clickGrafica',dblclick = 'dobleclick',hover = 'hvrclick'),
                  tabsetPanel(type = "pills",
                              tabPanel("Click",tableOutput("tabla")),
                              tabPanel("Doble Click",tableOutput("tabledoble")),
                              tabPanel("Hover",tableOutput("tablehover")))
                )
                )
      )
    )
  )
})

server <- function(input,output){
  output$grafica2<-renderPlot({
    ggplot(mpg)+geom_point(aes(x=displ,y=cyl))
  })
  
  output$tabla <- renderTable({
    nearPoints(mpg,input$clickGrafica,maxpoints = 5)
  })
  
  output$tabledoble <- renderTable({
    nearPoints(mpg,input$dobleclick,maxpoints = 5)
  })
  
  output$tablehover <- renderTable({
    nearPoints(mpg,input$hvrclick,maxpoints = 5)
  })
  

   misDatos <- reactiveValues(muestra = NULL)
  
   observe({
     input$norm 
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

shinyApp(ui,server)