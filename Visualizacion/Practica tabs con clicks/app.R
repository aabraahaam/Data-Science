#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
          plotOutput('grafica',click = 'clickGrafica',dblclick = 'dobleclick',hover = 'hvrclick'),
          tabsetPanel(type = "pills",
                    tabPanel("Click",tableOutput("tabla")),
                    tabPanel("Doble Click",tableOutput("tabledoble")),
                    tabPanel("Hover",tableOutput("tablehover")))

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$grafica<-renderPlot({
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

}

# Run the application 
shinyApp(ui = ui, server = server)

