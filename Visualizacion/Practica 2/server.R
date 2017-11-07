library(plotly)
library(shiny)
function(input,output){
  output$grafica <- renderPlot({
    
    graficobase <- ggplot(mpg,aes_string(x=input$variablex,y=input$variabley)) + 
                   geom_point()
    
    if(input$color & input$facet)
      graficobase + geom_point(aes(color=class)) + facet_wrap(~manufacturer,ncol=4)
    else if (input$color)
      graficobase + geom_point(aes(color=class))
    else if(input$facet)
      graficobase + facet_wrap(~manufacturer,ncol=4)
    else
      graficobase

  })
  output$texto <- renderText({
    paste0('Esta es la gráfica de la variable ',input$variablex, ' y la variable ', input$variabley)
  })
  
}

