library(shiny)
shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput(
          'variablex',label = 'Eje horizontal',
          choices = list('Cylinders'='cyl','Miles per gallon'='hwy','Displacement'='displ')
          ),
        selectInput(
          'variabley',label = 'Eje vertical',
          choices = list('Cylinders'='cyl','Miles per gallon'='hwy','Displacement'='displ')
        ),
        checkboxInput('facet',label = 'División',value = F),
        checkboxInput('color',label = 'Colorear por tipo',value = F)
      ),
      mainPanel(
        textOutput('texto'),
        plotOutput('grafica')
      )
    )
  )
)