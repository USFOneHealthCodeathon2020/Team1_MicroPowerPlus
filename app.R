library(shiny)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  h1("Team 1"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sampleType", "Choose a Sample Type:",
                  c("Human Gut" = "gut",
                    "Oral" = "oral",
                    "Lungs" = "lung",
                    "Vagina" = "vagina"
                    )),
      selectInput("sampleSize", "Choose a sample size:",
                  c("Three" = "3",
                    "Five" = "5",
                    "Ten" = "10",
                    "Fifteen" = "15",
                    "Twnety" = "20"
                  )),
      sliderInput("power", "Choose Power:",
                  min = 0, max = 100, value = 50
      ),
      sliderInput("eSize", "Choose Effect Size:",
                  min = 0, max = 10, value = 5
      )
     
    ),
    mainPanel(
      plotlyOutput("plot1"),
      plotlyOutput("plot2")
    )
  )
)

server <- function(input, output, session) {
  
  # output$plot1 <- renderPlot({
  #   effectsize <- matrix(rexp(input$power, rate=.1))
  #   power <- matrix( c(1,2,3,4))
  #   barplot(as.integer(effectsize, power),
  #           main = "Power (%Probability you can detect diff)",
  #           ylab="power",
  #           xlab="microbes",
  #           col = "blue")
  # 
  # })
  
  output$plot1 <- renderPlotly(
    plot3 <- plot_ly(
      x = c(matrix(rexp(input$power, rate=.1))),
      y = c(matrix(rexp(input$power, rate=.1))),
      name = "Power (%Probability you can detect diff)",
      type = "bar"
    ) %>% layout(title="Power (%Probability you can detect diff)", yaxis=list(title="power"), xaxis=list(title="microbes"))
  )
 
  # output$plot2 <- renderPlot({
  #   effectsize <- matrix(rexp(input$eSize, rate=.1))
  #   power <- matrix( c(1,2,3,4))
  #   barplot(as.integer(effectsize, power),
  #           ylab="effect size",
  #           xlab="microbes",
  #           main="Effect Size (How big would diff have to be?)",
  #           col = "red")
  #   
  # })
  
  output$plot2 <- renderPlotly(
    plot3 <- plot_ly(
      x = c(matrix(rexp(input$eSize, rate=.1))),
      y = c(matrix(rexp(input$eSize, rate=.1))),
      name = "Effect Size (How big would diff have to be?)",
      type = "bar"
    ) %>% layout(title="Effect Size (How big would diff have to be?)", yaxis=list(title="effect size"), xaxis=list(title="microbes"))
  )

}

shinyApp(ui, server)
