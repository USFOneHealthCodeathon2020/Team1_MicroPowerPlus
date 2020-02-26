library(shiny)
library(ggplot2)

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
      plotOutput("plot1"),
      plotOutput("plot2"),
      
    )
  )
)

server <- function(input, output, session) {
  
  output$plot1 <- renderPlot({
    effectsize <- matrix(rexp(input$power, rate=.1))
    power <- matrix( c(1,2,3,4))
    barplot(as.integer(effectsize, power),
            main = "Power (%Probability you can detect diff)",
            ylab="power",
            xlab="microbes",
            col = "blue")
  
  })
  
  output$plot2 <- renderPlot({
    effectsize <- matrix(rexp(input$eSize, rate=.1))
    power <- matrix( c(1,2,3,4))
    barplot(as.integer(effectsize, power),
            ylab="effect size",
            xlab="microbes",
            main="Effect Size (How big would diff have to be?)",
            col = "red")
    
  })

}

shinyApp(ui, server)
