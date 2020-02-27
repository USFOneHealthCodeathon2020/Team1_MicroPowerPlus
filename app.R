library(shiny)
library(plotly)
library(tidyverse)
df_sim_data <- read_csv("human_gut_power_simulation_results.csv")

ui <- fluidPage(h1("Team1"),
                tabsetPanel(
                  tabPanel(
                    "Dashboard",
                    fluid = TRUE,
                    br(),
                    sidebarLayout(
                      sidebarPanel(
                        selectInput(
                          "sampleType",
                          "Choose a Sample Type:",
                          c(
                            "Human Gut" = "Human",
                            "Oral" = "oral",
                            "Lungs" = "lung",
                            "Vagina" = "vagina"
                          )
                        ),
                        selectInput(
                          "sampleSize",
                          "Choose a Sample Size:",
                          c(
                            # "Three" = "3",
                            "Five" = "5",
                            "Ten" = "10",
                            "Fifteen" = "15",
                            "Twenty" = "20"
                          )
                        ),
                        sliderInput(
                          "power",
                          "Choose Power:",
                          min = 0,
                          max = 1,
                          value = 0.5
                        ),
                        # sliderInput(
                        #   "eSize",
                        #   "Choose Effect Size:",
                        #   min = 0,
                        #   max = 10,
                        #   value = 5
                        # )
                        
                      ),
                      mainPanel(# plotlyOutput("plot1"),
                        #         br(),
                        plotlyOutput("plot2"))
                    )
                  ),
                  tabPanel("Literature",  fluid = TRUE,
                           br(),
                           h3("Literature coming soon"), ),
                  tabPanel("Estimate Power",  fluid = TRUE,
                           br(),
                           h3("Estimate Power coming soon"), ),
                  tabPanel(
                    "Estimate Effect Size",
                    fluid = TRUE,
                    br(),
                    h3("Estimate Effect Size coming soon"),
                  )
                ))


server <- function(input, output, session) {
  # output$plot1 <- renderPlotly(
  #   plot3 <- plot_ly(
  #     x = c(matrix(rexp(
  #       input$power, rate = .1
  #     ))),
  #     y = c(matrix(rexp(
  #       input$power, rate = .1
  #     ))),
  #     name = "Power (%Probability you can detect diff)",
  #     type = "bar"
  #   ) %>% layout(
  #     title = "Power (%Probability you can detect diff)",
  #     yaxis = list(title = "power"),
  #     xaxis = list(title = "microbes")
  #   )
  # )
  effect_size <- reactive({
    get_effect_size_from_sample_size_and_power(df_sim_data, input$sampleSize, input$power)
  })
  
  output$plot2 <- renderPlotly(
    plot3 <- plot_ly(
      y = c(0.023,0.024, 0, 0.099, 0.019, 0.230, effect_size()),
      x = factor(c("Nares Smoker vs NonSmoker", 
            "Oral Smoker vs NonSmoker", 
            "Gut Before vs After Feeding", 
            " Oral Azithromycin vs No Azithromycin", 
            "Lung Azithromycin vs No Azithromycin", 
            "Human Anterior Nares vs Stool", 
            input$sampleType),c(input$sampleType,"Nares Smoker vs NonSmoker", 
                                "Oral Smoker vs NonSmoker", 
                                "Gut Before vs After Feeding", 
                                " Oral Azithromycin vs No Azithromycin", 
                                "Lung Azithromycin vs No Azithromycin", 
                                "Human Anterior Nares vs Stool")),
      name = "Effect Size (How big would diff have to be?)",
      type = "bar"
    ) %>% layout(
      title = "Effect Size (How big would diff have to be?)",
      height = 500,
      yaxis = list(title = "effect size"),
      xaxis = list(title = "microbes")
    )
  )
}


calculate_effect_size_model_for_sample_size <-
  function(df, sample_size) {
    ###Calculate a model to predict the effect size given power
    bp_model <- df %>% filter(Sample_Size == sample_size)
    #bp_model <- subset(bp, power < 0.95 & power > 0.2)
    bp_model <-
      data.frame(log_omega2 = log10(bp_model[["simulated_omega2"]]),
                 log_power = log10(bp_model[["power"]]))
    bp_model <- subset(bp_model, log_omega2 > -Inf)
    View(bp_model)
    bp_lm <- lm(log_omega2 ~ log_power, data = bp_model)
    return(bp_lm)
  }

get_effect_size_from_power <- function(model, power) {
  effect_size <-
    10 ^ predict(model, newdata = data.frame(log_power = log10(power)))
  return(effect_size)
}


get_effect_size_from_sample_size_and_power <-
  function(df_sim_data, sample_size, power) {
    model <-
      calculate_effect_size_model_for_sample_size(df_sim_data, sample_size)
    effect_size <- get_effect_size_from_power(model, power)
    return(effect_size)
  }


shinyApp(ui, server)
