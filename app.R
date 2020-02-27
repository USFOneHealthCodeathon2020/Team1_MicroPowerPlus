library(shiny)
library(plotly)
library(tidyverse)
df_sim_data <- read_csv("human_gut_power_simulation_results.csv")

ui <- fluidPage(h1("Team1 PublixPower"),
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
                            "Human Gut" = "human",
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
                          value = 0.8
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
                  tabPanel(
                    "Literature",
                    fluid = TRUE,
                    br(),
                    h4("Importance of Microbiome"),
                    p(
                      "The micobiome is the genetic material found within microbes that reside in organisms.  In recent years, there has been an increase in researching the associations between the microbiome and an organism’s overall wellbeing.  There have been connections between health conditions such as asthma, allergies, autoimmune conditions and other various diseases with the microbiome (Debelius, et al., 2016)",
                      h4("Importance of Statistical Power Calculations within Microbiome"),
                      p(
                        "Statistical Power Calculations aid in figuring out the sample size in order to determine the probability of acquiring significant results from a statistical test when an effect is present.  It is difficult to determine a correlational relationship between certain taxa and disease.  These differences can stem from differing definitions of what a clinical population signifies within different studies, how to handle sample preparation, and the overall bioinformatics and statistical tools (Debelius, et al., 2016). Determining effect size is of crucial importance to aid in determining the differences within community profiling.  Effect size is the quantitative portion of the differences between two or more groups.  For example, power and sample-size estimation along with PERMANOVA has been utilized in order to ensure that effect expected from the interference of interest is detectable (Kelly, et al., 2015)"
                      ),
                      h4("Limitations of Power Analyses"),
                      p(
                        "Power analyses do not generalize favorably, in which if one were to alter the methodology to collect the data or even alter the statistical steps to analyze the data then the power analysis will have to be conducted again. For example, a power analysis could suggest an amount of subjects that is inappropriate for the usual statistical procedure, which will make the gathered data less precise (Statistical Consulting Group, 2020).  Moreover, power analyses give the optimum and best case scenario estimates of the fundamental amount of subjects necessary to detect the effect (Statistical Consulting Group, 2020).  Most of the time these estimates are based on assumptions, and if the assumptions are incorrect then one would have less power. As power analyses are based off assumptions then in turn a range of numbers needed is produced not a precise number, thus reducing accuracy of the experiment.
	Despite the recent technological advancements in statistical testing some software packages do not bear in mind certain factors that affect the power. For example, some packages can recommend differing sample sizes rather than the optimal sample size for a procedure (Statistics Solutions, 2019).  This signifies that power analyses can create overall guidelines for the sample size, but it is unable to indicate the complexities that an experiment could possibly have.
	Even gathering the effect sizes of power calculations can cause unforeseen errors.  Researchers gather effect sizes in either an empirical approach or on the basis of goals approach (Gelman & Carlin, 2014). With the empirical method, one presumes that the effect size is equal to the estimate from a preceding study. The basis of goals approach allows for the researcher to infer the effect size that would be the lowest number that is substantively important.  These approaches can cause studies to be too minute thus leading to a misinterpretation of the findings. (Turner & Houle, 2018).  Some researchers and statistical authorities recommend against utilizing power functions as there is an inappropriate use of these power calculations.  (Gelman & Carlin, 2014).  They believe that effect size and power is usually overestimated and many times subsequent analysis after the completed experiment is used to analyze nonsignificant findings.
"
                      )
                    )
                  ),
                  tabPanel("Estimate Power",  fluid = TRUE,
                           br(),
                           h3("Estimate Power coming soon"),),
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
    get_effect_size_from_sample_size_and_power(df_sim_data,
                                               input$sampleSize,
                                               input$power,
                                               input$sampleType)
  })
  
  output$plot2 <- renderPlotly(
    plot3 <- plot_ly(
      y = c(effect_size(), 0.099, 0.019, 0.023, 0.024, 0, 0.230),
      x = c(
        "Estimated Effect Size",
        "Oral Azithromycin vs No Azithromycin",
        "Lung Azithromycin vs No Azithromycin",
        "Nares Smoker vs NonSmoker",
        "Oral Smoker vs NonSmoker",
        "Gut Before vs After Feeding",
        "Human Anterior Nares vs Stool"
      ),
      text = c(
        round(effect_size(), digits = 3),
        '0.099',
        '0.019',
        '0.023',
        '0.024',
        '0',
        '0.230'
      ),
      textposition = 'auto',
      name = "Effect Size (How big would diff have to be?)",
      type = "bar"
    ) %>% layout(
      title = paste(
        "Effect Size (How big would the difference\nbetween groups have to be to be detected?)"
      ),
      height = 500,
      yaxis = list(title = "effect size"),
      xaxis = list(
        title = "microbes",
        categoryarray = c(
          "Estimated Effect Size",
          "Oral Azithromycin vs No Azithromycin",
          "Lung Azithromycin vs No Azithromycin",
          "Nares Smoker vs NonSmoker",
          "Oral Smoker vs NonSmoker",
          "Gut Before vs After Feeding",
          "Human Anterior Nares vs Stool"
        ),
        categoryorder = "array"
      )
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
  function(df_sim_data,
           sample_size,
           power,
           sample_type) {
    model <-
      calculate_effect_size_model_for_sample_size(df_sim_data, sample_size)
    effect_size <- get_effect_size_from_power(model, power)
    return(effect_size)
  }


shinyApp(ui, server)
