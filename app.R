library(shiny)
library(plotly)
library(tidyverse)
library(shinythemes)

# increase max R-Shiny user-input file size from 5 to 30 MB
options(shiny.maxRequestSize = 30 * 1024 ^ 2)

# read in the static example-data
df_sim_data <- read_csv("human_gut_power_simulation_results.csv")

# the ui object has all the information for the user-interface
ui <- fluidPage(
  h1("Team1 PublixPower"),
  theme = shinytheme("sandstone"),
  tabsetPanel(
    # make the main tab
    tabPanel(
      "Estimate effect size and power",
      fluid = TRUE,
      br(),
      sidebarLayout(
        sidebarPanel(
          # Input: select Human microbiome-site for example-data
          selectInput(
            "sampleType",
            "Choose a Sample Type:",
            c(
              "Human Gut" = "gut",
              "Oral" = "oral",
              "Lungs" = "lung",
              "Vagina" = "vagina"
            )
          ),
          
          # Input: choose a pre-computed sample-size (up to 20 for proof-of-concept; may add more later)
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
          
          # Input: select a distance-measure to use to calculate effect-size (?)--doesn't work yet, just a concept
          selectInput(
            "mdistance",
            "Choose a Distance Measure:",
            c("Weighted Jaccard" = "wjac",
              "Wegihted Unifrac" = "wfrac")
          ),
          
          # Input: Slider to select desired power-level
          sliderInput(
            "power",
            "Choose desired power:",
            min = 0,
            max = 1,
            value = 0.8
          ),
          h4("Reference Data Sources"),
          
          p(
            a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3004851/", "1) Charlson et al. (2010)"),
            "Microbiota from the right and left nasopharynxand oropharynx of 29 smoking vs 33 non-smoking healthy adults were compared to determine the microbial configuration and effects of cigarette-smoking.",
            p(
              a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3480531/", "2) Charlson et al. (2012)"),
              "Microbial populations found within the respiratory tract of transplant-patients were compared to non-transplanted control-subjects. Lung-transplant patients had a higher bacterial burden in the Broncho alveolarlavage, a more frequent showing of dominant organisms, an increased distance between communities (signifying more distinct populations), and a smaller respiratory-tract microbial diversity.",
              p(
                a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3564958/", "3) HMP Consortium (2012b)"),
                "Microbiome samples from 18 body sites of 242 healthy, Western adults were compared to describe individual variation.",
                p(
                  a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3368382/", "4) Wu et al. (2011)"),
                  "The effect size that was studied inthis experiment was fecal communities that grouped into enterotypes characterized by various levels of Bacteroidesand Prevotella.It was deduced that alternative enterotype states are associated with a long-term diet."
                )
              )
            )
          )
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
      "Use your own Data",
      fluid = TRUE,
      br(),
      
      # Input: option for user to upload their own OTU/ASV-table (only a concept--not functional yet)
      fileInput("user.otu", "Pilot-study OTU or ASV-table", placeholder = "Or select example data-set below"),
      h4("Sample Size"),
      fluidRow(column(3,      numericInput(
        "exp",
        p("Experimental"),
        value = 1
      )),
      column(3,      numericInput(
        "control",
        p("Control"),
        value = 1
      ))),
      fluidRow(column(
        3, textInput("email", h4("Email"),
                     value = "Enter email..")
      )),
      p("The results will be emailed to you..")
      
    ),
    tabPanel(
      "Calculate Effect Size",
      fluid = TRUE,
      br(),
      a(href = "https://academic.oup.com/bioinformatics/article/31/15/2461/188732#26918939", "Data-sources and example effect-size calculations"),
      
      # This "parameter glossary" isn't strictly useful on its own without Sherry's markdown tutorial
      h4("Parameter Glossary"),
      p(
        "“adonis” is	a	function	for	the	analysis	and	partitioning	sums	of	squares	using	semimetric
and	metric	distance	matrices."
      ),
      p(
        "Null	hypothesis :	There	is	no	different	between	these	two	or	more	comparable	groups."
      ),
      p(
        "R-square is	the	important	statistic	for	interpreting	Adonis	as	it	gives	you	the	effect	size.
(For	example:	an	R-squared	of	0.44	means	that	44%	of	the	variation	in	distances	is	explained
by	the	grouping	being	tested.	The	p-value	tells	you	whether	or	not	this	result	was	likely	a
result	of	chance.	A	p-value	of	0.05	means	that	there	is	a	5%	chance	that	you	detected	a
difference	between	groups.)",
        p(
          "Small	p-value	with	small	R-square :	this	situation	normally	because	of	large	sample	size.
Actualy	only	small	part	can	be	explained,	however	large	sample	size	make	the	p-value
small."
        ),
        p(
          "Omega-squared provides	a	less	biased	measure	of	effect	size	for	ANOVA-type
analyses	by	accounting	for	the	mean-squared	error	of	the	observed	samples."
        ),
        sidebarLayout(
          sidebarPanel(
            # Input: option for user to upload their own OTU/ASV-table (only a concept--not functional yet)
            fileInput("user.otu", "Load File", placeholder = "Or select example data-set below"),
            h4("Sample Size"),
            fluidRow(column(
              6,      textInput("expName",
                                p("Experimental Name"),
                                value = "Experimental Name")
            ),
            column(6
              ,      textInput("controlName",
                                p("Control Name"),
                                value = "Control Name")
            )),
            h4("Reference Data Sources"),
            
            p(
              a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3004851/", "1) Charlson et al. (2010)"),
              "Microbiota from the right and left nasopharynxand oropharynx of 29 smoking vs 33 non-smoking healthy adults were compared to determine the microbial configuration and effects of cigarette-smoking.",
              p(
                a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3480531/", "2) Charlson et al. (2012)"),
                "Microbial populations found within the respiratory tract of transplant-patients were compared to non-transplanted control-subjects. Lung-transplant patients had a higher bacterial burden in the Broncho alveolarlavage, a more frequent showing of dominant organisms, an increased distance between communities (signifying more distinct populations), and a smaller respiratory-tract microbial diversity.",
                p(
                  a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3564958/", "3) HMP Consortium (2012b)"),
                  "Microbiome samples from 18 body sites of 242 healthy, Western adults were compared to describe individual variation.",
                  p(
                    a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3368382/", "4) Wu et al. (2011)"),
                    "The effect size that was studied inthis experiment was fecal communities that grouped into enterotypes characterized by various levels of Bacteroidesand Prevotella.It was deduced that alternative enterotype states are associated with a long-term diet."
                  )
                )
              )
            )
          ),
          mainPanel(
            plotlyOutput("plot3")
          )
        )
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
    tabPanel(
      "Markdown",
      fluid = TRUE,
      br(),
      
      )
  )
)

# define fonts for plot
f1 <- list(family = "Arial, sans-serif",
           size = 24,
           color = "black")

f2 <- list(family = "Arial, sans-serif",
           size = 20,
           color = "black")

f3 <- list(family = "Arial, sans-serif",
           size = 16,
           color = "black")

# set plot margins
m <- list(
  l = 20,
  r = 20,
  b = 10,
  t = 100,
  pad = 4
)


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
                                               input$sampleType,
                                               input$mdistance)
  })
  
  output$plot2 <- renderPlotly(
    plot3 <- plot_ly(
      y = c(effect_size(), 0.099, 0.019, 0.023, 0.024, 0, 0.230),
      x = c(
        "Estimated Effect Size",
        "Oral: Azithromycin vs No Azithromycin<sup>2</sup>",
        "Lung: Azithromycin vs No Azithromycin<sup>2</sup>",
        "Nares: Smoker vs NonSmoker<sup>1</sup>",
        "Oral: Smoker vs NonSmoker<sup>1</sup>",
        "Gut: Before vs After Feeding<sup>3</sup>",
        "Human Anterior Nares vs Stool<sup>4</sup>"
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
      name = "Effect Size (How big would difference have to be?)",
      type = "bar",
      height = 700,
      marker = list(
        color = c(
          'rgba(222,45,38,0.8)',
          'rgba(204,204,204,1)',
          'rgba(204,204,204,1)',
          'rgba(204,204,204,1)',
          'rgba(204,204,204,1)',
          'rgba(204,204,204,1)',
          'rgba(204,204,204,1)'
        )
      )
      # color = c(" blue, blue, blue, blue, blue, blue"),
    ) %>% layout(
      title = paste(
        "Effect Size<br>(How big would the difference\nbetween groups have to be to be detected?)"
      ),
      margin = m,
      titlefont = f1,
      yaxis = list(
        title = "Effect size",
        titlefont = f1,
        showticklabels = TRUE,
        tickfont = f3,
        range = c(0, 0.3)
      ),
      xaxis = list(
        title = "Microbiome data-set",
        titlefont = f2,
        showticklabels = TRUE,
        tickangle = 45,
        tickfont = f3,
        categoryarray = c(
          "Estimated Effect Size",
          "Oral: Azithromycin vs No Azithromycin<sup>2</sup>",
          "Lung: Azithromycin vs No Azithromycin<sup>2</sup>",
          "Nares: Smoker vs NonSmoker<sup>1</sup>",
          "Oral: Smoker vs NonSmoker<sup>1</sup>",
          "Gut: Before vs After Feeding<sup>3</sup>",
          "Human Anterior Nares vs Stool<sup>4</sup>"
        ),
        categoryorder = "array"
      )
    )
  )
  
  output$plot3 <- renderPlotly(
    plot4 <- plot_ly(
      y = c(0.03, 0.099, 0.019, 0.023, 0.024, 0, 0.230),
      x = c(
        "Estimated Effect Size",
        "Oral: Azithromycin vs No Azithromycin<sup>2</sup>",
        "Lung: Azithromycin vs No Azithromycin<sup>2</sup>",
        "Nares: Smoker vs NonSmoker<sup>1</sup>",
        "Oral: Smoker vs NonSmoker<sup>1</sup>",
        "Gut: Before vs After Feeding<sup>3</sup>",
        "Human Anterior Nares vs Stool<sup>4</sup>"
      ),
      text = c(
       '0.03',
        '0.099',
        '0.019',
        '0.023',
        '0.024',
        '0',
        '0.230'
      ),
      textposition = 'auto',
      name = "Calculated Effect Size compared to effect sizes reported in the literature",
      type = "bar",
      height = 700,
      marker = list(
        color = c(
          'rgba(222,45,38,0.8)',
          'rgba(204,204,204,1)',
          'rgba(204,204,204,1)',
          'rgba(204,204,204,1)',
          'rgba(204,204,204,1)',
          'rgba(204,204,204,1)',
          'rgba(204,204,204,1)'
        )
      )
      # color = c(" blue, blue, blue, blue, blue, blue"),
    ) %>% layout(
      title = paste(
        "Calculated Effect Size compared to \neffect sizes reported in the literature"
      ),
      margin = m,
      titlefont = f1,
      yaxis = list(
        title = "Effect size",
        titlefont = f1,
        showticklabels = TRUE,
        tickfont = f3,
        range = c(0, 0.3)
      ),
      xaxis = list(
        title = "Microbiome data-set",
        titlefont = f2,
        showticklabels = TRUE,
        tickangle = 45,
        tickfont = f3,
        categoryarray = c(
          "Estimated Effect Size",
          "Oral: Azithromycin vs No Azithromycin<sup>2</sup>",
          "Lung: Azithromycin vs No Azithromycin<sup>2</sup>",
          "Nares: Smoker vs NonSmoker<sup>1</sup>",
          "Oral: Smoker vs NonSmoker<sup>1</sup>",
          "Gut: Before vs After Feeding<sup>3</sup>",
          "Human Anterior Nares vs Stool<sup>4</sup>"
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
           sample_type,
           mdistance) {
    model <-
      calculate_effect_size_model_for_sample_size(df_sim_data, sample_size)
    effect_size <- get_effect_size_from_power(model, power)
    return(effect_size)
  }


shinyApp(ui, server)