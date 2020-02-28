# Team1_PublixPower
**Team-leads:** Jenna Oberstaller and Justin Gibbons

**Members:**
 
 * Samira Jahangiri, Francesca Prieto, Vyoma Sheth, Omkar Dokur, Xiangyun (Sherry) Liao

# Microbiome power-calculation tool for biologists: towards rigorous, reproducible microbiome study-design


## Rationale:

Statistical Power Calculations aid in figuring out the sample size in order to determine the probability of
acquiring significant results from a statistical test when an effect is present. It is difficult to determine
a correlational relationship between certain taxa and disease. These differences can stem from differing
definitions of what a clinical population signifies within different studies, how to handle sample preparation,
and the overall bioinformatics and statistical tools (Debelius, et al., 2016). Determining effect size is of crucial
importance to aid in determining the differences within community profiling. Effect size is the quantitative
portion of the differences between two or more groups. For example, power and sample-size estimation along
with PERMANOVA has been utilized in order to ensure that effect expected from the interference of interest
is detectable (Kelly, et al., 2015).

We sought to provide an intuitive power- and effect-size calculator-tool for biologists with limited computational experience.

## Goal:

Provide an intuitive power- and effect-size calculator-tool for biologists with limited computational experience.

## App concept overview

![flowchart](https://github.com/USFOneHealthCodeathon2020/Team1_Publix/blob/master/Flowchart.png)

we used R-Shiny to build a user-interface around reference microbiome-data OTU-tables from a variety of human body-sites on which we pre-computed statistical power and its relationship to sample-size using the microPower package (Kelly et al., 2015). 

We added additional functionality to compute effect-size from these data-sets using linear modeling.

## Running our App

The following packages are required to run PublixPower:

RStudio

R-packages [available from CRAN repository]():
 * library(shiny)
 * library(plotly)
 * library(tidyverse)

Download and open the "app.R" file in RStudio. In the upper-right corner of RStudio, press the "Run App" button.

![RStudio screenshot](https://github.com/USFOneHealthCodeathon2020/Team1_PublixPower/blob/master/img/Rstudio.play.png)



  
  
