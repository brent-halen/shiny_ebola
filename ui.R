#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

source("./global.R", local = TRUE)

if(!"shiny" %in% rownames(installed.packages())){install.packages("shiny", dep = T, repos = "https://cran.revolutionanalytics.com/")}
library(shiny)
if(!"shinydashboard" %in% rownames(installed.packages())){install.packages("shinydashboard", dep = T, repos = "https://cran.revolutionanalytics.com/")}
library(shinydashboard)
if(!"shinyjs" %in% rownames(installed.packages())){install.packages("shinyjs", dep = T, repos = "https://cran.revolutionanalytics.com/")}
library(shinyjs)
if(!"plotly" %in% rownames(installed.packages())){install.packages("plotly", dep = T, repos = "https://cran.revolutionanalytics.com/")}
library(plotly)
if(!"rhandsontable" %in% rownames(installed.packages())){install.packages("rhandsontable", dep = T, repos="https://cran.revolutionanalytics.com/")}
library(rhandsontable)



header <- dashboardHeader(title = "Ebola Plot Generator")

sidebar <- dashboardSidebar(
  tags$head(tags$style(HTML(
    '.myClass { 
    font-size: 20px;
    line-height: 50px;
    text-align: left;
    font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
    padding: 0 15px;
    overflow: hidden;
    color: STEELBLUE;
    }
    '))),
  sidebarMenu(id = "display_tabs",
              menuItem("Main Page", tabName = "mainPage"),
              conditionalPanel("input.display_tabs ==='mainPage'", 
                               class = "shiny-input-container"
                               ,actionButton("updateDat", "Update Data")
                               ,downloadButton("downloadDat", "Download data"))),br(),br(),br(),"Based on Automagic epi curve plotting: part I by Chris Von Scefalvay"
)

body <- dashboardBody(
  useShinyjs(),
  tabItems(
    tabItem(tabName = "mainPage", fluidPage(
      fluidRow(
        
                plotOutput('mainPlot')
             
      ),
      br()
      ,br()
      ,fluidRow(
        
        plotOutput('mainPlot_2')
        
      ),
      br()
      ,br()
      ,fluidRow(
        
                rHandsontableOutput("mainDat")
             
      )
    ))
  )
)

dashboardPage(
  header,
  sidebar,
  body
)