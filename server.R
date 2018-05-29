#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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

# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {
  
  mem_var <- reactiveValues(
    update_dat_flag = 1,
    source_dat = data.table(),
    viz = ggplot(),
    viz2 = ggplot()
  )
  
  observe({if(mem_var$update_dat_flag == 1){
    showModal(modalDialog(
      tile = "Processing",
      br(),
      h3("Processing Data. Please Wait..."),
      br(),
      footer = NULL,
      easyClose = FALSE
    ))
    mem_var$source_dat <- get_drc_dataset()
    mem_var$viz <- plot_case_status_by_health_zone_plot_return(mem_var$source_dat, height = 8)
    mem_var$viz2 <- plot_epi_curve_plot_return(mem_var$source_dat, height = 8)
    mem_var$update_dat_flag <- 0
    removeModal()
  }})

  output$mainPlot <- renderPlot({
    
    # mem_var$viz
    plot_case_status_by_health_zone_plot_return(mem_var$source_dat, height = 8)
    
    
  })
  
  output$mainPlot_2 <- renderPlot({
    
    mem_var$viz2
    
    
  })
  
  output$downloadDat <- downloadHandler(
    filename = function(){
      Sys.time() %>%
        format("%d%H%M%S%b%Y") %>%
        paste("./www/raw_data/drc/", "drc-", ., ".csv", sep = "")
    },
    content = function(file) {
      write.csv(mem_var$source_dat, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$updateDat, {
    mem_var$update_dat_flag <- 1
  })
  
  output$mainDat <- renderRHandsontable({
    rhandsontable(mem_var$source_dat, rowHeaders = FALSE, selectCallback = TRUE, highlightRow = TRUE, readOnly = TRUE)
  })
  
})
