# Shiny app for the demrat package
# https://github.com/galetap/demrat

# Load libraries
library(shiny)
library(tidyverse)
library(readxl)
library(bslib)
library(scales)
library(demrat)

# Server ########################

server <- function(input, output) {

  # TAB #1: READ ME ------------------
  # Example of data in summary format
  output$data_summary <- renderTable({
    BA %>%
      select(Site, Culture, D20_, D5_D20_) %>%
      slice(1:6)
  })

  # Example of data in raw format
  output$data_raw <- renderTable({
    BAraw %>%
      select(Site, Culture, Age_min, Age_max) %>%
      slice(1:100) %>%
      slice_sample(n = 12)
  })


  # TAB #2: LOAD DATA -----------------
  # Load data (summary or raw format)
  df <- reactive({
    req(input$data)
    # Do nothing until data are selected
    inFile <- input$data
    readxl::read_excel(inFile$datapath, 1)
  })

  # Data in summary format (copies of df OR converts from raw format)
  data <- reactive({
    if(input$sample_data){
      BA %>%
        slice(1:4)
    } else {
      if(sum(c("Site", "Culture", "Age_min", "Age_max") %in% names(df()))==4) {
        df() %>%
          demrat::dr()
      } else {
        df()}
    }
  })

  # Download data in summary format
  output$get_summary_data <- renderUI({
    # Download button displays only when data are loaded
    req(data())
    downloadButton("save_summary_data", label = "Save data in summary format")})
  # Download handler
  output$save_summary_data <- downloadHandler(
    filename = function() {"data_summary_format.xlsx"},
    content = function(file) {writexl::write_xlsx(data(), path = file)}
  )

  # Show heading and data (in summary format)
  output$text1 <- renderText({
    req(data())
    if(sum(c("Site", "Culture", "Age_min", "Age_max") %in% names(data()))==4) {
      "Input data (converted to the summary format, for data in the raw format, see below)"
    } else {
      "Input data (in the summary format)"
    }
  })

  # Show data in the summary format
  output$data_sumform <- renderTable({
    data()
  })

  # Show heading and data (in raw format, if exists)
  output$text2 <- renderText({
    req(input$data)
    if(sum(c("Site", "Culture", "Age_min", "Age_max") %in% names(df()))==4)
      paste("Input data (in the raw format)", "\n")
  })

  # Show data in raw format (if exists)
  output$data_rawform <- renderTable({
    if(sum(c("Site", "Culture", "Age_min", "Age_max") %in% names(df()))==4)
      df()
  })



  # TAB #3: OUTPUTS  -----------------

  # Creating list of Sites from data in summary format (format: Site #Culture)
  site_list <- reactive({
    data() %>%
      select(Site, Culture) %>%
      distinct() %>%
      mutate(Site_Culture = paste0(Site, " #", Culture)) %>%
      select(Site_Culture) %>%
      pull()
  })

  # Creating the list of IVs that are included in data (for drop-down menu)
  output$IV <-
    renderUI({
      selectInput(inputId = "IV",
                  label = tags$b("Independent variable (ratio)"),
                  choices = c("All",
                              intersect(c("D5_D20_", "D3_D20_", "D1_D20_", "P"), names(data()))),
                  selected = "All")
    })

  # Selecting Sites checked in CheckBox
  output$sites <-
    renderUI({
      # Select distinct values of Program by Type
      checkboxGroupInput(inputId = "sites",
                         label = tags$b("Select sites"),
                         choices = site_list(),
                         selected = site_list()[1])
    })

  # Show heading and results of diest function (demographic prediction)
  output$text3 <- renderText({
    "Prediction of demographic rates"
  })
  # Subtitle
  output$text4 <- renderText({
    "(It may take longer to see results)"
  })

  # Show prediction results / Data frame with diest result table
  diest_res <- reactive({
    # Computing notice
    req(input$sites)

    input$sites %>%
      as_tibble() %>%
      # Separate checkbox data (Site #Culture) to a tibble
      separate(col = "value", into = c("Site", "Culture"), sep = " #") %>%
      # Select subset of Sites that are checked in checkbox
      left_join(data(), by = c("Site", "Culture")) %>%
      # Prediction with diest
      diest(summary = T, sss = input$sss, samples = input$samples,
            e0_min = input$e0[1], e0_max = input$e0[2],
            growth_min = input$growth[1], growth_max = input$growth[2],
            IV=input$IV, DV=input$DV)
  })

  # Prediction results table
  output$diest <- renderTable({
    diest_res() %>%
      # Filter IV and DV according to user's preference
      {if("All" %in% input$DV)
        .
        else
          filter(., DV %in% input$DV)
      } %>%
      {if("All" %in% input$IV)
        .
        else
          filter(., IV %in% input$IV)
      }
  })

  # Show heading and plot (prediction model) / only if one site is selected
  output$text5 <- renderText({
    if(length(input$sites)==1)
      "Prediction model"
  })

  output$plot_diest <- renderPlot({
    req(input$sites)

    # Helper objects
    # If user selects "All" in IV, select D5_D20_ to allow plot the chart
    IV_plot <-
      ifelse(input$IV=="All", "D5_D20_", input$IV)
    DV_plot <-
      ifelse(input$DV=="All", "Growth", input$DV)
    sampl <- input$samples

    # Plot
    # Selecting checked Site
    input$sites %>%
      as_tibble() %>%
      # Filter Site that is selected in the checkbox
      separate(col = "value", into = c("Site", "Culture"), sep = " #") %>%
      left_join(data(), by = c("Site", "Culture"), multiple="all") %>%
      # plot_diest
      demrat::plot_diest(sss = input$sss, samples = input$samples,
                       e0_min = input$e0[1], e0_max = input$e0[2],
                       growth_min = input$growth[1], growth_max = input$growth[2],
                       IV = IV_plot, DV = DV_plot,
                       base_size = 11)
  })

  # Download prediction results
  output$save_pred <- downloadHandler(
    filename = function() {"demrat_prediction.xlsx"},
    content = function(file) {writexl::write_xlsx(diest_res(), path = file)}
  )
}
