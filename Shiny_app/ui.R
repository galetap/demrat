# Shiny app for the demrat package
# https://github.com/galetap/demrat

# UI ###########################x

ui <- fluidPage(
  # Change font size of the panel
  titlePanel(title = "Prediction of demographic rates from skeletal samples using demrat package"),
  theme = bslib::bs_theme(bg = "white",
                          fg = "black",
                          primary = "maroon", font_scale = 0.8),

  tabsetPanel(
    selected = "Load data",
    tabPanel("Read me", fluid = TRUE,
             mainPanel(
               h3("Data"),
               "The application accepts data in two formats; summary data and raw data format.",
               h5("Summary data format"),
               "The rows of the summary data format contain data for one skeletal sample.",
               "Data must have at least four variables.", br(),
               "1. Site", br(),
               "2. Culture", br(),
               "3. D20_ (i.e., D20+, number of adult skeletons in a sample)", br(),
               "4. One out of the following three age-at-death ratios: D1_D20_, D3_D20_, D5_D20_", br(),
               "If data contains column P, it will be used for prediction using Bocquet-Appel's (2002) original formulas.", br(),
               "The column with the total number of skeletons in a sample (named n) is recommended.", br(), br(),
               div(tableOutput("data_summary"), style = "font-size:90%"),

               h5("Raw data format"),
               "The rows of the raw data format contain data for one skeleton.", br(),
               "Data must have at least four variables.", br(),
               "1. Site", br(),
               "2. Culture", br(),
               "3. Age_min", br(),
               "4. Age_max", br(),
               "Note that the age-at.death intervals are closed on the left and open on the right. It is, interval from 20 to 40 means that individual died between 20 and 39.9 years of age.", br(),
               "If data is loaded in the raw format, they are automatically converted to the summary data format", br(),
               div(tableOutput("data_raw"), style = "font-size:90%"),
               h3("Load data tab"),
               "Data is loaded at 'Load data' tab.", br(),
               "MS Excel file is accepted (.xls or .xlsx). Data table should be on the first sheet of the Excel file.",
               br(), br(),
               h3("Prediction tab"),
               "Growth, crude birth, and total fertility rates are predcited on 'Prediction' tab.", br(),
               "For a single site, the results show a table and plot.", br(),
               "For a multiple sites, the results show only a table.", br(),
               "Characteristics of the prediction model can be set on the left side of the tab.", br(),
               "Estimation for sites with a large number of skeletons and/or estimation for many sites can be slow.", br(),
               br(), br(),
             )
    ),

    # (1) Load data -----------------
    tabPanel("Load data", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 fileInput("data", "Upload a file with data", accept = c(".xls", ".xlsx")),
                 checkboxInput(inputId = "sample_data", label = "Use sample data")
               ),

               mainPanel(
                 tagList(tags$strong(textOutput("text1"))),
                 div(uiOutput("get_summary_data")),
                 br(),
                 div(tableOutput("data_sumform"), style = "font-size:90%"),
                 br(),
                 tagList(tags$strong(textOutput("text2"))),
                 div(tableOutput("data_rawform"), style = "font-size:90%")
               )
             )
    ),

    # (2) Demographic estimation ----------------
    tabPanel("Prediction", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 # ROW #1
                 fluidRow(
                   column(width = 5,
                          uiOutput(outputId = "IV")
                   ),
                   column(width = 5,
                          selectInput(inputId = "DV",
                                      label = tags$b("Predicted variable (demographic rate)"),
                                      choices = c("All", "Growth", "TFR", "CBR"),
                                      selected = "All")
                   )
                 ),
                 # ROW #2
                 fluidRow(
                   column(width = 5,
                          numericInput(inputId = "samples",
                                       label = tags$b("Number of simulated reference samples"),
                                       value = 100, step = 100, min = 100, max = 1000)
                   ),
                   column(width = 5,
                          checkboxInput(inputId = "sss",
                                        label = "Create simulated skeletal samples",
                                        value = TRUE)
                   )
                 ),
                 fluidRow(
                   column(width = 5,
                          sliderInput(inputId = "e0",
                                      label = tags$b("Life expectancy (yr)"),
                                      min = 18, max = 80, value = c(18, 25), step = 1)
                   ),
                   column(width = 5,
                          sliderInput(inputId = "growth",
                                      label = tags$b("Growth rate (%)"),
                                      min = -5, max = 5, value = c(-3, 3), step = 0.1)
                   )
                 ),
                 # checkboxInput("all", "Select All/None", value=TRUE),
                 uiOutput("sites")

               ),

               mainPanel(
                 tagList(tags$b(textOutput("text3"))),
                 tagList(tags$a(textOutput("text4"))),
                 br(),
                 div(downloadButton("save_pred", "Save predictions"), style = "font-size:40%"),
                 tableOutput("diest"),
                 br(),
                 tagList(tags$b(textOutput("text5"))),
                 br(),
                 plotOutput("plot_diest", width = "70%")
               )
             )
    )
  )
)
