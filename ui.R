

ui <- dashboardPage(
  dashboardHeader (title = "Canoe Sprint Race Profile", titleWidth = 450),
  dashboardSidebar(
    # fileInput("file1", "Choose files", multiple = TRUE,
    #           accept = c("text/csv",
    #                      "text/comma-separated-values,text/plain",
    #                      ".csv")),
    
    
    selectInput("distance", "Distance:",
                choices = c("200" = "Labelled_data_200",
                            "500" = "Labelled_data_500",
                            "1000" = "Labelled_data_1000")),
    selectInput("Report_Type", "Report Type:",
                c("Single Race" = "Single Race",
                  "Two Races" = "Two Races",
                  "vs Top 10" = "vs Top 10")),    
    uiOutput("select_Class"),
    uiOutput("select_Name"),
    uiOutput("select_Competition"),
    uiOutput("select_Phase"),
    
    uiOutput("select_Name2"),
    uiOutput("select_Competition2"),
    uiOutput("select_Phase2"),
    actionButton("goButton", "View")
  ),
  dashboardBody(
    fluidRow(
      column(6,box(title = textOutput("BoxTitleWBT"), background = "orange", solidHeader = TRUE, width = 12,
                   collapsible = FALSE,
                   dataTableOutput("table_WBT"))),
      column(6,box(title = textOutput("BoxTitleProg"), background = "green", solidHeader = TRUE, width = 12,
                   collapsible = FALSE,
                   dataTableOutput("table_Prog")))
    ),
    fluidRow(
      h3(textOutput("Summaryhead"),align = "center")
    ),
    fluidRow(
      column(3),
      column(6,
             box(title = "Race Summary", status = "primary", solidHeader = TRUE, width = 12,
                 DT::dataTableOutput("RaceSummary"))),
      column(3)
    ),
    
    fluidRow(
      column(6,box(title = textOutput("Plothead1"), status = "primary", solidHeader = TRUE,width = 12,
                   collapsible = TRUE,plotlyOutput("ggplotvel"))),
      column(6,box(title = textOutput("Plothead2"), status = "primary", solidHeader = TRUE,width = 12,
                   collapsible = TRUE,plotlyOutput("ggplotSR")))
    ),
    fluidRow(
      column(6,box(title = "Splits", status = "primary", solidHeader = TRUE, width = 12,
                   collapsible = TRUE,
                   DT::dataTableOutput("SummaryTable"))),
      column(6,box(title = "Pace", status = "primary", solidHeader = TRUE, width = 12,
                   collapsible = TRUE,
                   DT::dataTableOutput("SummaryTablePace")))
    ),
    fluidRow(
      column(6,box(title = "Velocity", status = "primary", solidHeader = TRUE, width = 12,
                   collapsible = TRUE,
                   DT::dataTableOutput("SummaryTableVel"))),
      
      column(6,box(title = "Stroke Rate", status = "primary", solidHeader = TRUE, width = 12,
                   collapsible = TRUE,
                   DT::dataTableOutput("SummaryTableSR")))
    ),
    # fluidRow(
    #   box(title = "25m Split Times", status = "primary", solidHeader = TRUE,width = 12,
    #       collapsible = TRUE,dataTableOutput("SplitTable"))
    # ),
    
    # fluidRow(
    #   box(title = "25m Split Times", status = "primary", solidHeader = TRUE,width = 12,
    #       collapsible = TRUE,plotOutput("ggplotsplit"))
    # ),
    
    
    #    fluidRow(
    #      box(title = "Time vs Top 10 average", status = "primary", solidHeader = TRUE, width = 12,
    #          collapsible = TRUE,
    #          dataTableOutput("table2"))
    #    ),
    #    fluidRow(
    #      box(title = "Time vs Top 10 average", status = "primary", solidHeader = TRUE,width = 12,
    #          collapsible = TRUE,plotOutput("ggplot"))
    #    )
    
    
    
  )
  
)










# Create Shiny app ----
# shinyApp(ui, server)