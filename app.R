library(shiny)
library(readxl)
library(xgboost)
library(xeMetron)
library(bslib)
library(dplyr)

ui <- navbarPage(
  title = div(img(src = "logo.svg", height = "30px"), " xeMetron App"),
  id = "main_nav",
  theme = bs_theme(bootswatch = "flatly"),

  tabPanel("Upload & Settings",
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Upload CSV or XLSX",
                  accept = c(".csv", ".xlsx")),
        hr(),
        numericInput("censor", "Censor Time:", value = 60, min = 0),
        checkboxInput("dynamicCensoring", "Dynamic Censoring", TRUE),
        checkboxInput("fillMissingTimes", "Fill Missing Times", TRUE),
        checkboxInput("correctDir", "Correct Direction", TRUE),
        checkboxInput("showAllMetrics", "Show all metrics in report", FALSE),
        actionButton("process", "Process Data")
      ),
      mainPanel(
        verbatimTextOutput("uploadStatus")
      )
    )
  ),

  tabPanel("Results",
    sidebarLayout(
      sidebarPanel(
        downloadButton("downloadHTML", "Download HTML Report"),
        br(), br(),
        downloadButton("downloadPDF", "Download PDF Report"),
        br(), br(),
        downloadButton("downloadPNG", "Download Plot (PNG)"),
        br(), br(),
        downloadButton("downloadRDS", "Download xeMetron Object (.rds)")
      ),
      mainPanel(
        plotOutput("xePlot", height = "500px")
      )
    )
  )
)

server <- function(input, output, session) {
  xeObj <- reactiveVal(NULL)

  observeEvent(input$process, {
    req(input$file, input$censor)
    ext <- tools::file_ext(input$file$name)
    df <- switch(ext,
      "csv"  = read.csv(input$file$datapath),
      "xlsx" = read_excel(input$file$datapath),
      stop("Unsupported file type")
    )

    output$uploadStatus <- renderText("Calculating… please wait.")

    obj <- calcXemetronResponse(
      df = as.data.frame(df),
      censor = input$censor,
      dynamicCensoring = input$dynamicCensoring,
      fillMissingTimes = input$fillMissingTimes,
      correctDir = input$correctDir,
      generateReport = FALSE
    )
    xeObj(obj)
    output$uploadStatus <- renderText("✅ Analysis done – switching to Results.")

    updateTabsetPanel(session, "main_nav", selected = "Results")
  })

  output$xePlot <- renderPlot({
    req(xeObj())
    plot(xeObj())
  })

  output$downloadPNG <- downloadHandler(
    filename = "xeMetron_Plot.png",
    content = function(file) {
      req(xeObj())
      png(file, width = 12, height = 4, unit = 'in', res = 500)
      plot(xeObj())
      dev.off()
    }
  )

  output$downloadHTML <- downloadHandler(
    filename = "xeMetron_Report.html",
    content = function(file) {
      req(xeObj())
      withProgress("Rendering HTML...", {
        generateXemetronReport(
          object = xeObj(),
          show_all_metrics = input$showAllMetrics,
          output_file = file
        )
      })
    }
  )

  output$downloadPDF <- downloadHandler(
    filename = "xeMetron_Report.pdf",
    content = function(file) {
      req(xeObj())
      withProgress("Rendering PDF...", {
        generateXemetronReport(
          object = xeObj(),
          show_all_metrics = input$showAllMetrics,
          output_file = file
        )
      })
    }
  )

  output$downloadRDS <- downloadHandler(
    filename = "xeMetron_Object.rds",
    content = function(file) {
      req(xeObj())
      saveRDS(xeObj(), file)
    }
  )
}

shinyApp(ui, server)
