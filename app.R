library(shiny)
library(readxl)
library(xgboost)
library(xeMetron)
library(bslib)
library(dplyr)

# ---- Themes ----
light_theme <- bs_theme(version = 5, bootswatch = "flatly")
dark_theme  <- bs_theme(version = 5, bootswatch = "darkly")
# ---- UI ----
ui <- navbarPage(
  title = div(),
  windowTitle = "xeMetron App",
  id = "main_nav",
  theme = light_theme,

  header = div(
    style = "
      display: flex;
      align-items: center;
      justify-content: space-between;
      padding: 12px 20px;
    ",
    # div(
    #   style = "display: flex; align-items: center; gap: 10px;",
    #   img(
    #     src = "svgviewer-output.svg",
    #     style = "height: 60px; width: auto;"
    #   )
    # ),
    # tags$div(
    #   style = "display:flex; align-items:center; height: 260px; width: auto;",
    #   HTML(paste0(readLines("www/svgviewer-output2.svg"), collapse = ""))
    # ),
    uiOutput("darkModeBtn"),
  ),

  # ---- Upload Tab ----
  tabPanel(
    "Upload & Settings",
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Upload CSV or XLSX",
                  accept = c(".csv", ".xlsx")),

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

  # ---- Results Tab ----
  tabPanel(
    "Results",
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

# ---- Server ----
server <- function(input, output, session) {

  xeObj <- reactiveVal(NULL)

  # ---- Track current theme ----
  current_theme <- reactiveVal("light")

  # ---- Dark mode button ----
  output$darkModeBtn <- renderUI({
    label <- if (current_theme() == "light") "Dark Mode" else "Light Mode"
    actionButton("toggleTheme", label)
  })

  # ---- Theme toggle observer ----
  observeEvent(input$toggleTheme, {
    if (current_theme() == "light") {
      session$setCurrentTheme(dark_theme)
      current_theme("dark")
    } else {
      session$setCurrentTheme(light_theme)
      current_theme("light")
    }
  })

  # ---- Process Button ----
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

  # ---- Plot ----
  output$xePlot <- renderPlot({
    req(xeObj())
    plot(xeObj())
  })

  # ---- Downloads ----
  output$downloadPNG <- downloadHandler(
    filename = "xeMetron_Plot.png",
    content = function(file) {
      req(xeObj())
      png(file, width = 12, height = 4, units = "in", res = 500)
      plot(xeObj())
      dev.off()
    }
  )

  output$downloadHTML <- downloadHandler(
    filename = "xeMetron_Report.html",
    content = function(file) {
      req(xeObj())
      withProgress(message = "Rendering HTML...", value = 0, {
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
      withProgress(message = "Rendering PDF...", value = 0, {
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

# ---- Run App ----
shinyApp(ui, server)