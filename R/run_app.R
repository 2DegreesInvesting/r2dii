#' @export
run_app <- function() {
  library(shiny)
  library(readr, warn.conflicts = FALSE)
  library(dplyr, warn.conflicts = FALSE)
  library(DT)
  library(r2dii.match)
  library(r2dii.analysis)
  library(r2dii.plot)

  options(readr.show_col_types = FALSE)

  ui <- fluidPage(
    h1("Match"),
    h2("Inputs"),
    fileInput("loanbook", "Bank's data", accept = ".csv"),
    DTOutput("loanbook"),
    fileInput("companies", "Delivered data", accept = ".csv"),
    DTOutput("companies"),
    h2("Output"),
    strong("Matched"),
    DTOutput("matched"),
    downloadButton("to_validate", "Save in '~/Downloads/to_validate.csv'"),

    h1("Apply methodology"),
    h2("Inputs"),
    fileInput("validated", "Validated data", accept = ".csv"),
    DTOutput("validated"),
    fileInput("scenarios", "Additional data (scenario)", accept = ".csv"),
    DTOutput("scenarios"),
    fileInput("regions", "Additional data (regions)", accept = ".csv"),
    DTOutput("regions"),
    h2("Output"),
    strong("Table"),
    DTOutput("market_share"),
    downloadButton("result", "Save in '~/Downloads/result.csv'"),
    br(),
    br(),
    p(strong("Plot")),
    selectInput(
      "sector",
      "Sector",
      choices = c("automotive", "oil and gas", "power")
    ),
    plotOutput("sector")

  )

  server <- function(input, output, session) {
    loanbook <- reactive({
      req(input$loanbook)
      read_csv(input$loanbook$datapath)
    })
    output$loanbook <- renderDT({loanbook()})
    companies <- reactive({
      req(input$companies)
      read_csv(input$companies$datapath)
    })
    output$companies <- renderDT({companies()})
    matched <- reactive({
      req(input$companies)
      match_name(loanbook(), companies())
    })
    output$matched <- renderDT({matched()})
    output$to_validate <- downloadHandler(
      filename = function() {"to_validate.csv"},
      content = function(file) {write_csv(matched(), file)}
    )

    validated <- reactive({
      req(input$validated)
      prioritize(read_csv(input$validated$datapath))
    })
    output$validated <- renderDT({validated()})
    scenarios <- reactive({
      req(input$scenarios)
      read_csv(input$scenarios$datapath)
    })
    output$scenarios <- renderDT({scenarios()})
    regions <- reactive({
      req(input$regions)
      read_csv(input$regions$datapath)
    })
    output$regions <- renderDT({regions()})
    market_share <- reactive({
      req(input$regions)
      target_market_share(validated(), companies(), scenarios(), regions())
    })
    output$market_share <- renderDT({market_share()})
    output$result <- downloadHandler(
      filename = function() {"result.csv"},
      content = function(file) {write_csv(market_share(), file)}
    )

    output$sector <- renderPlot({
      qplot_techmix(
        filter(
          market_share(),
          scenario_source == "demo_2020",
          sector == input$sector,
          region == "global",
          metric %in% c("projected", "corporate_economy", "target_sds")
        )
      )
    })
  }

  shinyApp(ui, server)
}