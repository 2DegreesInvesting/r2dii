#' Run the app
#'
#' @return Called for its side effect.
#' @export
#'
#' @examples
#' if (interactive()) run_app()
run_app <- function() {
  local_options(list(readr.show_col_types = FALSE))

  ui <- navbarPage(
    theme = shinytheme("flatly"),
    a(
      "r2dii",
      href = "https://rmi-pacta.github.io/r2dii.analysis/articles/r2dii-analysis.html"
    ),
    tabPanel(
      "Match",
      h2("Inputs"),
      fileInput("loanbook", "Bank data", accept = ".csv"),
      DTOutput("loanbook"),
      fileInput("companies", "2DII data", accept = ".csv"),
      DTOutput("companies"),
      h2("Output"),
      strong("Matched"),
      DTOutput("matched"),
      downloadButton("to_validate", "Save in '~/Downloads/matched.csv'")
    ),
    tabPanel(
      "Analyze",
      h2("Inputs"),
      fileInput("validated", "Validated data", accept = ".csv"),
      DTOutput("validated"),
      fileInput("scenarios", "Additional 2DII data (scenario)", accept = ".csv"),
      DTOutput("scenarios"),
      fileInput("regions", "Additional 2DII data (regions)", accept = ".csv"),
      DTOutput("regions"),
      h2("Output"),
      strong("Analyzed"),
      DTOutput("market_share"),
      downloadButton("analyzed", "Save in '~/Downloads/analyzed.csv'")
    ),
    tabPanel(
      "Visualize",
      selectInput(
        "sector",
        "Plot",
        choices = c("automotive", "oil and gas", "power")
      ),
      plotOutput("sector")
    )
  )

  server <- function(input, output, session) {
    loanbook <- reactive({
      req(input$loanbook)
      read_csv(input$loanbook$datapath)
    })
    output$loanbook <- renderDT({
      loanbook()
    })
    companies <- reactive({
      req(input$companies)
      read_csv(input$companies$datapath)
    })
    output$companies <- renderDT({
      companies()
    })
    matched <- reactive({
      req(input$companies)
      match_name(loanbook(), companies())
    })
    output$matched <- renderDT({
      matched()
    })
    output$to_validate <- downloadHandler(
      filename = function() {
        "matched.csv"
      },
      content = function(file) {
        write_csv(matched(), file)
      }
    )

    validated <- reactive({
      req(input$validated)
      prioritize(read_csv(input$validated$datapath))
    })
    output$validated <- renderDT({
      validated()
    })
    scenarios <- reactive({
      req(input$scenarios)
      read_csv(input$scenarios$datapath)
    })
    output$scenarios <- renderDT({
      scenarios()
    })
    regions <- reactive({
      req(input$regions)
      read_csv(input$regions$datapath)
    })
    output$regions <- renderDT({
      regions()
    })
    market_share <- reactive({
      req(input$regions)
      target_market_share(validated(), companies(), scenarios(), regions())
    })
    output$market_share <- renderDT({
      market_share()
    })
    output$analyzed <- downloadHandler(
      filename = function() {
        "analyzed.csv"
      },
      content = function(file) {
        write_csv(market_share(), file)
      }
    )

    output$sector <- renderPlot({
      qplot_techmix(
        filter(
          market_share(),
          .data$scenario_source == "demo_2020",
          .data$sector == input$sector,
          .data$region == "global",
          .data$metric %in% c("projected", "corporate_economy", "target_sds")
        )
      )
    })
  }

  shinyApp(ui, server)
}
