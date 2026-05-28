#' Käynnistä visun Shiny-sovellus
#'
#' @param data Data frame visualisointiin. Oletuksena NULL.
#' @return Shiny-sovellus.
#' @export
run_visu_app <- function(data = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Paketti 'shiny' puuttuu. Asenna se ensin.", call. = FALSE)
  }

  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("visu: StatFi-visualisointi"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::helpText("Anna data run_visu_app-funktiolle paketin sisältä.")
        ),
        shiny::mainPanel(
          shiny::plotOutput("statfi_plot")
        )
      )
    ),
    server = function(input, output, session) {
      output$statfi_plot <- shiny::renderPlot({
        shiny::validate(
          shiny::need(!is.null(data), "Data puuttuu. Anna data parametrina.")
        )

        num_cols <- names(data)[vapply(data, is.numeric, logical(1))]
        if (length(num_cols) == 0L) {
          plot.new()
          title("Datassa ei ole numeerisia sarakkeita.")
          return(invisible(NULL))
        }

        x_col <- names(data)[1]
        y_col <- num_cols[1]
        print(plot_statfi_data(data, x = x_col, y = y_col))
      })
    }
  )
}
