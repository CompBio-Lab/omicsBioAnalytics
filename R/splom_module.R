#' UI of Scatterplot matrix of Principal components
#'
#' @param id, character used to specify namespace,
#' see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI element
#' @export
splom_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::plotOutput(ns("pca_plot"), width = "100%")
}

#' Scatterplot matrix module server-side processing (no extra deps)
#' @export
splom_server <- function(input, output, session, pcs, response, group_colors) {

  png_path <- file.path(tempdir(), "pca_splom.png")
  grDevices::png(png_path, width = 1600, height = 1200, res = 150, bg = "white")
  omicsBioAnalytics::pcaPairs(
    pcs = pcs,
    y   = response,
    col = group_colors[1:nlevels(response)]
  )
  grDevices::dev.off()

  output$pca_plot <- shiny::renderPlot({
    omicsBioAnalytics::pcaPairs(
      pcs = pcs,
      y   = response,
      col = group_colors[1:nlevels(response)]
    )
  })
}
