##################################################################################
# This file contains functions for plotting spec files and plate reader output.
##################################################################################


#' Generate a pdf file with a plot of a plate design
#'
#' @param plateName Name of the plate.
#' @param replicate Replicate plate number.
#' @param vWellType A matrix specifying the type of each well ("DATA", "BLANK" or "EMPTY")
#' @param rows A vector of values of the row variable
#' @param columns A vector of values of the column variable
#' @param border Type of border
#' @param fileName File name of for the pdf (including path)
#'
#'@keywords internal
#'
specPlot_fullFact <- function(plateName,
                              replicate,
                              vWellType,
                              rows,
                              columns,
                              border,
                              fileName) {
  plateX <- 0.03
  plateY <- 0.3
  wellWidth = 0.03
  wellHeight = 0.046
  nrowsTotal <- 8
  ncolsTotal <- 12
  labelSize <- 10
  colLabelDist <- 0.02
  rowLabelDist <- 0.01
  legendX <- 0.07
  legendY <- 0.8
  legendWidth <- 0.08
  legendHeight <- wellHeight
  legendXSpace <- 0.02
  colBLANK <- "orange"
  colDATA <- "blue"
  colEMPTY <- "grey"
  if (!is.null(border)) {
    rows <- c("", rows, "")
    columns <- c("", columns, "")
  }

  grDevices::pdf(fileName, width = 11.7, height = 8.3)
  graphics::plot.new()
  for(i in 1:nrowsTotal)
    for(j in 1:ncolsTotal) {
      colour <- switch(vWellType[i,j],
                       BLANK = colBLANK, DATA = colDATA, EMPTY = colEMPTY)
      graphics::rect(plateX + (j - 1) * wellWidth, plateY + (nrowsTotal - i + 1) * wellHeight,
           plateX + j * wellWidth, plateY + (nrowsTotal - i) * wellHeight,
           col = colour)
    }
  for(i in 0:nrowsTotal) {
    graphics::lines(c(plateX, plateX + ncolsTotal * wellWidth),
          rep(plateY + i * wellHeight, 2))
    if(i>0) {
      graphics::text(plateX - rowLabelDist, plateY + (i-0.5) * wellHeight,
           LETTERS[8:1][i], adj = c(1, 0.5))
      graphics::text(plateX + 12*wellWidth + rowLabelDist, plateY + (i-0.5) * wellHeight,
           rows[8:1][i], adj = c(0, 0.5))
    }
  }
  for(i in 0:ncolsTotal) {
    graphics::lines(rep(plateX + i * wellWidth, 2),
          c(plateY, plateY + nrowsTotal * wellHeight))
    if(i>0) {
      graphics::text(plateX + (i-0.5) * wellWidth, plateY - colLabelDist,
           columns[i], adj = c(1, 0.5), srt = 90)
      graphics::text(plateX + (i-0.5) * wellWidth, plateY + 8*wellHeight + colLabelDist,
           i, adj = c(0.5, 0.5))
    }
  }
  # titles:
  graphics::text(0, 1, plateName, adj = c(0, 1), cex = 2.8)
  if (!is.null(replicate))
    graphics::text(0, 0.91, paste0("Replicate #", replicate), adj = c(0, 1), cex = 1.4)

  graphics::rect(legendX, legendY - legendHeight,
       legendX + legendWidth, legendY, col = colDATA)
  graphics::rect(legendX + legendWidth + legendXSpace, legendY - legendHeight,
       legendX + 2*legendWidth + legendXSpace, legendY, col = colBLANK)
  graphics::rect(legendX + 2*legendWidth + 2*legendXSpace, legendY - legendHeight,
       legendX + 3*legendWidth + 2*legendXSpace, legendY, col = colEMPTY)
  graphics::text(legendX + legendWidth/2, legendY - legendHeight/2,
       "DATA", adj = c(0.5, 0.5))
  graphics::text(legendX + 3*legendWidth/2 + legendXSpace, legendY - legendHeight/2,
       "BLANK", adj = c(0.5, 0.5))
  graphics::text(legendX + 5*legendWidth/2 + 2*legendXSpace, legendY - legendHeight/2,
       "EMPTY", adj = c(0.5, 0.5))
  grDevices::dev.off()
}


#' Plot OD data through time
#'
#' This function produces a plot where OD through time is shown for each well of a 96-well plate.
#'
#' @param data The data to be plotted, as produced by the \code{processODData} function.
#' @param plate A character vector of length 1 indicating which of the (potentially several) plates within the data set should be plotted.
#' @param replicates A vector indicating which replicates should be plotted.
#' @param blanked A boolean value indicating whether the blanked OD should be plotted or not.
#' @param border A boolean value indicating whether or not to include the plate border (rows A and H, columns 1 and 12) in the plot.
#' @param hours A boolean value indicating whether time should be plotted in hours or minutes.
#' @param smallScreen If \code{TRUE}, many of the details in the plot (legend, axes labels etc.) will be omitted so as to fit all the data on a small screen.
#'
#' @return A ggplot object.
#' @export
#'
plotODs <- function(data,
                    plate,
                    replicates = NULL,
                    blanked = TRUE,
                    border = TRUE,
                    hours = TRUE,
                    smallScreen = FALSE) {
  data <- dplyr::filter(data, Plate == plate)
  if (nrow(data) == 0)
    stop("No data available with the plate ID specified.")
  if (!border)
    data <- data |>
      dplyr::filter(!(Row %in% c("A", "H")) & !(Column %in% c(1, 12)))
  if (blanked)
    data <- data |>
      dplyr::mutate(OD = blankedOD)
  if (!is.null(replicates)) {
    data <- data |>
      dplyr::filter(Replicate %in% replicates)
  } else {
    replicates <- sort(unique(data$Replicate))
  }
  if (hours) {
    data <- data |>
      dplyr::mutate(Time = Time_h)
    xLabel <- "Time [h]"
  } else {
    data <- data |>
      dplyr::mutate(Time = Time_min)
    xLabel <- "Time [min]"
  }

  # try to use Wes Anderson colours if package is available, otherwise default ggplot colours:
  if (length(suppressWarnings(find.package('wesanderson', quiet = TRUE, verbose = FALSE))) > 0) {
    colours <- wesanderson::wes_palettes[["Darjeeling1"]][replicates]
  } else {
    colours <- scales::hue_pal()(length(replicates))[replicates]
  }
  p <- ggplot2::ggplot(data) +
    ggplot2::geom_line(ggplot2::aes(Time, OD, col = as.factor(Replicate))) +
    ggplot2::facet_grid(rows = ggplot2::vars(Row), cols = dplyr::vars(Column)) +
    ggplot2::labs(x = xLabel, y = "OD600") +
    ggplot2::theme_bw() +
    ggplot2::scale_colour_manual(name = "Replicate",
                                 values = colours) +
    ggplot2::theme(legend.position = "top")

  if (smallScreen)
    p <- p +
    ggplot2::scale_x_continuous(breaks = NULL) +
    ggplot2::scale_y_continuous(breaks = NULL) +
    ggplot2::theme(legend.position = "none", panel.spacing = grid::unit(0, "cm"),
                   axis.title.x=element_blank(), axis.title.y=element_blank(),
                   axis.text.x=element_blank(), axis.text.y=element_blank(),
                   axis.ticks.x=element_blank(), axis.ticks.y=element_blank())
  return(p)
}



#' A shiny app to explore OD data from a 96-well plate.
#'
#' This shiny app lets you choose replicates and a number of options to plot OD data through time for each well of a 96-well plate.
#'
#' @param data The data to be plotted, as produced by the \code{processODData} function.
#'
#' @return Starts a shiny app.
#' @export
#'
shinyPlate <- function(data) {

  replicates <- sort(unique(data$Replicate))

  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::radioButtons("Plate", "Plate ID:", choices = unique(data$Plate)),
        shiny::checkboxGroupInput("Replicates", "Reps:", choices = replicates, selected = replicates),
        shiny::checkboxGroupInput("Options", "Options:", choices = c("blanked", "border", "hours", "small"), selected = c("blanked", "border", "hours")),
        width = 2
      ),
      shiny::mainPanel(shiny::plotOutput(outputId = "main_plot", height = "600px"), width = 10)
    )
  )

  # shiny server function:

  server <- function(input, output, session) {
    output$main_plot <- shiny::renderPlot({
      selectedReps <- as.integer(input$Replicates)
      plotODs(data,
              plate = input$Plate,
              replicates = selectedReps,
              blanked = "blanked" %in% input$Options,
              border = "border" %in% input$Options,
              hours = "hours" %in% input$Options,
              smallScreen = "small" %in% input$Options)
    })
  }

  return(shiny::shinyApp(ui = ui, server = server))
}


