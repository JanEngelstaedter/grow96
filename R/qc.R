

#' Obtain quality control data and plots on temperature
#'
#' @param data The data set, as produced by processODData
#' @param setT The temperature to which the instrument had been set
#'
#' @return A list of two elements: a summary table and a list of plots for each plate
#'
#'@keywords internal
#'
qcTemperature <- function(data) {
  cat("Running quality control analyses...")
  temp <- data %>%
    dplyr::filter(Well == "A1") %>% # T is the same for all wells, so just pick one here
    dplyr::select(Plate, Replicate, SetTemperature, Time_min, Temperature)

  TSummary <- temp %>%
    dplyr::group_by(Plate, Replicate) %>%
    dplyr::summarise(min = min(Temperature),
                     max = max(Temperature),
                     mean = mean(Temperature),
                     maxDev = max(abs(Temperature - SetTemperature)),
                     SetT = mean(SetTemperature), .groups = "drop")

  output <- list(summary = TSummary,
                 individual = vector(mode = "list", length = nrow(TSummary)))

  plates <- unique(temp$Plate)
  counter <- 1
  for(plate in plates) {
    replicates <- unique(temp$Replicate[temp$Plate == plate])
    for(replicate in replicates) {
      toPlot <- temp %>%
        dplyr::filter(Plate == plate, Replicate == replicate) %>%
        tidyr::pivot_longer(c("Temperature", "SetTemperature"),
                      names_to = "ActualOrSet", values_to = "Temperature")

      p <- ggplot2::ggplot(toPlot) +
        ggplot2::geom_line(ggplot2::aes(x=Time_min, y=Temperature, colour = ActualOrSet)) +
        ggplot2::scale_color_manual(values = c("red", "black"), labels = c("Set", "Actual")) +
        ggplot2::labs(x = "Time [min]", y = "Temperature [\u00B0C]", colour = "") +
        ggplot2::ggtitle("Temperature through time") +
        ggplot2::theme_bw()

      output$individual[[counter]]$plate <- plate
      output$individual[[counter]]$replicate <- replicate
      output$individual[[counter]]$plot <- p
      counter <- counter + 1
    }
  }
  return(output)
}


#' Obtain quality control data and plots on blank wells
#'
#' @param data The data set, as produced by processODData
#' @param blankGroups If specified, one or several columns in the \code{data} tibble by which blanking should be grouped. For examples, if there is a variable 'Medium' in the tibble, then with \code{blankGroups = 'Medium'}, averages for blanking will be taken across all wells with \code{wellType="BLANK"} for each value of this column (e.g. "LB", "M9" etc.), and subtracted from OD for data wells with the same Medium values.
#'
#' @return A list of two elements: a summary table and a list of plots for each plate
#'
#' @keywords internal
#'
qcBlanks <- function(data, blankGroups = NULL) {
  blankODs <- data %>%
    dplyr::filter(WellType == "BLANK") %>%
    dplyr::select(c("Plate", "Replicate", "Well", all_of(blankGroups), "Time_min", "OD"))

  blankODMeans <- blankODs %>%
    dplyr::group_by(across(c("Plate", "Replicate", blankGroups, "Time_min"))) %>%
    dplyr::summarise(meanOD = mean(OD), .groups = "drop")

  blankODSummary <- blankODMeans %>%
    dplyr::group_by(across(c("Plate", "Replicate", blankGroups))) %>%
    dplyr::summarise(maxChange = max(abs(meanOD - dplyr::first(meanOD))),
                     mean = min(meanOD),
                     var = stats::var(meanOD),
                     se = se(meanOD), .groups = "drop")

  if (is.null(blankGroups)) {
    blankODMeans <- blankODMeans %>% dplyr::mutate(Group = "all")
  } else {
    colnames(blankODMeans)[colnames(blankODMeans) == blankGroups[1]] <- "Group"
  }

  output <- list(summary = blankODSummary,
                 individual = vector(mode = "list",
                                     length = data %>%
                                       dplyr::group_by(Plate, Replicate) %>%
                                       dplyr::summarise(n=dplyr::n(), .groups = "drop") %>%
                                       nrow()))

  plates <- unique(data$Plate)
  counter <- 1
  for(plate in plates) {
    replicates <- unique(data$Replicate[data$Plate == plate])
    for(replicate in replicates) {
      p <- ggplot2::ggplot() +
        ggplot2::geom_line(data = blankODs %>% dplyr::filter(Plate == plate, Replicate == replicate),
                           ggplot2::aes(x=Time_min, y=OD, group = Well), alpha = 0.1) +
        ggplot2::geom_line(data = blankODMeans %>% dplyr::filter(Plate == plate, Replicate == replicate),
                           ggplot2::aes(x=Time_min, y=meanOD, colour = Group), alpha = 1) +
        ggplot2::labs(x = "Time [min]", y = "Optical density") +
        ggplot2::ggtitle("OD for blank wells") +
        ggplot2::theme_bw()

      output$individual[[counter]]$plate <- plate
      output$individual[[counter]]$replicate <- replicate
      output$individual[[counter]]$plot <- p
      counter <- counter + 1
    }
  }
  return(output)
}


#' Perform quality control on OD data set
#'
#' This function performs a quality control analysis of an OD dataset,
#' covering both temperature through time and OD for wells designated as blanks.
#' The report is saved as a pdf file but the data and plots are also directly returned to the user.
#'
#' @param data The data set, as produced by processODData
#' @param blankGroups If specified, one or several columns in the \code{data} tibble by which blanking should be grouped. For examples, if there is a variable 'Medium' in the tibble, then with \code{blankGroups = 'Medium'}, averages for blanking will be taken across all wells with \code{wellType="BLANK"} for each value of this column (e.g. "LB", "M9" etc.), and subtracted from OD for data wells with the same Medium values.
#'
#' @param path The path where to save the pdf report. Defaults to the current working directory.
#' @param silent If \code{TRUE} (the default), the function won't return anything. If set to \code{FALSE}, the function will return the complete quality control analysis.
#'
#' @return Either NULL or a list containing all quality control summary statistics and plots, depending on the argument \code{silent}.
#' @export
#'
qcODData <- function(data, blankGroups = NULL, path = '.', silent = TRUE) {
  path <- fixPathName(path)
  qcT <- qcTemperature(data)
  qcB <- qcBlanks(data, blankGroups)
  grDevices::pdf(paste0(path,"/qc.pdf"), paper = "a4")
  projectName <- strsplit(getwd(), "/")[[1]]
  projectName <- projectName[length(projectName)]
  titleText <- paste0("Quality control report for project ", projectName)
  title <- cowplot::ggdraw() + cowplot::draw_label(titleText, fontface='bold', size = 18)
  subtitle <- cowplot::ggdraw() + cowplot::draw_label("Overview", fontface='bold', size = 24)
  # temperature summary plot:
  summaryPlotT <- ggplot2::ggplot(qcT$summary) +
    ggplot2::geom_errorbar(ggplot2::aes(x = Replicate, ymin = min, ymax = max)) +
    ggplot2::geom_point(ggplot2::aes(x = Replicate, y = SetT), col = "red", size = 10, shape = "_") +
    ggplot2::geom_point(ggplot2::aes(x = Replicate, y = mean)) +
    ggplot2::facet_wrap(facet = ggplot2::vars(Plate),
                        nrow = 1 + nrow(qcT$summary) %/% 16) +
    ggplot2::labs(x = "Replicate", y = "Temperature [\u00B0C]", colour = "") +
    ggplot2::ggtitle("Set vs. actual temperature") +
    ggplot2::theme_bw()

  # blanking summary plot:
  summaryPlotB <- ggplot2::ggplot(qcB$summary) +
    ggplot2::geom_errorbar(ggplot2::aes(x = Replicate, ymin = mean-se, ymax = mean+se), position = "dodge") +
    ggplot2::geom_point(ggplot2::aes(x = Replicate, y = mean)) +
    ggplot2::facet_wrap(facet = ggplot2::vars(Plate),
                        nrow = 1 + nrow(qcT$summary) %/% 16) +
    ggplot2::labs(x = "Replicate", y = "OD", colour = "") +
    ggplot2::ggtitle("OD for blank wells") +
    ggplot2::theme_bw()
  # print summary page:
  plots <- cowplot::plot_grid(summaryPlotT, summaryPlotB, nrow = 2)
  print(cowplot::plot_grid(title, subtitle, plots, rel_heights = c(0.1, 0.2, 1), nrow = 3))
  # individual plots
  for(i in 1:(length(qcT$individual))) {
    legendT <- cowplot::get_legend(qcT$individual[[i]]$plot)
    plotT <- qcT$individual[[i]]$plot + ggplot2::theme(legend.position = "none")
    legendB <- cowplot::get_legend(qcB$individual[[i]]$plot)
    plotB <- qcB$individual[[i]]$plot + ggplot2::theme(legend.position = "none")
    plots <- cowplot::plot_grid(plotT, legendT, plotB, legendB, rel_widths = c(1, .3), nrow = 2)
    titleText <- paste0("Plate ", qcT$individual[[i]]$plate, ", replicate ", qcT$individual[[i]]$replicate)
    title <- cowplot::ggdraw() + cowplot::draw_label(titleText, fontface='bold', size = 24)
    print(cowplot::plot_grid(title, plots, rel_heights = c(0.2, 1), nrow = 2))
  }
  grDevices::dev.off()
  cat(paste0("done!\nQuality control report saved in file ", path, "/qc.pdf", ".\n"))
  if (silent) {
    return(invisible(NULL))
  } else {
    return(list(qcTemperature = qcT, qcBlanks = qcB))
  }
}
