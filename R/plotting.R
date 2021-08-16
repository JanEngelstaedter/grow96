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

  pdf(fileName, width = 11.7, height = 8.3)
  plot.new()
  for(i in 1:nrowsTotal)
    for(j in 1:ncolsTotal) {
      colour <- switch(vWellType[i,j],
                       BLANK = colBLANK, DATA = colDATA, EMPTY = colEMPTY)
      rect(plateX + (j - 1) * wellWidth, plateY + (nrowsTotal - i + 1) * wellHeight,
           plateX + j * wellWidth, plateY + (nrowsTotal - i) * wellHeight,
           col = colour)
    }
  for(i in 0:nrowsTotal) {
    lines(c(plateX, plateX + ncolsTotal * wellWidth),
          rep(plateY + i * wellHeight, 2))
    if(i>0) {
      text(plateX - rowLabelDist, plateY + (i-0.5) * wellHeight,
           LETTERS[8:1][i], adj = c(1, 0.5))
      text(plateX + 12*wellWidth + rowLabelDist, plateY + (i-0.5) * wellHeight,
           rows[8:1][i], adj = c(0, 0.5))
    }
  }
  for(i in 0:ncolsTotal) {
    lines(rep(plateX + i * wellWidth, 2),
          c(plateY, plateY + nrowsTotal * wellHeight))
    if(i>0) {
      text(plateX + (i-0.5) * wellWidth, plateY - colLabelDist,
           columns[i], adj = c(1, 0.5), srt = 90)
      text(plateX + (i-0.5) * wellWidth, plateY + 8*wellHeight + colLabelDist,
           i, adj = c(0.5, 0.5))
    }
  }
  # titles:
  text(0, 1, plateName, adj = c(0, 1), cex = 2.8)
  if (!is.null(replicate)) text(0, 0.91, paste0("Replicate #", replicate), adj = c(0, 1), cex = 1.4)

  rect(legendX, legendY - legendHeight,
       legendX + legendWidth, legendY, col = colDATA)
  rect(legendX + legendWidth + legendXSpace, legendY - legendHeight,
       legendX + 2*legendWidth + legendXSpace, legendY, col = colBLANK)
  rect(legendX + 2*legendWidth + 2*legendXSpace, legendY - legendHeight,
       legendX + 3*legendWidth + 2*legendXSpace, legendY, col = colEMPTY)
  text(legendX + legendWidth/2, legendY - legendHeight/2,
       "DATA", adj = c(0.5, 0.5))
  text(legendX + 3*legendWidth/2 + legendXSpace, legendY - legendHeight/2,
       "BLANK", adj = c(0.5, 0.5))
  text(legendX + 5*legendWidth/2 + 2*legendXSpace, legendY - legendHeight/2,
       "EMPTY", adj = c(0.5, 0.5))
  dev.off()
}
