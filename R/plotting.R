specPlot <- function(plateName,
                     replicate,
                     vWellType,
                     rows,
                     columns,
                     border,
                     fileName) {
  plateX <- 0.1
  plateY <- 0.2
  wellWidth = 0.03
  wellHeight = 0.046
  nrowsTotal <- 8
  ncolsTotal <- 12
  labelSize <- 10
  colLabelDist <- 0.02
  rowLabelDist <- 0.01
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
           i, adj = c(0.5, 0.5))
      text(plateX + (i-0.5) * wellWidth, plateY + 8*wellHeight + colLabelDist,
           columns[i], adj = c(0, 0.5), srt = 90)
    }
  }
  text(0, 1, plateName, adj = c(0, 1), cex = 3)
  if (!is.null(rep)) text(0, 0.91, paste0("Replicate #", replicate), adj = c(0, 1), cex = 2)
  dev.off()
}
