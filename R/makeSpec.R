# functions to automatically generate spec files

# to dos:
#    - write file
#    - graphical output
#    - roxygen



#' Title
#'
#' @param rowName
#' @param colName
#' @param rows
#' @param cols
#' @param reps
#' @param randomise
#' @param plateName
#' @param border
#' @param constants
#' @param path
#'
#' @return
#' @export
#'
#' @examples
makeSpec_fullfact <- function(plateName,
                              rowName,
                              colName,
                              rows,
                              cols,
                              border = NULL,
                              borderStyle = 'NAs',
                              blankRows = NULL,
                              blankCols = NULL,
                              reps = NULL,
                              constants = NULL,
                              randomise = NULL,
                              path = NULL,
                              makePlot = TRUE) {

  # so far, only 96 plates supported:
  nrowsTotal <- 8
  ncolsTotal <- 12
  if (!is.null(border)) {
    nrowsUsed <- nrowsTotal - 2
    ncolsUsed <- ncolsTotal - 2
  } else {
    nrowsUsed <- nrowsTotal
    ncolsUsed <- ncolsTotal
  }

  # initial checks:
  if (length(rows) !=nrowsUsed)
    stop(paste(nrows, "rows expected."))
  if (length(cols) !=ncolsUsed)
    stop(paste(ncols, "columns expected."))
  if (!is.null(border) && border != 'EMPTY' && border != 'BLANK')
    stop("Unexpected value for 'border' argument: must be 'EMPTY' or 'BLANK'.")
  if (!is.null(randomise) && !(randomise %in% c("rows", "columns", "both")))
    stop("Unexpected value for 'randomise' argument: must be 'rows', 'columns' or 'both'.")

  # columns with row, column and well labels:
  vRow <- rep(LETTERS[1:nrowsTotal], ncolsTotal)
  vColumn <- rep(1:ncolsTotal, each = nrowsTotal)
  vWell <- paste0(vRow, vColumn)

  # matrix for well type:
  if (!is.null(border)) {
    vWellType <- matrix(border, nrow = nrowsTotal, ncol = ncolsTotal)
    vWellType[2:(nrowsTotal-1), 2:(ncolsTotal-1)] <- matrix("DATA", nrow=nrowsUsed, ncol=ncolsUsed)
    if (!is.null(blankRows))
      vWellType[blankRows, 2:(ncolsTotal-1)] <- "BLANK"
    if (!is.null(blankCols))
      vWellType[2:(nrowsTotal-1), blankColumns] <- "BLANK"
  } else {
    vWellType <- matrix("DATA", nrow = nrowsTotal, ncol = ncolsTotal)
    if (!is.null(blankRows))
      vWellType[blankRows, 1:ncolsTotal] <- "BLANK"
    if (!is.null(blankCols))
      vWellType[1:nrowsTotal, blankColumns] <- "BLANK"
  }

  # matrices for the row and column variables:
  if (!is.null(border)) {
    vVarRow <- matrix(NA, nrow = nrowsTotal, ncol = ncolsTotal)
    if (borderStyle %in% c('extendRows', 'extendBoth'))
      vVarRow[2:(nrowsTotal-1), 1:ncolsTotal] <- matrix(rows, nrow=nrowsUsed, ncol=ncolsTotal)
    else
      vVarRow[2:(nrowsTotal-1), 2:(ncolsTotal-1)] <- matrix(rows, nrow=nrowsUsed, ncol=ncolsUsed)

    vVarColumn <- matrix(NA, nrow = nrowsTotal, ncol = ncolsTotal)
    if (borderStyle %in% c('extendColumns', 'extendBoth'))
      vVarColumn[1:nrowsTotal, 2:(ncolsTotal-1)] <- matrix(cols, nrow=nrowsTotal, ncol=ncolsUsed, byrow=TRUE)
    else
      vVarColumn[2:(nrowsTotal-1), 2:(ncolsTotal-1)] <- matrix(cols, nrow=nrowsUsed, ncol=ncolsUsed, byrow=TRUE)
  } else {
    vVarRow <- matrix(rows, nrow = nrowsTotal, ncol = ncolsTotal)
    vVarColumn <- matrix(cols, nrow = nrowsTotal, ncol = ncolsTotal, byrow = TRUE)
  }

  # matrices for constant variables:
  if (!is.null(constants)) {
    vConstList <- vector(mode = "list", length = length(constants))
    names(vConstList) <- names(constants)
    for(i in 1:length(constants)) {
      if (!is.null(border)) {
        vConstList[[i]] <- matrix(NA, nrow = nrowsTotal, ncol = ncolsTotal)
        if (borderStyle %in% c('extendRows', 'extendBoth'))
          vConstList[[i]][2:(nrowsTotal-1), 1:ncolsTotal] <- matrix(constants[i], nrow=nrowsUsed, ncol=ncolsTotal)
        else
          vConstList[[i]][2:(nrowsTotal-1), 2:(ncolsTotal-1)] <- matrix(constants[i], nrow=nrowsUsed, ncol=ncolsUsed)
        if (borderStyle %in% c('extendColumns', 'extendBoth'))
          vConstList[[i]][1:nrowsTotal, 2:(ncolsTotal-1)] <- matrix(constants[i], nrow=nrowsTotal, ncol=ncolsUsed)
      } else {
        vConstList[[i]] <- matrix(constants[i], nrow = nrowsTotal, ncol = ncolsTotal)
      }
    }
  }

  if (is.null(reps)) {
    # shuffling if randomisation is requested:
    if (!is.null(randomise)) {
      if (randomise %in% c("rows", "both")) {
        if (is.null(border))
          rowPerm <- sample(1:nrowsTotal, nrowsTotal)
        else
          rowPerm <- c(1, sample(2:(nrowsTotal-1), nrowsTotal-2), nrowsTotal)
        vWellType <- vWellType[rowPerm,]
        vVarRow <- vVarRow[rowPerm,]
      }
      if (randomise %in% c("columns", "both")) {
        if (is.null(border))
          colPerm <- sample(1:ncolsTotal, ncolsTotal)
        else
          colPerm <- c(1, sample(2:(ncolsTotal-1), ncolsTotal-2), ncolsTotal)
        vWellType <- vWellType[, colPerm]
        vVarColumn <- vVarColumn[, colPerm]
      }
    }

    specDF <- data.frame(Plate = rep(plateName, nrowsTotal * ncolsTotal),
                         Row = vRow,
                         Column = vColumn,
                         Well = vWell,
                         WellType = as.vector(vWellType),
                         VarRow = as.vector(vVarRow),
                         VarCol = as.vector(vVarColumn))
    names(specDF)[names(specDF) == "VarRow"] <- rowName
    names(specDF)[names(specDF) == "VarCol"] <- colName

    # add constant columns:
    if (!is.null(constants))
      for(i in 1:length(constants))
        specDF[names(constants)[i]] <- as.vector(vConstList[[i]])

    fileName <- paste0(path, "spec_", plateName, ".csv")
    write.csv(specDF, fileName, row.names = FALSE)
    print(paste0("Spec file ", fileName, " written."))

  } else {   # several replicates should be created
    specDFs <- vector(mode = "list", length = reps)
    for(r in 1:reps) {
      vWellTypeRep <- vWellType
      vVarRowRep <- vVarRow
      vVarColumnRep <- vVarColumn

      # shuffling if randomisation is requested:
      if (!is.null(randomise)) {
        if (randomise %in% c("rows", "both")) {
          if (is.null(border))
            rowPerm <- sample(1:nrowsTotal, nrowsTotal)
          else
            rowPerm <- c(1, sample(2:(nrowsTotal-1), nrowsTotal-2), nrowsTotal)
          vWellTypeRep <- vWellTypeRep[rowPerm,]
          vVarRowRep <- vVarRowRep[rowPerm,]
        }
        if (randomise %in% c("columns", "both")) {
          if (is.null(border))
            colPerm <- sample(1:ncolsTotal, ncolsTotal)
          else
            colPerm <- c(1, sample(2:(ncolsTotal-1), ncolsTotal-2), ncolsTotal)
          vWellTypeRep <- vWellTypeRep[, colPerm]
          vVarColumnRep <- vVarColumnRep[, colPerm]
        }
      }

      specDFs[[r]] <- data.frame(Plate = rep(plateName, nrowsTotal * ncolsTotal),
                                   Replicate = r,
                                   Row = vRow,
                                   Column = vColumn,
                                   Well = vWell,
                                   WellType = as.vector(vWellTypeRep),
                                   VarRow = as.vector(vVarRowRep),
                                   VarCol = as.vector(vVarColumnRep))
      names(specDFs[[r]])[names(specDFs[[r]]) == "VarRow"] <- rowName
      names(specDFs[[r]])[names(specDFs[[r]]) == "VarCol"] <- colName

      # add constant columns:
      if (!is.null(constants))
        for(i in 1:length(constants))
          specDFs[[r]][names(constants)[i]] <- as.vector(vConstList[[i]])

      fileName <- paste0(path, "spec_", plateName, "_rep", r, ".csv")
      write.csv(specDFs[[r]], fileName, row.names = FALSE)
      print(paste0("Spec file ", fileName, " written."))

      if (makePlot) {
        fileName <- paste0(path, "specplot_", plateName, "_rep", r, ".pdf")
        specPlot(vRow, vColumn, vWell, vWellTypeRep, vVarRowRep, vVarColumnRep, fileName)
        print(paste0("Spec plot file ", fileName, " written."))
      }
    }
  }
  return(invisible(NULL))
}

border <- "EMPTY"
drugs <- c("STP", "RIF", "TMP", "KAN", "CIP", "NONE")
strains <- paste0("S", 1:10)
constants <- c(Medium = "LB+")

makeSpec_fullfact("test", "Drug", "Strain", drugs, strains,
                  border = "EMPTY",
                  borderStyle = "extendBoth",
                  blankRows = 7,
                  constants = constants,
                  reps = 3,
                  randomise = "both",
                  path = "./data/")
