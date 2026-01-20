############################################################################################
# This file contains functions (currently just one) to automatically generate spec files.
############################################################################################

#' Generate a plate specification based on a full factorial design
#'
#' \code{makeSpec_fullFact} produces "spec-files": csv files containing tables that assign values of two (or more) variables to each well on a 96-well plate. The design used is one where one variable varies across rows and another across column, in a full factorial design.
#'
#' @param plateName Name of the plate design
#' @param rowName Name of the variable for the rows. Typical values could be "strain", "medium" or "drug".
#' @param columnName Name of the variable for the columns. Typical values could be "strain", "medium" or "drug".
#' @param rows Values of the row variable.
#' @param columns Values of the column variable.
#' @param border Specifies whether the border wells should be treated differently. With the default value \code{NULL} there is no border and all wells can be used to generate data. With the value \code{'EMPTY'} the wells on the border will be designated as wellType=EMPTY, which means the OD data will be ignored in subsequent analyses. With the value \code{'BLANK'} these wells are designated as wellType=BLANK, which means they can be used for blanking. See below for details.
#' @param borderStyle Specifies how to fill the border wells, provided that a border has been defined with the \code{border} parameter. A value of \code{'NAs'} (the default) add NAs for all variables on the entire border. A values of \code{'extendRows'} extends the row variable and all constant variables to the first and last column of the plate, whereas the remainder of the border will be filled with NAs. A values of \code{'extendColumns'} extends the column variable and all constant variables to the first and last row of the plate, and a value of \code{'both'} does both types of extensions.
#' @param blankRows A vector of one or more row numbers that will be used for blanking. By default (value \code{NULL}) no blank rows are introduced.
#' @param blankColumns  vector of one or more column numbers that will be used for blanking. By default (value \code{NULL}) no blank columns are introduced.
#' @param emptyRows A vector of one or more row numbers that will be left empty. By default (value \code{NULL}) no blank rows are introduced.
#' @param emptyColumns  vector of one or more column numbers that will be left empty. By default (value \code{NULL}) no blank columns are introduced.
#' @param replicates Number of replicate plate designs to generate. By default (value \code{NULL}), only a single plate design will be generated. When specified as an integer number, this number of spec files will be generated, and for each replicate number will appear in the file name as well as a column in the table.
#' When a vector of more than one number is supplied, those vector elements will be used as replicate numbers.
#' @param randomise Whether to randomise rows and/or columns. Possible values are \code{NULL} (no ramdomisation, the default), \code{'rows'}, \code{'columns'}, and \code{'both'}.
#' @param constants Names and values of additional variables that are constant across the plate (see Details).
#' @param specPath Path where to save the spec file(s).
#' @param makePlot Whether to produce a pdf file showing the plate design. Defaults to \code{TRUE}.
#' @param plotPath Path where to save the spec plot file(s). Defaults to \code{path}.
#'
#' @details
#'
#' The main output of this function is a csv file with a table in which rows are the 96 wells of a standard microtitre plate. This table will have, at the minimum, the following columns: "Plate", "Row",	"Column",	"Well",	"WellType", <\code{rowName}>, and <\code{columnName}>, where the latter two are specified by the corresponding arguments of the function. The "Plate" column contains the name of the plate design in all rows, as specified by the \code{plateName} argument. "Row", "Column" and "Well" contains the coordinates for each well, in the standard format A...H, 1...12, and "A1...H12". The "WellType" column specifies how a well should be treated in subsequent data analyses. The default value "DATA" means the OD values from this well should be treated as normal growth data, a value "BLANK" indicates that the well should be used for blanking, and a value "EMPTY" indicates that the well should be ignored. "BLANK" and "EMPTY" values are generated automatically when a border is specified (argument \code{border}), and when empty/blank rows or columns are defined by the respective arguments \code{blankRows}, \code{blankColumns}, \code{emptyRows}, and \code{emptyColumns}. Finally, the last two columns contain the values of the row and column variables for each well.
#'
#' In addition to these default columns, an optional second column "Replicate" is generated when several replicate plates are defined using the parameter \code{replicates}. This means that several spec files will be generated (one for each replicate), and the column "Replicate" then contains the same replicate number for all wells in each of these files. The replicate number is also attached to the file name of all csv files.
#'
#' Finally, it is possible to add one or several columns for variables that are constant in a plate design. This might be useful if several different plate designs are to be combined in a larger analysis where the values of the constant variables may differ between plates, or simply as a way to store additional information about the plate. These columns are added using the \code{constants} argument, which should be a named vector in which each element corresponds to a column and where the value of that element defines the value of the variable and the name of the element the name of the variable (i.e. column).
#'
#' The \code{randomise} argument specifies if row and/or column variables should be randomised. If this is the case, and if several replicate plate designs are generated, each replicate will have a different order of values in the rows and/or columns. (Note though that the function does not check if all plates are actually different, but given the large number of possible permutations this is very likely. E.g., even if only the six interior rows are used and randomised, there are 6!=720 possible permutations and hence with <10 replicates the chance of randomly generating two identical plates is small.)
#'
#' In addition to the main output, the spec file(s), the \code{makeSpec_fullFact} function also (by default) generates pdf files showing the plate design, which may be useful for checking that the function did what the user desired to do, and also may help filling the plates during the experiment. These pdf files can be saved in a separate folder as the spec files, as specified by the \code{plotPath} vs. \code{specPath} arguments.
#'
#' @export
#'
#' @examples
#'
#' # simple example:
#' #makeSpec_fullFact(plateName = "Example1",
#' #                  rowName = "concRIF", columnName = "concSTP",
#' #                  rows = 0:7, columns = 0:11)
#'
#' # more complex design:
#' # makeSpec_fullFact(plateName = "Example2",
#' #                   rowName = "Drug",
#' #                   columnName = "Strain",
#' #                   rows = c("STP", "RIF", "TMP", "KAN", "CIP", "NONE"),
#' #                   columns = paste0("Strain", LETTERS[1:10]),
#' #                   border = "EMPTY",
#' #                   blankRows = 7,
#' #                   replicates = 3,
#' #                   randomise = "rows")
#'
makeSpec_fullFact <- function(plateName,
                              rowName,
                              columnName,
                              rows,
                              columns,
                              border = NULL,
                              borderStyle = 'NAs',
                              blankRows = NULL,
                              blankColumns = NULL,
                              emptyRows = NULL,
                              emptyColumns = NULL,
                              replicates = NULL,
                              constants = NULL,
                              randomise = NULL,
                              specPath = '.',
                              makePlot = TRUE,
                              plotPath = specPath) {

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
    stop(paste(nrowsUsed, "rows expected."))
  if (length(columns) !=ncolsUsed)
    stop(paste(ncolsUsed, "columns expected."))
  if (!is.null(border) && border != 'EMPTY' && border != 'BLANK')
    stop("Unexpected value for 'border' argument: must be 'EMPTY' or 'BLANK'.")
  if (!is.null(randomise) && !(randomise %in% c("rows", "columns", "both")))
    stop("Unexpected value for 'randomise' argument: must be 'rows', 'columns' or 'both'.")

  # check paths:
  specPath <- fixPathName(specPath)
  plotPath <- fixPathName(plotPath)
  if(!dir.exists(specPath)) {
    dir.create(specPath)
    warning(paste0("The directory ", specPath, " for the spec files didn't exist but has now been created."))
  }
  if(makePlot & (!dir.exists(plotPath))) {
    dir.create(plotPath)
    warning(paste0("The directory ", plotPath, " for the plots didn't exist but has now been created."))
  }

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
    if (!is.null(blankColumns))
      vWellType[2:(nrowsTotal-1), blankColumns] <- "BLANK"
    if (!is.null(emptyRows))
      vWellType[emptyRows, 2:(ncolsTotal-1)] <- "EMPTY"
    if (!is.null(emptyColumns))
      vWellType[2:(nrowsTotal-1), emptyColumns] <- "EMPTY"
  } else {
    vWellType <- matrix("DATA", nrow = nrowsTotal, ncol = ncolsTotal)
    if (!is.null(blankRows))
      vWellType[blankRows, 1:ncolsTotal] <- "BLANK"
    if (!is.null(blankColumns))
      vWellType[1:nrowsTotal, blankColumns] <- "BLANK"
    if (!is.null(emptyRows))
      vWellType[emptyRows, 1:ncolsTotal] <- "EMPTY"
    if (!is.null(emptyColumns))
      vWellType[1:nrowsTotal, emptyColumns] <- "EMPTY"
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
      vVarColumn[1:nrowsTotal, 2:(ncolsTotal-1)] <- matrix(columns, nrow=nrowsTotal, ncol=ncolsUsed, byrow=TRUE)
    else
      vVarColumn[2:(nrowsTotal-1), 2:(ncolsTotal-1)] <- matrix(columns, nrow=nrowsUsed, ncol=ncolsUsed, byrow=TRUE)
  } else {
    vVarRow <- matrix(rows, nrow = nrowsTotal, ncol = ncolsTotal)
    vVarColumn <- matrix(columns, nrow = nrowsTotal, ncol = ncolsTotal, byrow = TRUE)
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

  if (is.null(replicates)) { # no replicates
    reps <- 1
  } else if (length(replicates) == 1L) {
    reps <- 1:replicates
  } else {
    reps <- replicates
  }
  specDFs <- vector(mode = "list", length = length(reps))
  for(r in 1:length(reps)) {
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
                               Replicate = reps[r],
                               Row = vRow,
                               Column = vColumn,
                               Well = vWell,
                               WellType = as.vector(vWellTypeRep),
                               VarRow = as.vector(vVarRowRep),
                               VarCol = as.vector(vVarColumnRep))
    names(specDFs[[r]])[names(specDFs[[r]]) == "VarRow"] <- rowName
    names(specDFs[[r]])[names(specDFs[[r]]) == "VarCol"] <- columnName

    # add constant columns:
    if (!is.null(constants))
      for(i in 1:length(constants))
        specDFs[[r]][names(constants)[i]] <- as.vector(vConstList[[i]])

    if (is.null(replicates)) {
      fileName <- paste0(specPath, "/spec_", plateName, ".csv")
    } else {
      fileName <- paste0(specPath, "/spec_", plateName, "_rep", reps[r], ".csv")
    }
    if (file.exists(fileName))
      stop(paste0("Spec file '", fileName, "' already exists. Either this file needs to be deleted or a different file name needs to be chosen."))
    readr::write_csv(specDFs[[r]], fileName)
    cat(paste0("Spec file ", fileName, " written.\n"))

    if (makePlot) {
      if (is.null(replicates)) {
        fileName <- paste0(plotPath, "/specplot_", plateName, ".pdf")
        repPlot <- NULL
      } else {
        fileName <- paste0(plotPath, "/specplot_", plateName, "_rep", reps[r], ".pdf")
        repPlot <- reps[r]
      }
      if (file.exists(fileName))
        stop(paste0("Spec plot '", fileName, "' already exists. Either this file needs to be deleted or a different file name needs to be chosen."))
      specPlot_fullFact(plateName,
                        repPlot,
                        vWellTypeRep,
                        stats::na.omit(vVarRowRep[,2]),
                        stats::na.omit(vVarColumnRep[2,]),
                        border,
                        fileName)
      cat(paste0("Spec plot file ", fileName, " written.\n"))
    }
  }
  return(invisible(NULL))
}



#' Generate a plate specification based on a wrapping design
#'
#' \code{makeSpec_fullFact} produces "spec-files": csv files containing tables that assign values of two (or more) variables to each well on a 96-well plate.
#' The design used is one where one variable called "wrap" varies across columns in a row and gets then wrapped to the next row, etc.
#' Alternatively, the plate can also be filled column by column in this manner.
#' There may also be several groups of wrapping variables.
#'
#' @param plateName Name of the plate design
#' @param wrapName Name of the variable to be arranged as wrapped. Typical values could be "strain", "medium" or "drug".
#' @param wraps Values of the wrap variable.
#' @param groupName Name of the variable for a group, in case there is more than one wrapped variable. Typical values could be "strain", "medium" or "drug". Defaults to NULL.
#' @param groups Values of the group variable.
#' @param border Specifies whether the border wells should be treated differently. With the default value \code{NULL} there is no border and all wells can be used to generate data. With the value \code{'EMPTY'} the wells on the border will be designated as wellType=EMPTY, which means the OD data will be ignored in subsequent analyses. With the value \code{'BLANK'} these wells are designated as wellType=BLANK, which means they can be used for blanking. See below for details.
#' @param spares Specifies whether well not covered by the wrap variable should be designated as blanking wells ('BLANK', default) or as empty ('EMPTY').
#' @param replicates Number of replicate plate designs to generate. By default (value \code{NULL}), only a single plate design will be generated. When specified as an integer number, this number of spec files will be generated, and for each replicate number will appear in the file name as well as a column in the table.
#' When a vector of more than one number is supplied, those vector elements will be used as replicate numbers.
#' @param randomise Whether to randomise the wrap and/or the group variables. Possible values are \code{NULL} (no ramdomisation, the default), \code{'wraps'}, \code{'groups'}, and \code{'both'}.
#' @param constants Names and values of additional variables that are constant across the plate (see Details).
#' @param specPath Path where to save the spec file(s).
#' @param makePlot Whether to produce a pdf file showing the plate design. Defaults to \code{TRUE}.
#' @param plotPath Path where to save the spec plot file(s). Defaults to \code{specPath}.
#'
#' @details
#'
#' The main output of this function is a csv file with a table in which rows are the 96 wells of a standard microtitre plate. This table will have, at the minimum, the following columns: "Plate", "Row",	"Column",	"Well",	"WellType", <\code{rowName}>, and <\code{columnName}>, where the latter two are specified by the corresponding arguments of the function. The "Plate" column contains the name of the plate design in all rows, as specified by the \code{plateName} argument. "Row", "Column" and "Well" contains the coordinates for each well, in the standard format A...H, 1...12, and "A1...H12". The "WellType" column specifies how a well should be treated in subsequent data analyses. The default value "DATA" means the OD values from this well should be treated as normal growth data, a value "BLANK" indicates that the well should be used for blanking, and a value "EMPTY" indicates that the well should be ignored. "BLANK" and "EMPTY" values are generated automatically when a border is specified (argument \code{border}), and when empty/blank rows or columns are defined by the respective arguments \code{blankRows}, \code{blankColumns}, \code{emptyRows}, and \code{emptyColumns}. Finally, the last two columns contain the values of the row and column variables for each well.
#'
#' In addition to these default columns, an optional second column "Replicate" is generated when several replicate plates are defined using the parameter \code{replicates}. This means that several spec files will be generated (one for each replicate), and the column "Replicate" then contains the same replicate number for all wells in each of these files. The replicate number is also attached to the file name of all csv files.
#'
#' Finally, it is possible to add one or several columns for variables that are constant in a plate design. This might be useful if several different plate designs are to be combined in a larger analysis where the values of the constant variables may differ between plates, or simply as a way to store additional information about the plate. These columns are added using the \code{constants} argument, which should be a named vector in which each element corresponds to a column and where the value of that element defines the value of the variable and the name of the element the name of the variable (i.e. column).
#'
#' The \code{randomise} argument specifies if row and/or column variables should be randomised. If this is the case, and if several replicate plate designs are generated, each replicate will have a different order of values in the rows and/or columns. (Note though that the function does not check if all plates are actually different, but given the large number of possible permutations this is very likely. E.g., even if only the six interior rows are used and randomised, there are 6!=720 possible permutations and hence with <10 replicates the chance of randomly generating two identical plates is small.)
#'
#' In addition to the main output, the spec file(s), the \code{makeSpec_fullFact} function also (by default) generates pdf files showing the plate design, which may be useful for checking that the function did what the user desired to do, and also may help filling the plates during the experiment. These pdf files can be saved in a separate folder as the spec files, as specified by the \code{plotPath} vs. \code{specPath} arguments.
#'
#' @export
#'
#' @examples
#'
#' # simple example:
#' #makeSpec_wrapping(plateName = "Example1",
#' #                  wrapName = "strain",
#' #                  wraps = paste0("Strain", 1:48)
#'
#' # more complex design:
#' # makeSpec_wrapping(plateName = "Example2",
#' #                   wrapName = "strain",
#' #                   wraps = paste0("Strain", 1:24),
#' #                   groupName = "Drug",
#' #                   groups = c("no drug", "RIF"),
#' #                   border = "EMPTY",
#' #                   blankRows = 7,
#' #                   replicates = 3,
#' #                   randomise = "wraps")
#'
makeSpec_wrapping <- function(plateName,
                              wrapName,
                              wraps,
                              groupName = NULL,
                              groups = NULL,
                              border = NULL,
                              spares = "BLANK",
                              replicates = NULL,
                              constants = NULL,
                              randomise = NULL,
                              specPath = '.',
                              makePlot = TRUE,
                              plotPath = specPath) {

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

  nwraps <- length(wraps)
  if (is.null(groups)) {
    ngroups <- 1
  } else {
    ngroups <- length(groups)
  }

  nrowsPerGroup <- (nwraps - 1) %/% ncolsUsed + 1

  # initial checks:
  if (nwraps <= ncolsUsed)
    stop("Not enough values in wraps vector to wrap over multiple rows.")
  if (!(spares %in% c("EMPTY", "BLANK")))
    stop("The spare argument should either be EMPTY or BLANK.")
  if (nrowsPerGroup * ngroups > ncolsUsed)
    stop("Not enough rows on plate for this design.")
  if (!is.null(border) && border != 'EMPTY' && border != 'BLANK')
    stop("Unexpected value for 'border' argument: must be 'EMPTY' or 'BLANK'.")
  if (!is.null(randomise) && !(randomise %in% c("wraps", "groups", "both")))
    stop("Unexpected value for 'randomise' argument: must be 'wraps', 'groups' or 'both'.")

  # check paths:
  specPath <- fixPathName(specPath)
  plotPath <- fixPathName(plotPath)
  if(!dir.exists(specPath)) {
    dir.create(specPath)
    warning(paste0("The directory ", specPath, " for the spec files didn't exist but has now been created."))
  }
  if(makePlot & (!dir.exists(plotPath))) {
    dir.create(plotPath)
    warning(paste0("The directory ", plotPath, " for the plots didn't exist but has now been created."))
  }

  # determine replicates:
  if (is.null(replicates)) { # no replicates
    reps <- 1
  } else if (length(replicates) == 1L) {
    reps <- 1:replicates
  } else {
    reps <- replicates
  }

  # columns with row, column and well labels:
  vRow <- rep(LETTERS[1:nrowsTotal], ncolsTotal)
  vColumn <- rep(1:ncolsTotal, each = nrowsTotal)
  vWell <- paste0(vRow, vColumn)

  specDFs <- vector(mode = "list", length = length(reps))
  for(r in 1:length(reps)) {
    # matrices for welltype, wrap and group variables:
    if (is.null(border)) {
      vWellType <- matrix(spares, nrow = nrowsTotal, ncol = ncolsTotal)
    } else {
      vWellType <- matrix(border, nrow = nrowsTotal, ncol = ncolsTotal)
      vWellType[2:(nrowsTotal-1), 2:(ncolsTotal-1)] <- matrix(spares, nrow=nrowsUsed, ncol=ncolsUsed)
    }
    vVarWrap <- matrix(NA, nrow = nrowsTotal, ncol = ncolsTotal)
    wraps_rep <- c(wraps, rep(NA, ncolsUsed - length(wraps) %% ncolsUsed))
    if (!is.null(groups)) {
      vVarGroup <- matrix(NA, nrow = nrowsTotal, ncol = ncolsTotal)
      groups_rep <- groups
    } else {
      vVarGroup <- NULL
    }
    if (!is.null(randomise)) {
      if (randomise %in% c("wraps", "both")) {
        wraps_rep <- sample(wraps_rep)
      }
      if (randomise %in% c("groups", "both")) {
        groups_rep <- sample(groups_rep)
      }
    }
    for(g in 1:ngroups) {
      indices <- wrap_indices(nwraps, g, ngroups, nrowsTotal, ncolsTotal, border)
      # Row-wise fill using transpose:
      vWellType[] <- t(replace(t(vWellType), t(indices), ifelse(is.na(wraps_rep), spares, "DATA")))
      vVarWrap[] <- t(replace(t(vVarWrap), t(indices), wraps_rep))
      if (!is.null(groups)) {
        vVarGroup[] <- t(replace(t(vVarGroup), t(indices), groups_rep[g]))
      }
    }

    # matrices for constant variables:
    if (!is.null(constants)) {
      vConstList <- vector(mode = "list", length = length(constants))
      names(vConstList) <- names(constants)
      for(i in 1:length(constants)) {
        if (!is.null(border)) {
          vConstList[[i]] <- matrix(NA, nrow = nrowsTotal, ncol = ncolsTotal)
          vConstList[[i]][2:(nrowsTotal-1), 2:(ncolsTotal-1)] <- matrix(constants[i], nrow=nrowsUsed, ncol=ncolsTotal)
        } else {
          vConstList[[i]] <- matrix(constants[i], nrow = nrowsTotal, ncol = ncolsTotal)
        }
      }
    }

    specDFs[[r]] <- data.frame(Plate = rep(plateName, nrowsTotal * ncolsTotal),
                               Replicate = reps[r],
                               Row = vRow,
                               Column = vColumn,
                               Well = vWell,
                               WellType = as.vector(vWellType),
                               VarWrap = as.vector(vVarWrap))
    names(specDFs[[r]])[names(specDFs[[r]]) == "VarWrap"] <- wrapName
    if (!is.null(groups)) {
      specDFs[[r]][["VarGroup"]] = as.vector(vVarGroup)
      names(specDFs[[r]])[names(specDFs[[r]]) == "VarGroup"] <- groupName
    }

    # add constant columns:
    if (!is.null(constants))
      for(i in 1:length(constants))
        specDFs[[r]][names(constants)[i]] <- as.vector(vConstList[[i]])

    if (is.null(replicates)) {
      fileName <- paste0(specPath, "/spec_", plateName, ".csv")
    } else {
      fileName <- paste0(specPath, "/spec_", plateName, "_rep", reps[r], ".csv")
    }
    if (file.exists(fileName))
      stop(paste0("Spec file '", fileName, "' already exists. Either this file needs to be deleted or a different file name needs to be chosen."))
    readr::write_csv(specDFs[[r]], fileName)
    cat(paste0("Spec file ", fileName, " written.\n"))

    if (makePlot) {
      if (is.null(replicates)) {
        fileName <- paste0(plotPath, "/specplot_", plateName, ".pdf")
        repPlot <- NULL
      } else {
        fileName <- paste0(plotPath, "/specplot_", plateName, "_rep", reps[r], ".pdf")
        repPlot <- reps[r]
      }
      if (file.exists(fileName))
        stop(paste0("Spec plot '", fileName, "' already exists. Either this file needs to be deleted or a different file name needs to be chosen."))
      specPlot_wrapping(plateName,
                        repPlot,
                        vWellType,
                        vVarWrap,
                        border,
                        fileName,
                        vVarGroup = vVarGroup)
      cat(paste0("Spec plot file ", fileName, " written.\n"))
    }
  }
  return(invisible(NULL))
}
