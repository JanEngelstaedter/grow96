# functions to automatically generate spec files


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
                              border = TRUE,
                              reps = NULL,
                              constants = NULL,
                              randomise = 'none',
                              path = '') {

   # so far, only 96 plates supported:
  if (border) {
    nrows <- 6
    ncols <- 10
  } else {
    nrows <- 6
    ncols <- 10
  }

  if (length(rows) !=nrows)
    stop(paste(nrows, "rows expected."))
  if (length(cols) !=ncols)
    stop(paste(ncols, "columns expected."))
  if (!is.null(border) && border != 'EMPTY' && border != 'BLANK')
    stop("Unexpected value for 'border' argument; either 'EMPTY' or 'BLANK' expected.")

  vRow <- rep(LETTERS[1:8], 12)
  vColumn <- rep(1:12, each = 8)
  vWell <- paste0(vRow, vColumn)

  if (border) {
    vVarRow <- matrix(NA, nrow = 8, ncol = 12)
    vVarRow[2:7, 2:11] <- matrix(rows, nrow = 6, ncol = 10)
    vVarColumn <- matrix(NA, nrow = 8, ncol = 12)
    vVarColumn[2:7, 2:11] <- matrix(cols, nrow = 6, ncol = 10, byrow = TRUE)
  } else {
    vVarRow <- matrix(rows, nrow = 8, ncol = 12)
    vVarColumn <- matrix(cols, nrow = 8, ncol = 12, byrow = TRUE)
  }
  dim(vVarRow) <- NULL
  dim(vVarColumn) <- NULL


}

border <- TRUE
rows <- c("STP", "RIF", "TMP", "KAN", "CIP", "NONE")
cols <- paste0("strain", LETTERS[1:10])
