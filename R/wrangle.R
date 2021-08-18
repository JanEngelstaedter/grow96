###################################################################################
# This file contains functions to import and wrange the raw data coming out of the plate reader, and connecting it with the information in the spec files.
###################################################################################

#' Import raw OD data from file
#'
#' @param fileName The name (including path) of the file. Currently supported file types are xlsx, csv and tsv.
#'
#' @return An untidy tibble in which each row represents one time point and each column represents one well, in addition to columns for date, plate reader model, time and temperature.
#' @export
#'
importODFile <- function(fileName) {
  # import entire file:
  fileType <- fileExtension(fileName)
  if (fileType=="csv") {
    dat <- readr::read_csv(fileName, col_names = FALSE)
  } else if (fileType=="tsv") {
    dat <- readr::read_tsv(fileName, col_names = FALSE)
  } else if (fileType=="xlsx") {
    dat <- readxl::read_excel(fileName, col_names = FALSE, col_types = "text")
  } else {
    stop("File type not recognised, extension must be csv, tsv or xlsx.")
  }

  # find date:
  dateRow <- which(dat[,1] == "Date")
  if (length(dateRow) != 1) {
    stop("Couldn't impute date from data file.")
  } else {
    if (fileType == "xlsx") {
      plateDate <- as.integer(dat[dateRow, 2])
      plateDate <- as.Date.numeric(plateDate, origin = "1899-12-30")
    } else {
      plateDate <- dat[dateRow, 2]
      plateDate <- as.Date.character(plateDate, origin = "1970-01-01")
    }
  }
  # find plate reader:
  plateReaderRow <- which(dat[,1] == "Reader Type:")
  if (length(plateReaderRow) != 1) {
    stop("Couldn't impute plate reader from data file.")
  } else {
    plateReader <- dat[plateReaderRow, 2]
  }

  # find start and end of data matrix:
  matrixStartRow <- which(dat[,2] == "Time")[1] + 1  # first occurrence of "Time" keyword
  matrixEndRow <- which(is.na(dat[matrixStartRow:nrow(dat),2]) |
                          dat[matrixStartRow:nrow(dat),2]== "")[1] + matrixStartRow - 2

  # constructing final imported data frame:
  processedDat <- dat[matrixStartRow:matrixEndRow, 2:(3 + 96)]
  if(fileType == "xlsx") {
    processedDat[,1] <- 24*60*as.double(dplyr::pull(processedDat,1))  # convert from day to minutes
  } else {
    processedDat[,1] <- as.POSIXct(processedDat[,1],format="%H:%M:%S")
  }
  processedDat[,-1] <- lapply(processedDat[,-1], as.double)
  names(processedDat) <- dat[matrixStartRow - 1, 2:(3 + 96)]
  names(processedDat)[[2]] <- "Temperature"
  processedDat <- processedDat %>%
    mutate(Date=plateDate, PlateReader=plateReader, .before = 1)
}
