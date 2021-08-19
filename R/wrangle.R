###################################################################################
# This file contains functions to import and wrangle the raw data coming out of the plate reader, and connecting it with the information in the spec files.
###################################################################################

#' Import raw OD data from file
#'
#' This function imports a single raw data file produced by a microplate reader. Currently supported are BioTek Synergy HT and Epoch plate readers.
#'
#' @param fileName The name (including path) of the file. Supported file types are xlsx, csv and tsv.
#'
#' @return An (untidy) tibble in which each row represents one time point and each column represents one well, in addition to columns for date, plate reader model, time and temperature.
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
    dat <- suppressMessages(readxl::read_excel(fileName, col_names = FALSE, col_types = "text"))
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
    plateReader <- dat[[2]][plateReaderRow]
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
  names(processedDat)[c(1,2)] <- c("Time_min", "Temperature")
  processedDat <- processedDat %>%
    dplyr::mutate(Date=plateDate, PlateReader=plateReader, .before = 1)
  return(processedDat)
}


#' Import and process OD data
#'
#' \code{processODData} takes the path(s) of optical density (OD) data files and corresponding spec files, imports all those files, integrates spec and data files and returns a single, tidy tibble containing all the data.
#'
#' @param specPath The path of the spec files. Defaults to current working directory.
#' @param dataPath The path of the data files. Defaults to current working directory.
#' @param filePrefix The prefix of the data files. Defaults to "raw_", but this can be changed, including to "" when all files in the dataPath directory may be treated as potential data files.
#'
#' @return A single, tidy tibble with complete data from all experiments and replicates.
#' @export
#'
#' @importFrom magrittr %>%
processODData <- function(specPath=".", dataPath=".", filePrefix = "raw_") {
  specFileNames <- list.files(specPath)
  specFileNames <- specFileNames[startsWith(specFileNames, "spec_") & endsWith(specFileNames, ".csv")]
  if (length(specFileNames) == 0)
    stop("No spec files found.")

  dataFileNames <- list.files(dataPath)

  allData <- data.frame() # create the allData frame before populating
  for(i in 1:length(specFileNames)) {
    fileName <- substr(specFileNames[i], 6, nchar(specFileNames[i]) - 4)
    fileName <- paste0(filePrefix, fileName)
    cat(paste0("Working on data file ", fileName, " ..."))
    # check if corresponding data file is present:
    if (paste0(fileName, ".csv") %in% dataFileNames) {
      fileName <- paste0(fileName, ".csv")
    } else if (paste0(fileName, ".tsv") %in% dataFileNames) {
      fileName <- paste0(fileName, ".tsv")
    } else if (paste0(fileName, ".xlsx") %in% dataFileNames) {
      fileName <- paste0(fileName, ".xlsx")
    } else {
      fileName <- NA
      cat("no data file found!\n")
      warning("No data file corresponding to ", specFileNames[i], " found, was skipped.")
    }

    if (!is.na(fileName)) {
      specs <- suppressMessages(readr::read_csv(paste0(specPath, "/", specFileNames[i])))
      trafoData <- importODFile(paste0(dataPath, "/", fileName)) %>%
        tidyr::pivot_longer(cols = A1:H12, names_to = "Well", values_to = "OD") %>%
        dplyr::left_join(specs, by = "Well") %>%
        dplyr::relocate(Plate, Replicate, Date, PlateReader, Row, Column, Well, WellType) %>%
        dplyr::relocate(Time_min, Temperature,  OD, .after = last_col())

      if (all(names(allData)==names(trafoData))) {
        allData <- rbind(allData, trafoData)
        cat(paste0("Working on data file ", fileName, " ... done!\n"))
      } else {
        stop(paste0("Columns in ", specFileNames[i], " don't match with previous files."))
      }
    }
  }
  return(allData)
}


#' Blank optical density data
#'
#' \code{blankODs} takes a data file (tibble) of optical density (OD) data and, for each individual plate and time step, uses the average of wells designated as \code{wellType="BLANK"} to blank all the data cells (\code{wellType="BLANK"}).
#'
#' @param data A tibble containing OD data as produced by the function \code{processODdata}.
#' @param groups If specified, a column in the \code{data} tibble by which blanking should be grouped. For examples, if there is a variable 'Medium' in the tibble, averages for blanking will be taken across all wells with \code{wellType="BLANK"} for each value of this column (e.g. "LB", "M9" etc.), and subtracted from OD for data wells with the same Medium values.
#'
#' @return The original \code{data} tibble with an additional column \code{blankedOD}.
#' @export
#'
blankODs <- function(data, groups = NULL) {
  groups <- c("Plate", "Replicate", "Time_min", groups)
  blankMeans <- data %>%
    dplyr::filter(WellType == "BLANK") %>%
    dplyr::group_by_at(groups) %>%
    dplyr::summarise(meanBlankOD = mean(OD))

  blankedData <- data %>%
    dplyr::left_join(blankMeans, groups) %>%
    dplyr::mutate(blankedOD = OD - meanBlankOD) %>%
    dplyr::select(-meanBlankOD)
  return(blankedData)
}
