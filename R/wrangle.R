########################################################################################################
# This file contains functions to import and wrangle the raw data coming out of the plate reader,
# and connecting it with the information in the spec files.
########################################################################################################

#' Import raw OD data from file
#'
#' This function imports a single raw data file produced by a microplate reader. Currently supported are BioTek Synergy HT and Epoch plate readers.
#'
#' @param fileName The name (including path) of the file. Supported file types are xlsx, csv and tsv.
#'
#' @return An (untidy) tibble in which each row represents one time point and each column represents one well, in addition to columns for date, plate reader model, time and temperature.
#'
#' @keywords internal
#'
importODFile <- function(fileName) {
  # import entire file:
  fileType <- fileExtension(fileName)
  if (fileType=="csv") {
    #dat <- readr::read_csv(fileName, col_names = FALSE)
    maxCols <- max(count.fields(fileName, sep = ','))
    dat <- tibble::as_tibble(utils::read.csv(fileName,
                                             header = FALSE,
                                             fill = TRUE,
                                             blank.lines.skip = FALSE,
                                             col.names = paste0("V", 1:maxCols)))
  } else if (fileType=="tsv" || fileType=="txt") {
    maxCols <- max(count.fields(fileName, sep = '\t'))
    dat <- tibble::as_tibble(utils::read.delim(fileName,
                                               header = FALSE,
                                               fill = TRUE,
                                               blank.lines.skip = FALSE,
                                               col.names = paste0("V", 1:maxCols)))
  } else if (fileType=="xlsx") {
    dat <- suppressMessages(readxl::read_excel(fileName,
                                               col_names = FALSE,
                                               col_types = "text"))
  } else {
    stop("File type not recognised, extension must be csv, tsv, txt or xlsx.")
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
      plateDate <- dat[[2]][dateRow]
    }
  }
  # find plate reader:
  plateReaderRow <- which(dat[,1] == "Reader Type:")
  if (length(plateReaderRow) != 1) {
    stop("Couldn't impute plate reader from data file.")
  } else {
    plateReader <- dat[[2]][plateReaderRow]
  }

  # find setpoint temperature:
  setTRow <- which(dat[,1] == "Set Temperature")
  if (length(setTRow) != 1) {
    stop("Couldn't impute set temperature from data file.")
  } else {
    setT <- readr::parse_number(dat[[2]][setTRow])
  }

  # find start and end of data matrix:
  matrixStartCol <- 0
  matrixStartRow <- NA
  while(is.na(matrixStartRow)) {
    matrixStartCol <- matrixStartCol + 1
    matrixStartRow <- which(dat[15:nrow(dat), matrixStartCol] == "Time")[1] + 15
  }

  matrixEndRow <- which(is.na(dat[matrixStartRow:nrow(dat), matrixStartCol]) |
                        dat[matrixStartRow:nrow(dat), matrixStartCol] == "" |
                        dat[matrixStartRow:nrow(dat), matrixStartCol] == "Results")[1] + matrixStartRow - 2

  # constructing final imported data frame:
  processedDat <- dat[matrixStartRow:matrixEndRow, matrixStartCol:(matrixStartCol + 97)]
  if(fileType == "xlsx") {
    processedDat[,1] <- 24*60*as.double(dplyr::pull(processedDat,1))  # convert from day to minutes
  } else {
    processedDat[,1] <- (processedDat |>
      dplyr::pull(1) |>
      lubridate::hms() |>
      lubridate::period_to_seconds()) / 60
  }
  processedDat <- dplyr::mutate_all(processedDat, as.double)
  names(processedDat) <- dat[matrixStartRow - 1, matrixStartCol:(matrixStartCol + 97)] |>
    as.character()
  names(processedDat)[c(1,2)] <- c("Time_min", "Temperature")
  processedDat <- processedDat |>
    dplyr::mutate(Date=plateDate, PlateReader=plateReader, SetTemperature=setT, .before = 1)
  return(processedDat)
}


#' Import and process OD data
#'
#' \code{processODData} takes the path(s) of optical density (OD) data files and corresponding spec files, imports all those files, integrates spec and data files and returns a single, tidy tibble containing all the data.
#'
#' @param specPath The path of the spec files. Defaults to current working directory.
#' @param dataPath The path of the data files. Defaults to current working directory.
#' @param filePrefix The prefix of the data files. Defaults to "raw_", but this can be changed,
#' including to "" when all files in the dataPath directory may be treated as potential data files.
#'
#' @return A single, tidy tibble with complete data from all experiments and replicates.
#' @export
#'
processODData <- function(specPath = '.',
                          dataPath = '.',
                          filePrefix = "raw_") {

  # make sure the two paths exist and are properly defined:
  specPath <- fixPathName(specPath)
  dataPath <- fixPathName(dataPath)
  if (!dir.exists(specPath))
    stop(paste0("Couldn't find directory ", specPath, " for spec files."))
  if (!dir.exists(dataPath))
    stop(paste0("Couldn't find directory ", dataPath, " for data files."))

  specFileNames <- list.files(specPath)
  specFileNames <- specFileNames[startsWith(specFileNames, "spec_") &
                                 (endsWith(specFileNames, ".csv") | endsWith(specFileNames, ".xlsx"))
                                ]
  if (length(specFileNames) == 0)
    stop("No spec files found. Spec files need to be either csv or xlsx files and their names need to start with 'spec_'.")

  dataFileNames <- list.files(dataPath)

  allData <- data.frame() # create the allData frame before populating
  for(i in 1:length(specFileNames)) {
    specFileExtension <- fileExtension(specFileNames[i])
    fileName <- substr(specFileNames[i], 6, nchar(specFileNames[i]) - nchar(specFileExtension) - 1)
    fileName <- paste0(filePrefix, fileName)
    cat(paste0("Working on data file ", fileName, "..."))
    # check if corresponding data file is present:
    if (paste0(fileName, ".csv") %in% dataFileNames) {
      fileName <- paste0(fileName, ".csv")
    } else if (paste0(fileName, ".tsv") %in% dataFileNames) {
      fileName <- paste0(fileName, ".tsv")
    } else if (paste0(fileName, ".txt") %in% dataFileNames) {
      fileName <- paste0(fileName, ".txt")
    } else if (paste0(fileName, ".xlsx") %in% dataFileNames) {
      fileName <- paste0(fileName, ".xlsx")
    } else {
      fileName <- NA
      cat("no data file found!\n")
      warning("No data file corresponding to ", specFileNames[i], " found, was skipped.")
    }

    if (!is.na(fileName)) {
      if (specFileExtension == "csv") {
      specs <- suppressMessages(readr::read_csv(paste0(specPath, "/", specFileNames[i]),
                                                col_types = readr::cols(.default = "c")))
      } else if (specFileExtension == "xlsx") {
        specs <- readxl::read_excel(paste0(specPath, "/", specFileNames[i]))
      }
      trafoData <- importODFile(paste0(dataPath, "/", fileName)) |>
        tidyr::pivot_longer(cols = A1:H12, names_to = "Well", values_to = "OD") |>
        dplyr::mutate(Time_h = Time_min / 60) |>
        dplyr::left_join(specs, by = "Well") |>
        dplyr::relocate(Plate, Replicate, Date, PlateReader, SetTemperature, Row, Column, Well, WellType) |>
        dplyr::relocate(Time_min, Time_h, Temperature,  OD, .after = last_col())

      if (all(names(allData)==names(trafoData))) {
        allData <- rbind(allData, trafoData)
        cat(paste0("Working on data file ", fileName, "... done!\n"))
      } else {
        stop(paste0("Columns in ", specFileNames[i], " don't match with previous files."))
      }
    }
  }
  cat(" done!\n")
  return(allData)
}

