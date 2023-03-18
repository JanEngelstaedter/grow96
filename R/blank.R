###############################################################################################################
# This file contains functions to blank OD data, i.e. to subtract OD values measured from bacteria-free wells
# from corresponding wells that do contain bacteria.
###############################################################################################################


# function to blank using BLANK values from plate
# (see wrapper blankODs function for details on the parameters.)
blankODsFromPlate <- function(data,
                              perTimePoint = TRUE,
                              groups = NULL,
                              tukeyK = NULL) {
  if (perTimePoint) {  # subtract mean OD from BLANK wells at each time point
    groups <- c("Plate", "Replicate", "Time_min", groups)
    blankMeans <- data |>
      dplyr::filter(WellType == "BLANK") |>
      dplyr::group_by_at(groups) |>
      dplyr::summarise(meanBlankOD = meanNoOutliers(OD, tukeyK = tukeyK), .groups = "drop")

    blankedData <- data |>
      dplyr::left_join(blankMeans, groups) |>
      dplyr::mutate(blankedOD = OD - meanBlankOD) |>
      dplyr::select(-meanBlankOD)
  } else {
    groups <- c("Plate", "Replicate", groups)
    blankMeans <- data |>
      dplyr::filter(WellType == "BLANK") |>
      dplyr::group_by_at(groups) |>
      dplyr::summarise(meanBlankOD = meanNoOutliers(OD, tukeyK = tukeyK), .groups = "drop")

    blankedData <- data |>
      dplyr::left_join(blankMeans, groups) |>
      dplyr::mutate(blankedOD = OD - meanBlankOD) |>
      dplyr::select(-meanBlankOD)
  }
  return(blankedData)
}

# function to calculate blanked ODs using fixed numbers provided
# (see wrapper blankODs function for details on the parameters.)
blankODsFixed <- function(data,
                     perTimePoint = TRUE,
                     values = NULL,
                     tukeyK = NULL) {

  if (is.vector(values) && length(values) == 1) {
    blankedData <- data |>
      mutate(blankedOD = OD - values)
  } else if (is.data.frame(values)) {
    if (!("blankOD" %in% names(values)))
      stop("Column 'blankOD' expected in 'values' data frame.")
    groups <- names(values)
    groups <- groups[groups != "blankOD"]
    if (perTimePoint) {
      if (!("Time_min" %in% groups))
        stop("Column 'Time_min' expected in 'values' data frame.")
    } else {
      if ("Time_min" %in% groups) {  # first average across time
        values <- values |>
          dplyr::group_by_at(groups) |>
          dplyr::summarise(blankOD = meanNoOutliers(blankOD, tukeyK = tukeyK), .groups = "drop")
        groups <- groups[groups != "Time_min"]
      }
    }
    blankedData <- data |>
      dplyr::left_join(values, groups) |>
      dplyr::mutate(blankedOD = OD - blankOD) |>
      dplyr::select(-blankOD)
  } else {
    stop("Argument 'values' needs to be either a single value or a data frame.")
  }
  return(blankedData)
}


#' Blank optical density data
#'
#' \code{blankODs} takes a data file (tibble) of optical density (OD) data and uses one of several methods to blank the OD data.
#'
#' Currently, two methods for blanking are implemented. When code{method} = "fromPlate" (the default),
#' OD values from wells on the same plate that are designated as type "BLANK" will be used for blanking.
#' If no groups are provided (argument \code{groups = NULL}, the default), OD values of all BLANK wells
#' will be averaged, and the blanked OD will then be calculated as the original OD in each DATA well,
#' minus the this average value. However, it is also possible to group the blanked values by one or more
#' of the variables in the data tibble, and then subtract the group average from the corresponding DATA well ODs.
#' For examples, if there is a variable 'Medium' in the tibble, then with \code{blankGroups = 'Medium'},
#' averages for blanking will be taken across all wells with \code{wellType="BLANK"} for each value of this column
#' (e.g. "LB", "M9" etc.), and subtracted from OD for data wells with the same 'Medium' values.
#'
#' When code{method} = "fixed", one or several blank OD values, supplied in the \code{blankValues} argument,
#' are subtracted from all corresponding DATA wells. In the simplest case, \code{blankValues} is just a single
#' number that is subtracted. However, \code{blankValues} can also be a data frame or tibble of values.
#' In this case, one of the columns needs to be named "blankOD" (the blank OD values), and additional columns
#' "Time_min" and/or columns for grouping. For example, if in the \code{data} tibble that is supplied there
#' is a variable "Medium" then the \code{blankValues} tibble could have a corresponding column "Medium"
#' specifying the different values for which blank ODs are provided.
#'
#' The code{perTimePoint} argument is applicable to both methods.
#' When code{perTimePoint} = "TRUE" (the default), blanking will be performed on a per-time-point basis,
#' i.e. at each time point, the blanking value at this time point is subtracted from the respective wells to be blanked.
#' When code{perTimePoint} = "FALSE", the OD of blank values is averaged across all time points (if there are several),
#' and then subtracted from ODs at all time points. Note that with this latter method,
#' any trend in OD in the BLANK wells will lead to spurious results because this trend will be averaged out.
#'
#' @param data A tibble containing OD data as produced by the function \code{processODdata}.
#' @param method Determines whether to blank using OD values on the same plate (code{method} = "fromPlate"),
#' or fixed OD values as specified in \code{blankValues} argument (code{method} = "fixed").
#' For more information, see Details below.
#' @param perTimePoint Determines whether blanking will be performed on a per-time-point basis (code{perTimePoint} = "TRUE"),
#' or whether blank OD values are averaged through time before being subtracted (code{perTimePoint} = "FALSE").
#' @param groups One or several columns in the \code{data} tibble by which blanking should be grouped.
#' This argument is only used when code{method} = "fromPlate". See Details below.
#' @param values OD value(s) for blanking. This argument is only used when code{method} = "valuesProvided".
#' See Details below.
#' @param tukeyK This is an argument for calculating the blanked ODs.
#' When averaging OD of BLANK wells (across wells and/or across time), outliers may be excluded.
#' Here, Tukey's fences method is used, excluding all OD values that are more than tukeyK times the
#' interquartile range away from the upper and lower quartile, respectively.
#' When kTukey is NULL (the default), no outliers will be excluded.
#'
#' @return The original \code{data} tibble with an additional column \code{blankedOD}.
#'
#' @export
#'
blankODs <- function(data,
                     method = "fromPlate",
                     perTimePoint = TRUE,
                     groups = NULL,
                     values = NULL,
                     tukeyK = NULL) {

  if (method == "fromPlate") {
    if(!is.null(values))
      warning("Argument 'values' will be ignored.")
    return(blankODsFromPlate(data, perTimePoint, groups, tukeyK))
  } else if (method == "fixed") {
    if (is.null(values))
      stop("Argument 'values' needs to be provided when method 'fixed' is used.")
    if(!is.null(groups))
      warning("Groups will be imputed from 'values' argument, so the 'groups' argument will be ignored.")
    return(blankODsFixed(data, perTimePoint, values, tukeyK))
  } else {
    stop("The method argument needs to be either 'fromPlate' or 'fixed'.")
  }
}

