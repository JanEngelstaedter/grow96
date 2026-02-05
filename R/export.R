
#' Export OD dataset for fitting consumer-resource models
#'
#' This function prepares an OD dataset for downstream fitting of consumer-resource models.
#' It takes as input a data frame with OD data, in tidy format as produced by the
#' processODData function. After some basic filtering (removing wells with `WellType == "EMPTY"`
#' and rows where either time or OD has NA values), it then adds four numeric ID columns to the data frame
#' that are essential for the fitting procedure:
#' * Plate_ID: A unique number for each individual plate (i.e., each unique combination of `PlateID` and `Replicate`)
#' * Timeseries_ID: A unique number for each time series (i.e. well) within a plate
#' * Inoculum_ID: A unique number for each independent inoculum
#' * Resource_ID: A unique number for each independently prepared growth medium
#'
#' Whilst `Plate_ID` and `Timeseries_ID` are determined automatically from default columns,
#' correct assignment of `Inoculum_ID` and `Resource_ID` may require the user to either adjust the function arguments
#' `inoculum_IDs` and `resource_IDs`, or, in more complex scenarios, to change the output data frame after function call.
#'
#' @param ODs Data frame containing OD data, as produced by the processODData function
#' @param inoculum_IDs A character string specifying columns in the ODs data frame that determine
#' which wells were inoculated with the same culture.
#' For example, with the default value, `inoculum_IDs = c("Plate", "Replicate", "Strain")`, all wells with unique
#' combinations of values in the Plate, Replicate and Strain columns are assumed to be have been inoculated from the
#' same culture.
#' @param resource_IDs A character string specifying columns in the ODs data frame that determine
#' which wells have the identical growth medium.
#' For example, with the default value, `resource_IDs = c("Plate", "Replicate")`, all wells with unique
#' combinations of values in the Plate and Replicate columns are assumed to contain growth media that
#' is identical in terms of composition and batch,
#' but all media in the different wells within a single plate is assumed to be identical.
#' @returns A data frame of OD values,
#' with some rows filtered out and some additional columns added compared to the input data frame.
#' @export
#'
export_for_CRfitting <- function(ODs,
                                 inoculum_IDs = c("Plate", "Replicate", "Strain"),
                                 resource_IDs = c("Plate", "Replicate")) {

  # check if column names match inoculum_IDs:
  for(iID in inoculum_IDs) {
    if (!(iID %in% colnames(ODs))) {
      stop(paste0("Undetected inoculum_IDs value: ", iID))
    }
  }

  # check if column names match resource_ID:
  for(rID in resource_IDs) {
    if (!(rID %in% colnames(ODs))) {
      stop(paste0("Undetected resource_IDs value: ", rID))
    }
  }

  # remove unwanted rows and deal with zero time points:
  ODs <- ODs |>
    dplyr::filter(WellType != 'EMPTY') |>
    dplyr::filter(!is.na(Time_h), !is.na(OD), !(Time_h == 0)) |>
    dplyr::mutate(Time_h = dplyr::if_else(Time_h!=0, Time_h, .Machine$double.eps))

  # add an ID for each plate:
  ODs <- ODs |>
    dplyr::group_by(Plate, Replicate) |>
    dplyr::mutate(Plate_ID = dplyr::cur_group_id()) |>
    dplyr::ungroup()

  # add an ID for each time series:
  ODs <- ODs |>
    dplyr::group_by(Plate, Replicate, Well) |>
    dplyr::mutate(Timeseries_ID = dplyr::cur_group_id()) |>
    dplyr::ungroup() |>
    dplyr::arrange(Plate_ID, Timeseries_ID, Time_h)

  # add an ID for each independent inoculum source:
  ODs <- ODs |>
    dplyr::group_by(across(all_of(inoculum_IDs))) |>
    dplyr::mutate(Inoculum_ID = dplyr::cur_group_id()) |>
    dplyr::ungroup()

  # add an ID for each independent growth medium:
  ODs <- ODs |>
    dplyr::group_by(dplyr::across(tidyselect::all_of(resource_IDs))) |>
    dplyr::mutate(Resource_ID = dplyr::cur_group_id()) |>
    dplyr::ungroup()

  return(ODs)
}

