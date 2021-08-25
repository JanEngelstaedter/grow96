
#' Estimate growth parameters for a single vector of OD measurements
#'
#' @param times Time points where OD measurements have been taken
#' @param ODs OD measurements, ideally blanked
#' @param method Currently, only the default value "easylinear" is supported
#' @param ...
#'
#' @return A vector of estimated growth parameters
#'
getGrowthParameters <- function(times, ODs, method = "easylinear", ...) {
  if (method == "easylinear") {
    fit <- tryCatch(growthrates::fit_easylinear(times, ODs, ...),
                    error=function(e) NA)
    if (!isS4(fit))
      return(c(mumax = NA, lag = NA, r2 = NA, maxOD = max(ODs)))
    else {
      return(c(growthrates::coef(fit)[3:4],
               growthrates::rsquared(fit),
               maxOD = max(ODs)))
    }
  } else {
    stop(paste("Method",method,"is not supported"))
  }
}


#' Title
#'
#' @param data
#' @param method
#' @param tmin
#' @param tmax
#' @param r2Threshold
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
analyseODData <- function(data,
                          method = "easylinear",
                          tmin = 0,
                          tmax = Inf,
                          r2Threshold = 0.5,
                          ...) {
  growthParams <- data %>%
    dplyr::group_by(Plate, Replicate, Well, WellType) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(!(Time_min:blankedOD)) %>%
    dplyr::filter(WellType == "DATA") %>%
    dplyr::select(!WellType) %>%
    as.data.frame()

  # determine names of the two variables, as names of last two columns:
  variableNames <- names(growthParams)
  variableNames <- variableNames[length(variableNames) - c(1,0)]

  # restrict data to time range:
  data <- dplyr::filter(data, Time_min >= tmin, Time_min <=tmax)

  cat("Calculating growth parameters ...\n")
  for(i in 1:nrow(growthParams)) {
    dataSubset <- data %>%
      filter(Plate == growthParams$Plate[i] &
               Replicate == growthParams$Replicate[i] &
               Well == growthParams$Well[i])
    gps <- getGrowthParameters(dataSubset$Time_min, dataSubset$blankedOD)
    growthParams[i, names(gps)] <- gps
    progressPercent <- floor(i/nrow(growthParams)*20)
    cat('\r', paste0(c("0% [", rep("|", progressPercent), rep(" ", 20-progressPercent), "] 100%"), collapse = ""))
    if (i==nrow(growthParams)) {
      cat("\n")
    }
  }

  # discarding estimates with poor fit:
  if (method == "easylinear") {
    growthParams %>%
      mutate(mumax = ifelse(r2<r2Threshold, NA, mumax)) %>%
      mutate(lag = ifelse(r2<r2Threshold, NA, lag))
  }

  cat("Summarising growth parameters ...\n")
  growthParamMeans <- growthParams %>%
    dplyr::group_by(across(variableNames)) %>%
    summarise(across(names(gps), mean, na.rm = TRUE), .groups = 'drop')
  growthParamSDs <- growthParams %>%
    dplyr::group_by(across(variableNames)) %>%
    summarise(across(names(gps), sd, na.rm = TRUE), .groups = 'drop')
  growthParamSEs <- growthParams %>%
    dplyr::group_by(across(variableNames)) %>%
    summarise(across(names(gps), se, na.rm = TRUE), .groups = 'drop')
  growthParamN <- growthParams %>%
    dplyr::group_by(across(variableNames)) %>%
    summarise(across(names(gps), nNonNAs), .groups = 'drop')
  return(list(pars = growthParams,
              means = growthParamMeans,
              SDs = growthParamSDs,
              SEs = growthParamSEs,
              n = growthParamN))
}
