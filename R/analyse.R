
#' Estimate growth parameters for a single vector of OD measurements
#'
#' @param times Time points where OD measurements have been taken
#' @param ODs OD measurements, ideally blanked
#' @param method Currently, only the default value "easylinear" is supported
#' @param ... Further arguments to be passed on to the method used
#'
#' @return A vector of estimated growth parameters
#'
#' @keywords internal
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


#' Analyse a dataset of OD reads through time
#'
#' @param data The data in tidy format, as produced by the function \code{processODData}
#' @param method Method to be used to analyse the OD data. Currently, the only supported method is "easylinear",
#' which calls the fit_easylinear function from the growthrates package (Petzold 2020).
#' @param tmin Time point from which onwards the OD data should be analysed. Defaults to \code{0}.
#' @param tmax Time point up to which the OD data should be analysed. Defaults to \code{Inf}.
#' @param r2Threshold Threshold of r2 (variance explained) for model fitting. If the model fitted
#' (e.g., linear function fitted to log(OD)) explains less variance than specified in this way,
#' the parameters estimated from this fit will not be discarded and NAs reported instead.
#' @param ... Other parameters that may be passed on to the methods function.
#'
#' @return A list of five data frames. The first data frame contains the estimated parameters for
#' each OD-through-time dataset. The other four data frames contain the following summary statistics
#' across replicate experiments: mean, standard deviations, standard errors of the means, and number of replicate
#' data points included in each summary statistic.
#'
#' @export
#'
#' @references Thomas Petzoldt (2020). growthrates: Estimate Growth Rates from Experimental Data. R package version
#' 0.8.2. https://CRAN.R-project.org/package=growthrates

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
      dplyr::filter(Plate == growthParams$Plate[i] &
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
      dplyr::mutate(mumax = ifelse(r2<r2Threshold, NA, mumax)) %>%
      dplyr::mutate(lag = ifelse(r2<r2Threshold, NA, lag))
  }

  cat("Summarising growth parameters ...\n")
  growthParamMeans <- growthParams %>%
    dplyr::group_by(dplyr::across(variableNames)) %>%
    dplyr::summarise(dplyr::across(names(gps), mean, na.rm = TRUE), .groups = 'drop')
  growthParamSDs <- growthParams %>%
    dplyr::group_by(dplyr::across(variableNames)) %>%
    dplyr::summarise(dplyr::across(names(gps), stats::sd, na.rm = TRUE), .groups = 'drop')
  growthParamSEs <- growthParams %>%
    dplyr::group_by(dplyr::across(variableNames)) %>%
    dplyr::summarise(dplyr::across(names(gps), se, na.rm = TRUE), .groups = 'drop')
  growthParamN <- growthParams %>%
    dplyr::group_by(dplyr::across(variableNames)) %>%
    dplyr::summarise(dplyr::across(names(gps), nNonNAs), .groups = 'drop')
  return(list(pars = growthParams,
              means = growthParamMeans,
              SDs = growthParamSDs,
              SEs = growthParamSEs,
              n = growthParamN))
}
