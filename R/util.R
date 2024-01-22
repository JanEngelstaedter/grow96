
# divine the extension from a file name:
fileExtension <- function (fileName)
{
  pos <- regexpr("\\.([[:alnum:]]+)$", fileName)
  return(ifelse(pos >=0, substring(fileName, pos + 1), ""))
}

# make sure a path name is properly formatted.
# strings like "myPath", "/myPath/" etc. will all be adjusted to "./myPath"
fixPathName <- function(path) {
  if (path == '.') {
    return(path)
  } else if (substr(path, 1, 1) == '/') {
    path <- paste0('.', path)
  } else if (substr(path, 1, 2) != './') {
    path <- paste0('./', path)
  }
  if (substr(path, nchar(path), nchar(path))=="/")
    path <- substr(path, 1, nchar(path) - 1)
  return(path)
}

# standard error:
se <- function(x, na.rm=FALSE) {
  if (na.rm) x <- stats::na.omit(x)
  return(sqrt(stats::var(x)/length(x)))
}

# count how many non-NA values a vector has:
nNonNAs <- function(x) sum(!is.na(x))


# calculate a mean after excluding outliers:
meanNoOutliers <- function(x, tukeyK = NULL, na.rm = FALSE) {
  if (is.null(tukeyK)) {
    return(mean(x, na.rm = na.rm))
  } else {
    q <- quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = na.rm)
    x <- x[(x > (q[1] - tukeyK * (q[3] - q[1]))) & (x < (q[3] + tukeyK * (q[3] - q[1])))]
    return(mean(x, na.rm = na.rm))
  }
}
