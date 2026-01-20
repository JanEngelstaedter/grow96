
# divine the extension from a file name:
fileExtension <- function (fileName)
{
  pos <- regexpr("\\.([[:alnum:]]+)$", fileName)
  return(ifelse(pos >=0, substring(fileName, pos + 1), ""))
}

# make sure a path name is properly formatted.
# strings like "myPath", "/myPath/" etc. will all be adjusted to "./myPath"
fixPathName <- function(path) {
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


# calculate a matrix of wrap indices:
# nwraps: number of wrap values
# group: which group to fill
# ngroup: number of groups
# nrow, ncol: dimensions of plate, defaults to 12x8
# border: should the plate have a border?
# extend: should the TRUE indices be extended to the end of the row?
wrap_indices <- function(nwraps, group, ngroups, nrow, ncol, border, extend = TRUE) {
  if (!is.null(border)) {
    nrow_group <- (nwraps - 1) %/% (ncol - 2) + 1  # number of rows per group
    if (extend) nwraps <- nrow_group * (ncol - 2)
    j <- 2 + (group - 1) * (6 %/% ngroups)  # row number where the wrap starts
    im <- matrix(FALSE, nrow = nrow, ncol = ncol)
    im_inner <- matrix(c(rep(TRUE, nwraps), rep(FALSE, (nrow_group) * (ncol - 2) - nwraps)),
                       nrow = nrow_group,
                       ncol = ncol - 2,
                       byrow = TRUE)
    im[j:(j + nrow_group - 1), 2:(ncol - 1)] <- im_inner
  } else {
    nrow_group <- (nwraps - 1) %/% ncol + 1  # number of rows per group
    if (extend) nwraps <- nrow_group * ncol
    j <- 1 + (group - 1) * ngroups  # row number where the wrap starts
    im <- matrix(FALSE, nrow = nrow, ncol = ncol)
    im_inner <- matrix(c(rep(TRUE, nwraps), rep(FALSE, nrow_group * ncol - nwraps)),
                       nrow = nrow_group,
                       ncol = ncol,
                       byrow = TRUE)
    im[j:(j + nrow_group - 1), 1:ncol] <- im_inner
  }
  return(im)
}
