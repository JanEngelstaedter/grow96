


# divine the extension from a file name:
fileExtension <- function (fileName)
{
  pos <- regexpr("\\.([[:alnum:]]+)$", fileName)
  ifelse(pos >=0, substring(fileName, pos + 1), "")
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

# extract the core name from a spec file name:
nameFromSpecFileName <- function(specFileName) {
  name <- substr(specFileNames[i], 6, nchar(specFileNames[i]) - 4)
  substr(name, 1, 5) <- "?"
  substr(name, nchar(name) - 3, nchar(name)) <- ""
  return(name)
}

# standard error:
se <- function(x, na.rm=FALSE) {
  if (na.rm) x <- stats::na.omit(x)
  return(sqrt(stats::var(x)/length(x)))
}

# count how many non-NA values a vector has:
nNonNAs <- function(x) sum(!is.na(x))

