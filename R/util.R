


fileExtension <- function (fileName)
{
  pos <- regexpr("\\.([[:alnum:]]+)$", fileName)
  ifelse(pos >=0, substring(fileName, pos + 1), "")
}


nameFromSpecFileName <- function(specFileName) {
  name <- substr(specFileNames[i], 6, nchar(specFileNames[i]) - 4)
  substr(name, 1, 5) <- "?"
  substr(name, nchar(name) - 3, nchar(name)) <- ""
  return(name)
}