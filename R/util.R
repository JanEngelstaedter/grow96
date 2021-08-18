


fileExtension <- function (fileName)
{
  pos <- regexpr("\\.([[:alnum:]]+)$", fileName)
  ifelse(pos >=0, substring(fileName, pos + 1), "")
}
