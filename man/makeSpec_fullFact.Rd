% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeSpec.R
\name{makeSpec_fullFact}
\alias{makeSpec_fullFact}
\title{Generate a plate specification based on a full factorial design}
\usage{
makeSpec_fullFact(
  plateName,
  rowName,
  columnName,
  rows,
  columns,
  border = NULL,
  borderStyle = "NAs",
  blankRows = NULL,
  blankColumns = NULL,
  emptyRows = NULL,
  emptyColumns = NULL,
  replicates = NULL,
  constants = NULL,
  randomise = NULL,
  specPath = ".",
  makePlot = TRUE,
  plotPath = specPath
)
}
\arguments{
\item{plateName}{Name of the plate design}

\item{rowName}{Name of the variable for the rows. Typical values could be "strain", "medium" or "drug".}

\item{columnName}{Name of the variable for the columns. Typical values could be "strain", "medium" or "drug".}

\item{rows}{Values of the row variable.}

\item{columns}{Values of the column variable.}

\item{border}{Specifies whether the border wells should be treated differently. With the default value \code{NULL} there is no border and all wells can be used to generate data. With the value \code{'EMPTY'} the wells on the border will be designated as wellType=EMPTY, which means the OD data will be ignored in subsequent analyses. With the value \code{'BLANK'} these wells are designated as wellType=BLANK, which means they can be used for blanking. See below for details.}

\item{borderStyle}{Specifies how to fill the border wells, provided that a border has been defined with the \code{border} parameter. A value of \code{'NAs'} (the default) add NAs for all variables on the entire border. A values of \code{'extendRows'} extends the row variable and all constant variables to the first and last column of the plate, whereas the remainder of the border will be filled with NAs. A values of \code{'extendColumns'} extends the column variable and all constant variables to the first and last row of the plate, and a value of \code{'both'} does both types of extensions.}

\item{blankRows}{A vector of one or more row numbers that will be used for blanking. By default (value \code{NULL}) no blank rows are introduced.}

\item{blankColumns}{vector of one or more column numbers that will be used for blanking. By default (value \code{NULL}) no blank columns are introduced.}

\item{emptyRows}{A vector of one or more row numbers that will be left empty. By default (value \code{NULL}) no blank rows are introduced.}

\item{emptyColumns}{vector of one or more column numbers that will be left empty. By default (value \code{NULL}) no blank columns are introduced.}

\item{replicates}{Number of replicate plate designs to generate. By default (value \code{NULL}), only a single plate design will be generated. When specified as an integer number, this number of spec files will be generated, and for each replicate number will appear in the file name as well as a column in the table.}

\item{constants}{Names and values of additional variables that are constant across the plate (see Details).}

\item{randomise}{Whether to randomise rows and/or columns. Possible values are \code{NULL} (no ramdomisation, the default), \code{'rows'}, \code{'columns'}, and \code{'both'}.}

\item{specPath}{Path where to save the spec file(s).}

\item{makePlot}{Whether to produce a pdf file showing the plate design. Defaults to \code{TRUE}.}

\item{plotPath}{Path where to save the spec plot file(s). Defaults to \code{path}.}
}
\description{
\code{makeSpec_fullFact} produces "spec-files": csv files containing tables that assign values of two (or more) variables to each well on a 96-well plate. The design used is one where one variable varies across rows and another across column, in a full factorial design.
}
\details{
The main output of this function is a csv file with a table in which rows are the 96 wells of a standard microtitre plate. This table will have, at the minimum, the following columns: "Plate", "Row",	"Column",	"Well",	"WellType", <\code{rowName}>, and <\code{columnName}>, where the latter two are specified by the corresponding arguments of the function. The "Plate" column contains the name of the plate design in all rows, as specified by the \code{plateName} argument. "Row", "Column" and "Well" contains the coordinates for each well, in the standard format A...H, 1...12, and "A1...H12". The "WellType" column specifies how a well should be treated in subsequent data analyses. The default value "DATA" means the OD values from this well should be treated as normal growth data, a value "BLANK" indicates that the well should be used for blanking, and a value "EMPTY" indicates that the well should be ignored. "BLANK" and "EMPTY" values are generated automatically when a border is specified (argument \code{border}), and when empty/blank rows or columns are defined by the respective arguments \code{blankRows}, \code{blankColumns}, \code{emptyRows}, and \code{emptyColumns}. Finally, the last two columns contain the values of the row and column variables for each well.

In addition to these default columns, an optional second column "Replicate" is generated when several replicate plates are defined using the parameter \code{replicates}. This means that several spec files will be generated (one for each replicate), and the column "Replicate" then contains the same replicate number for all wells in each of these files. The replicate number is also attached to the file name of all csv files.

Finally, it is possible to add one or several columns for variables that are constant in a plate design. This might be useful if several different plate designs are to be combined in a larger analysis where the values of the constant variables may differ between plates, or simply as a way to store additional information about the plate. These columns are added using the \code{constants} argument, which should be a named vector in which each element corresponds to a column and where the value of that element defines the value of the variable and the name of the element the name of the variable (i.e. column).

The \code{randomise} argument specifies if row and/or column variables should be randomised. If this is the case, and if several replicate plate designs are generated, each replicate will have a different order of values in the rows and/or columns. (Note though that the function does not check if all plates are actually different, but given the large number of possible permutations this is very likely. E.g., even if only the six interior rows are used and randomised, there are 6!=720 possible permutations and hence with <10 replicates the chance of randomly generating two identical plates is small.)

In addition to the main output, the spec file(s), the \code{makeSpec_fullFact} function also (by default) generates pdf files showing the plate design, which may be useful for checking that the function did what the user desired to do, and also may help filling the plates during the experiment. These pdf files can be saved in a separate folder as the spec files, as specified by the \code{plotPath} vs. \code{specPath} arguments.
}
\examples{

# simple example:
#makeSpec_fullFact(plateName = "Example1",
#                  rowName = "concRIF", columnName = "concSTP",
#                  rows = 0:7, columns = 0:11)

# more complex design:
# makeSpec_fullFact(plateName = "Example2",
#                   rowName = "Drug",
#                   columnName = "Strain",
#                   rows = c("STP", "RIF", "TMP", "KAN", "CIP", "NONE"),
#                   columns = paste0("Strain", LETTERS[1:10]),
#                   border = "EMPTY",
#                   blankRows = 7,
#                   replicates = 3,
#                   randomise = "rows")

}
