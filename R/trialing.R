
border <- "EMPTY"
rows <- c("STP", "RIF", "TMP", "KAN", "CIP", "NONE")
columns <- paste0("Strain", 1:10)
constants <- c(Medium = "LB+")
plateName <- "test"
rowName <- "Drug"
columnName <- "Strain"
blankColumns <- NULL
blankRows <- 7

makeSpec_fullfact("test", "Drug", "Strain", drugs, strains,
                  border = "EMPTY",
                  borderStyle = "extendBoth",
                  blankRows = 7,
                  constants = constants,
                  replicates = 3,
                  randomise = "rows",
                  specPath = "./specs/",
                  plotPath = "./plots/")


makeSpec_fullfact(plateName = "Example1",
                  rowName = "concRIF", columnName = "concSTP",
                  rows = 0.025 * 0:7, columns = 0.05* 0:11)
