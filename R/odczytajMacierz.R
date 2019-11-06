### This file is part of PCSS's Run Test suite.

### Run Test is free software: you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation, either version 3 of the License, or
### (at your option) any later version.

### Run Test is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.

### You should have received a copy of the GNU General Public License
### along with Run Test.  If not, see <http://www.gnu.org/licenses/>.
### Run Test is free software: you can redistribute it and/or modify
### it under the terms of the GNU General Public License as published by
### the Free Software Foundation, either version 3 of the License, or
### (at your option) any later version.

### Run Test is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.

### You should have received a copy of the GNU General Public License
### along with Run Test.  If not, see <http://www.gnu.org/licenses/>.

dDotIt <- function(sString) {
  paste(strsplit(sString, ",")[[1]], collapse=".")
}

splitToNumbers <- function(sString){
  strsplit(sString, "\t")[[1]]
}

#' function for reading a data frame copied and pasted from Excel to numericInput shiny field
#'
#' @param accepts a string which is the result of pasting a selection from Excel
#' @return returns a data frame corresponding to the selection in Excel. Interprets whther a column is numeric or factor
#' @export
readMatrix <- function(numericInputString){
    rResult <- strsplit(numericInputString, " ")[[1]]
    rResult <- unname(sapply((matrix(rResult)), dDotIt))
    rResult <- t(unname(sapply(matrix(rResult), splitToNumbers)))
    ### a lot of casting below so:
    options(warn=-1)
    ## now I will try to see if the columns have names
    nNames <- rResult[1,]
    for (i in 1:length(nNames)){
      nNames[1]=paste("x",i, sep="")
    }
    firstLine <- 1
    if (all(is.na(as.numeric(rResult[1,])))){
      nNames <- rResult[1,]
      firstLine <- 2
    }
    ##
    startingFrame <- data.frame(rResult[firstLine:length(rResult[,1]),1])

    ### now I will cut out the names line, if there is any
    rResult <- rResult[firstLine:length(rResult[,1]), 1:length(rResult[1,])]
    ### now I will write to a data.frame, deciding whether a column is a numerical vecotor or a factors vector
    for (i in 1:length(rResult[1,])){
      if (any(is.na(as.numeric(rResult[,i]))))
        startingFrame[[i]] <- as.factor(rResult[,i])
      else
        startingFrame[[i]] <- as.numeric(rResult[,i])
    }
    ### let's restore warnings
    options(warn=0)
    names(startingFrame) <- nNames
    return(startingFrame)
}
