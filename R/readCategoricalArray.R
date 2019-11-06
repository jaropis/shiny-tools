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


#' function for reading a data frame copied and pasted from Excel to numericInput shiny field
#'
#' @param accepts a string which is the result of pasting a selection from Excel
#' @return returns a data frame corresponding to the selection in Excel. All entriest are strings
#' @export
readCategoricalArray <- function(numericInputString){
    rResult <- strsplit(numericInputString, " ")[[1]]
    rResult <- t(unname(sapply(matrix(rResult), splitToNumbers)))
    
    ## the names of the columns
    nNames <- rResult[1,]
    firstLine <- 2

    startingFrame <- data.frame(rResult[firstLine:length(rResult[,1]),1])
    for (i in 1:length(rResult[1,]))
        startingFrame[[i]] <- rResult[,i][firstLine:length(rResult[,1])]
    ### now I will cut out the names line
    names(startingFrame) <- nNames
    print(startingFrame)
    return(startingFrame)
}
