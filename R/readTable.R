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

splitToNumbers <- function(sString){
  strsplit(sString, "\t")[[1]]
}
#' function for reading a contingency table copied and pasted from Excel to numericInput shiny field
#'
#' @param accepts a string which is the result of pasting a selection from Excel
#' @return returns a contingency table (matrix) corresponding to the selection in Excel .
#' @export
readTable <- function(numericInputString){
    rResult <- strsplit(numericInputString, " ")[[1]]
    rResult <- t(unname(sapply(matrix(rResult), splitToNumbers)))
    class(rResult) <- "integer"
    return(rResult)
}
