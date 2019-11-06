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

#' function for reading a 2x2 table with row and column names
#'
#' @param accepts a string which is the result of pasting a selection from Excel
#' @return returns a matrix corresponding to the selection in Excel - a 
#' @export
readTable2by2 <- function(numericInputString){
  rResult <- strsplit(numericInputString, " ")[[1]]
  rResult <- t(unname(sapply(matrix(rResult), splitToNumbers)))
  nNamesKolumn <- c(rResult[1,2], rResult[1,3])
  nNamesWierszy <- c(rResult[2,1], rResult[3,1])
  myTable <- matrix(c(as.numeric(rResult[2,2]), as.numeric(rResult[3,2]), 
                      as.numeric(rResult[2,3]), as.numeric(rResult[3,3])),ncol = 2 )
  rownames(myTable) <- nNamesWierszy
  colnames(myTable) <- nNamesKolumn
  return(myTable)
}
