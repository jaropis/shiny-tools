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

#' function for formating the p-value as it is inserted into text documents
#'
#' @param accepts the numerical p-value and the boolean value indicating whether the output will be used in a RMarkdown document
#' @return returns a formated string corresponding to the p-value and RMarkdown setup
#' @export

puj <- function(p, rmarkdown = TRUE){
  if (rmarkdown ) {
    if (p<0.0001) return("<em>p</em><0.0001")
    else return(paste("<em>p</em>=",as.character(round(p,4)),sep=""))
  } else {
    if (p<0.0001) return("p<0.0001")
    else return(paste("p",as.character(round(p,4)), sep=""))
  }
}
