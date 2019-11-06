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

#' takes a 1X2 vector and formats it as a string with confidence interval
#'
#' @param x -1x2 vector
#' @return string confidence interval in brackets
#' @export
getConfInt <- function(x){
  rounding <- function(){3} ## I have a purpose here
  return(paste("(",round(x[1],rounding()),", ",round(x[2],rounding()),")", sep=""))
}
#' takes a nX2 vector and formats it as a string vector with confidence interval
#'
#' @param x -nx2 vector
#' @return string confidence interval in brackets vector
#' @export
getConfInt2 <- function(x,y){
  rounding <- function(){3} ## I have a purpose here
  if (is.null(ncol(x)))
    x <- matrix(x, ncol=2)
  return(apply(x, 1, function(x){paste("(",round(x[1], rounding())," , ",round(x[2],rounding()),")", sep="")}))
}
