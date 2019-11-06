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

#' function for returning a line to the report of ChiSquared if there are small values in the analysed table
#'
#' @param accepts a logical variable, TRUE means that there are small (<5) values in the table
#' @return returns a formatted string
#' @export
smallValues <- function(small, language = "pl"){
  rReturn <- ""
  if (small & language == "pl") rReturn <- " Wyniki mogą być niepoprawne, ponieważ w tabeli znajdują się wartości < 5. Proszę spróbować testu Fishera."
  if (small & language == "en") rReturn <-  " The results may be incorrect as the table contain entries which are <5. Try the exact Fisher test."
  return(rReturn)
}
