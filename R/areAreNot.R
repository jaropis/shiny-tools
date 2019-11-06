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

#' function for returning a line to the report of ChiSquared tabel test and Fisher table test
#'
#' @param accepts a P value
#' @return returns a formatted string
#' @export
areAreNot <- function(P, language = "pl"){
  if (language == "pl"){
    if (P<0.05) rReturn <- "istnieją istotnie statystyczne związki pomiędzy rRowami a cColumnmi - myTTable nie jest losowa"
    else rReturn <- "nie ma istotnych statystyczne związków pomiędzy rRowami a cColumnmi - myTTable jest losowa"
  } else {
    if (P<0.05) rReturn <- "there are statistically significant relations between the rows and the columns - the table is not random"
    else rReturn <- "there are no statistically significant relations between the rows and the columns - the table is random"
  }
  return(rReturn)
}
