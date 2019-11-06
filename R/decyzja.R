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

#' takes the decision if a p value indicates statistical significance
#'
#' @param p -value, language
#' @return string with the "is" "is not" decision
#' @export
decision <- function(p,language="pl", liczba = "singular"){
  if (liczba == "singular"){
    if (language=="pl"){
      if (p<0.05) decision <- "jest" #istona statystycznie
      else decision <- "nie jest"
    } else {
      if (p<0.05) decision <- "is" #statistically significant
      else decision <- "is not"
    }
  } else {
    if (language=="pl"){
      if (p<0.05) decision <- "są" #istona statystycznie
      else decision <- "nie są"
    } else {
      if (p<0.05) decision <- "are" #statistically significant
      else decision <- "are not"
    }
    return(decision)
  }
}
