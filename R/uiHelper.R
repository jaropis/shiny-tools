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

#' Wybor colorFunctionow do rysunkow
#'
#' Nie przyjmuje parametrow
#'
#' @return funkcja SelectInput
#' @export
colorFunctionSelector <- function(){
selectInput("colorFunction", "colorFunction:",
            list("pomarańczowy" = "orange",
                 "biały"="white",
                 "szary" = "gray", 
                 "czerwony"="#CC6666",
                 "fioletowy"="#9999CC",
                 "zielony"="#66CC99"))
}

#' tekst informacyjny
#'
#' @param text tekst w HTML do wyswietlenia w panelu shiny'ego
#' @return funkcja HTML
#' @export
creationInfo <- function(tekst="<br>Program napisany przez <a href=\"http://imyData.pl\" target=\"_blank\">Jarosław Piskorski-imyData.pl</a>."){
    return(HTML(text))
}
