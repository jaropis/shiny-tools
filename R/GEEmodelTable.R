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

textuj_tabelke <- function(model_summary, round_to = 3){
  variableNames <- rownames(model_summary$coefficients)#[2:length(model_summary[,1])])
  myCoefficients <- c()
  pValues <- c()
  starlets <- c()
  for (i in 1:nrow(model_summary$coefficients)){
    cCoef <- as.character(round(model_summary$coefficients[i,1], round_to))
    if(model_summary$coefficients[i,4]<=0.001) {tekst <- "< 0.001"
    star <- "**"}
    if(model_summary$coefficients[i,4]<=0.0001) {texts <-"< 0.0001"
    star <- "***"}
    if(model_summary$coefficients[i,4]>0.001) {tekst <- as.character(round(model_summary$coefficients[i,4],3))
    star <- "*"}
    if(model_summary$coefficients[i,4]>0.05 & model_summary$coefficients[i,2]<=0.1) {tekst <- paste(as.character(round(model_summary$coefficients[i,4],3)),"NS")
    star <- "."}
    if(model_summary$coefficients[i,4]>0.1) {tekst <- "NS"
    star <- " "}
    myCoefficients <- c(myCoefficients,cCoef)
    pValues <- c(pValues,tekst)
    starlets <- c(starlets,star)
  }
  myTable <- data.frame(coeffs=myCoefficients, P.values=pValues, category=starlets)
  rownames(myTable) <- variableNames
  myTable
}

#' takes the summary of a gee (generalized estimating equations) model and formats a table
#'
#' @param geeSummary accepts summary(gee_model)
#' @return table
#' @export
tableGEESummary <- function(result_summary, round_to = 3){
  myTable <- textuj_tabelke(result_summary, round_to = round_to)
  return(myTable)
}
