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

#' takes a multiple logistic regression model and returns the string stating whether the model contains any statistically significant covariates - as a rule there are no whole-model tests or p-values for generalized linear models
#'
#' @param glm model
#' @return string stating whether the model contains any statistically significant covariates
#' @export
getPFromMultipleGLM <- function(model, lang = "pl"){
  rounding <- function(){3}
  result <- c()
  values_p <- summary(model)$coefficients[,4, drop=FALSE]
  if(nrow(values_p)==1)
    values_p <- rbind(values_p, c(1))
  for (i in 2:length(values_p)){
    if(values_p[i]<0.05)
      result <- c(result, values_p[i])
  }
  if (lang == "pl"){
    if (length(result)>0){
      if (length(result)==1){
        line <- paste("zawiera istotne statystycznie zmienną: ", paste(names(result), collapse = ","),
                      ", wartość <em>p</em> wynosi", round(result, rounding()))
      } else {
        line <- paste("zawiera istotne statystycznie zmienne: ", paste(names(result), collapse = " ,"),
                      ", wartości p, odpowiednio", paste(round(result, rounding()), collapse = ","))
      }
    } else { line  <- "nie zawiera istotnych statystycznie zmiennych" }
  } else {
    if (length(result)>0) {
      if (length(result)==1) {
        line <- paste("contains one statistically significant covariate: ", paste(names(result), collapse = ","),
                      ", the <em>p</em>-value is", round(result, rounding()))
      } else {
        line <- paste("contains statistically significant covariates: ", paste(names(result), collapse = ","),
                      ", the <em>p</em>-values are, respectively", paste(round(result, rounding()), collapse = ","))}
    } else {
      line  <- "does not contain any statistically significant covariates"
    }
  }
  return(line)
}
