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

library(rmeta)
textMulti <- function(fullResults){
    variableNames <- c(rownames(fullResults))
    myCoefficients <- c()
    pValues <- c()
    starlets <- c()
    iInterval <- c()
    for (i in 1:nrow(fullResults)){
        cCoef <- as.character(round(fullResults[i,1],3))
        if(fullResults[i,2]<=0.001) {tekst <- "< 0.001"
                              star <- "**"}
        if(fullResults[i,2]<=0.0001) {texts <-"< 0.0001"
                               star <- "***"}
        if(fullResults[i,2]>0.001) {tekst <- as.character(round(fullResults[i,2],3))
                                      star <- "*"}
        if(fullResults[i,2]>0.05 & fullResults[i,2]<=0.1) {tekst <- paste(as.character(round(fullResults[i,2],3)),"NS")
                            star <- "."}
        if(fullResults[i,2]>0.1) {tekst <- "NS"
                            star <- " "}
        myCoefficients <- c(myCoefficients,cCoef)
        pValues <- c(pValues,tekst)
        starlets <- c(starlets,star)
    }
    for (i in 1:nrow(fullResults)){
        iInterval <- c(iInterval,paste(as.character(round(fullResults[i,3],2)), "-", as.character(round(fullResults[i,4],2))))
    }
    myTable <- data.frame(coeffs=myCoefficients, P.values=pValues, CI95=iInterval, category=starlets)
    rownames(myTable) <- variableNames
    myTable
}

plotForest <- function(fullResults){
    #png(file="myTable.png", res=300, width=2000, height=2000)
    variableNames <- c(rownames(fullResults))
    textTable <- cbind(c("", "Variable", variableNames),
                             c("", "HR", as.character(round(fullResults[,1],2))),
                             c("", "lower", as.character(round(fullResults[,3],2))),
                             c("", "upper", as.character(round(fullResults[,4],2))))
    forestplot(textTable, c(NA,NA,fullResults[,1]), c(NA,NA,fullResults[,3]),c(NA,NA,fullResults[,4]), zero=1, col=meta.colors(box="royalblue",line="darkblue", summary="royalblue"))
    #dev.off()
}

#' takes the summary of a Cox model and formats a table with results as wel as plotting the forest plot
#'
#' @param tableCoxSummary accepts summary(coxModel)
#' @return table, plot
#' @export
tableCoxSummary <- function(result, shouldIPlot="no"){
    fullResults <- data.frame(coeff=result$conf.int[,1], "P value"=result$coefficients[,5], l95=result$conf.int[,3], u95=result$conf.int[,4])
    ##narysowanie forest
    if (shouldIPlot=="yes") {
        plotForest(fullResults)}
    myTable <- textMulti(fullResults)
    myTable
}

