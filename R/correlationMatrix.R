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

#' oblicza values p i cCoefolczynniki r/rho dla macierzy korelacji
#'
#' @param myData jako dataframe
#' @return macierz tekstowa z valuesami r/rho (gora) i p (dol)
#' @export

corMatrix <- function(myDataFrama, method = "p"){
  oldOptions <- options(scipen=10)
  rRow <- 1
  cColumn <- 1
  dDimension <- length(myDataFrama)
  correlationMatrix <- matrix("1", nrow=dDimension, ncol=dDimension)
  row.names(correlationMatrix) <- colnames(myDataFrama)
  colnames(correlationMatrix) <- colnames(myDataFrama)
  for (i in 1:dDimension){
    for(j in i:dDimension){
      cCorrelation <- cor.test(myDataFrama[[i]], myDataFrama[[j]], method = method)
      if (i!=j){
        correlationMatrix[i,j] <- as.character(round(cCorrelation$estimate,4))
        correlationMatrix[j,i] <- "p<0.0001"
        if (cCorrelation$p.value>0.0001){
          correlationMatrix[j,i] <- paste("p=",as.character(round(cCorrelation$p.value,4)), sep = "")
        }
        if (cCorrelation$p.value>=0.05)
          correlationMatrix[j,i] <- "NS"
      }
    }
  }
  options(oldOptions)

  return(as.data.frame(correlationMatrix, stringsAsFactors = FALSE))
}

panelSmooth <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
    points(x, y, pch = 21, col = "black", bg = "gray", cex = 0.8)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) {
      abline(lm(y~x), lty=2, lwd=2)
      lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
            col = col.smooth, lwd=2, ...)
      }
}
panelCor <- function(x, y, digits=2, prefix="", cex.cor) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    #r <- abs(cor(x, y))
    cCorrelation <- cor.test(x,y, method = methodUpper)## ok, I am abusing scoping rules and reaching for the method to the calling function
    r <- cCorrelation$estimate
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 1.5/strwidth(txt)

    test <- cor.test(x,y)
    # borrowed from printCoefmat
    Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", " "))

    text(0.45, 0.45, txt, cex = cex * (1.5-7/6*r)*r)
    text(.75, .75, Signif, cex=cex/2.7, col=2)
}

#' rysuje macierz korelacji wraz z linią loess - pozyczylem troche kodu z internetu - musze sprawdzic skad
#'
#' @param przyjmuje dataFrame z danymi, tTitle calej analizy, oraz nNames kolumn
#' @return zwraca rysunek - correlation matrix z odpowiednio sformatowanymi panelami
#' @export
correlationDraw <- function(myDataFrama, tTitle="", nNames=names(myDataFrama), method = "p"){
  methodUpper <<- method # this is an ugly hack - I am defining a variable in the package environment in which the functions above are defined (lexical scoping) - this is because I do not know how to give parameters to the panel.upper function
    pairs(myDataFrama, lower.panel=panelSmooth, upper.panel=panelCor,
          main=tTitle, labels=nNames)
}
