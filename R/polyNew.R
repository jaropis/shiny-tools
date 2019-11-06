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

#' this is just the poly function without the safeguards on the degree of polyomial, which some consider a bug - in this way I am able to fit the polynomial of order n-1 (n - number of datapoints)
#'
#' @param x-vector and polynomial degree
#' @return whatever poly returns
#' @export
polyNew <- function (x, ..., degree = 1, coefs = NULL, raw = FALSE) 
{
  dots <- list(...)
  if (nd <- length(dots)) {
    if (nd == 1 && length(dots[[1L]]) == 1L) 
      degree <- dots[[1L]]
    else return(polym(x, ..., degree = degree, raw = raw))
  }
  if (is.matrix(x)) {
    m <- unclass(as.data.frame(cbind(x, ...)))
    return(do.call("polym", c(m, degree = degree, raw = raw)))
  }
  if (degree < 1) 
    stop("'degree' must be at least 1")
  if (anyNA(x)) 
    stop("missing values are not allowed in 'poly'")
  n <- degree + 1
  if (raw) {
    Z <- outer(x, 1L:degree, "^")
    colnames(Z) <- 1L:degree
    attr(Z, "degree") <- 1L:degree
    class(Z) <- c("poly", "matrix")
    return(Z)
  }
  if (is.null(coefs)) {
    xbar <- mean(x)
    x <- x - xbar
    X <- outer(x, seq_len(n) - 1, "^")
    QR <- qr(X)
    z <- QR$qr
    z <- z * (row(z) == col(z))
    raw <- qr.qy(QR, z)
    norm2 <- colSums(raw^2)
    alpha <- (colSums(x * raw^2)/norm2 + xbar)[1L:degree]
    Z <- raw/rep(sqrt(norm2), each = length(x))
    colnames(Z) <- 1L:n - 1L
    Z <- Z[, -1, drop = FALSE]
    attr(Z, "degree") <- 1L:degree
    attr(Z, "coefs") <- list(alpha = alpha, norm2 = c(1, 
                                                      norm2))
    class(Z) <- c("poly", "matrix")
  }
  else {
    alpha <- coefs$alpha
    norm2 <- coefs$norm2
    Z <- matrix(, length(x), n)
    Z[, 1] <- 1
    Z[, 2] <- x - alpha[1L]
    if (degree > 1) 
      for (i in 2:degree) Z[, i + 1] <- (x - alpha[i]) * 
      Z[, i] - (norm2[i + 1]/norm2[i]) * Z[, i - 1]
    Z <- Z/rep(sqrt(norm2[-1L]), each = length(x))
    colnames(Z) <- 0:degree
    Z <- Z[, -1, drop = FALSE]
    attr(Z, "degree") <- 1L:degree
    attr(Z, "coefs") <- list(alpha = alpha, norm2 = norm2)
    class(Z) <- c("poly", "matrix")
  }
  Z
}
