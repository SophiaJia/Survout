#' competing risk helper function, crr
#'
#' Given a factor variable x, create an indicator matrix of dimension
#' length(x) x (nlevels(x)-1) dropping the column corresponding to the
#  baseline level (by default the first level is used as baseline).
#'@param x a Variable
#'@param baseline Reference level

#'@return x variable with some NA Values
#'@examples
#'
#'factor2ind(Sex,"M")
#'factor2ind(Sex)
#'
#'
#'x = gl(4, 2, labels = LETTERS[1:4])
#'factor2ind(x)
#'factor2ind(x, "C")
#'@export
#'@name factor2ind
#'
#'
factor2ind <- function(x, baseline)
{
  # Given a factor variable x, create an indicator matrix of dimension
  # length(x) x (nlevels(x)-1) dropping the column corresponding to the
  # baseline level (by default the first level is used as baseline).

  xname <- deparse(substitute(x))
  n <- length(x)
  x <- as.factor(x)
  if(!missing(baseline)) x <- relevel(x, baseline)
  X <- matrix(0, n, length(levels(x)))
  X[(1:n) + n*(unclass(x)-1)] <- 1
  X[is.na(x),] <- NA
  dimnames(X) <- list(names(x), paste(xname, levels(x), sep = ":"))
  return(X[,-1,drop=FALSE])
}





