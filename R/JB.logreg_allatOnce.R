#' A output table for univariable and multivariable logistic regression
#'
#'JB.logreg_allatOnce  return results for UMA and  MVA logistic regression in a list
#'@param D Dataset
#'@param xvar a vector of X variable colnames
#'@param yvar Y variable colname
#'@param xname X output label
#'@param yname Y output label
#'@examples
#'f1 = glm(as.matrix(y~.,family = binomial, data = tmp)
#'rs <- JB.logreg_multi(f1)
#'@export
#'@name JB.logreg_allatOnce
#'

JB.logreg_allatOnce <- function(D, xvar, yvar, xname, yname){
  ## UVA
  tmp  = D[xvar]
  ytmp = as.matrix(D[yvar])[,1]
  T2 <- JB.logreg_m(tmp, ytmp, xname, yname)
  ## MVA
  f1 <- JB.logreg_multi(glm(ytmp~.,family = binomial, data = tmp))
  T3 <- T2
  T3[which(T3[,5] != "Ref"),5] <- f1[,1]
  T3[which(T3[,5] != "Ref"),6] <- f1[,2]
  T3 = T3[,c(1, 2, 5, 6)]
  T4 = cbind(T2, T3[,c(3, 4)])
  colnames(T4) <- c( colnames(T4)[1:4], "UVA-OR(95%CI)", "UVA-P",  "MVA-OR(95%CI)", "MVA-P")
  out = list(T2, T3, T4)
}
