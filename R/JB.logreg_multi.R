#' A output table for multivariable logistic regression
#'
#'JB.logreg_multi return results for MVA logistic regression: Varname,OR , P
#'@param outx glm returning result
#'@examples
#'f1 = glm(as.matrix(y~.,family = binomial, data = tmp)
#'rs <- JB.logreg_multi(f1)
#'@export
#'@name JB.logreg_multi
#'

JB.logreg_multi <- function (outx) {
  OR <- J.digit(exp(coef(outx)), 2)
  CL <- exp(confint(outx))
  LCL <- J.digit(CL[, 1], 2)
  UCL <- J.digit(CL[, 2], 2)
  OR95CI <- paste(OR, "(", LCL, ",", UCL, ")")
  ctable <- coef(summary(outx))
  .P <- ctable[, 4]
  P <- JS.p(.P[1:length(UCL)])
  .out <- cbind(OR95CI, P)
  out <- .out[-1, ]
  colnames(out) <- c("Odds Ratio (95% CI)", "P")
  return(out)
}
