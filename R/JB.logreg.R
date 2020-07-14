#' A complete table for univariable survival analysis
#'
#'JB.logreg return results for logistic regression: Varname, levels, outcome(yes),outcome(no), OR , P
#'@param Dat A data.frame that includes two column x and y
#'@param name X variable output name
#'@param yname Y variable noutput name
#'@examples
#'colnames(Dat) <- c("x","y")
#'rs <- JB.logreg(Dat, name[i],yname)
#'@export
#'@name JB.logreg
#'

JB.logreg <- function (Dat,name,yname) {
  am.glm = glm(formula = y ~ x, family = binomial,data = Dat)
  Xlabel  <- am.glm$xlevels$x
  Varname <- c(name,rep("",length(Xlabel)-1))
  Num <- as.matrix(table(Dat))
  c1 = paste(Num[,1],"(",J.digit(prop.table(table(Dat[Dat$y == 0,1])) * 100,1),'%)')
  c2 = paste(Num[,2],"(",J.digit(prop.table(table(Dat[Dat$y == 1,1])) * 100,1),'%)')
  OR <- J.digit(exp(-coef(summary(am.glm)))[c(-1), 2], 2)
  CL <- exp(-confint(am.glm))
  LCL <- J.digit(CL[c(-1), 2], 2)
  UCL <- J.digit(CL[c(-1), 1], 2)
  OR95CI <- c("Ref", paste(OR, "(", LCL, ",", UCL, ")"))
  P <- c("",JS.p(summary(am.glm)$coef[(-1), 4]))
  out <- cbind(Varname,Xlabel,c1,c2,OR95CI,P)
  colnames(out) <- c("Variable Name","Levels",paste(yname,"(No)"),paste(yname,"(Yes)"), "Odds Ratio (95% CI)", "P")
  return(out)
}
