#' A complete table for univariable survival analysis
#'
#'JB.logreg_m return logistic regression  results for for each of the input variables : Varname, evels, outcome(yes),outcome(no),  OR , P
#'@param xvar a dataframe has all the X variables
#'@param yvar a vector of outcome variable
#'@param name X variable output name
#'@param yname Y variable noutput name
#'@examples
#'rs <- JB.logreg_m(Date, D$Hypertension10, cats[1:16],"Hypertension")
#'@export
#'@name JB.logreg_m
#'

JB.logreg_m <- function (xvar, yvar, name, yname){
  rs.all <- NULL
  for (i in 1:length(xvar[1, ])) {
    Dat <- xvar[,i]
    Dat$y <- yvar
    colnames(Dat) <- c("x","y")
    rs <- JB.logreg(Dat, name[i],yname)
    rs.all <- rbind(rs.all, rs)
  }
  return(rs.all)
}
