#' output for univariable and multivariable competing risk analysis (continuous and ordinal only)
#'
#'output the table with general competing risk analysis result with HR (95\% Confidence Interval),P value. This function only change the format of the output.
#'@param cevent The status indicator, normally 0 = alive, 1 = event, 2 = other event
#'@param csurv Follow up time
#'@param cvars A Matrix of groups variables (continuous and ordinal only)
#'@param gnames A text vector of the the group name for output
#'@return A dataframed output including HRs (95\% Confidence Intervals), P values.
#'@examples
#'X      <- cbind(D$v1, D$v2 , D$age_ge60)
#'Gnames <- c('SRSF2/U2FA1(WT vs Mutation)', 'Disease(AML vs MPN)', 'Age(< 60 vs > 60)')
#'output <- crisk_con(D$surv, D$censor_rm, X, Gnames)
#'
#'@export
#'@name crisk_con
#'
crisk_con <- function (csurv, cevent, cvars, gnames){
  fit  = cmprsk::crr(csurv, cevent, cvars)
  S   <- summary(fit)
  HR  <- J.digit(S$coef[,c(2)], 2)
  LCL <- J.digit(S$conf.int[,c(3)], 2)
  UCL <- J.digit(S$conf.int[,c(4)], 2)
  HR95CI <- paste(HR,'(',LCL,',',UCL,')')
  p   <- JS.p(S$coef[,c(5)])

  result <- tibble(
    `Variable` = gnames,
    `HR (95% CI)` = HR95CI,
    `P` = p
  )
  return(result)
}

