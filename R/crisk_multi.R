#' A output table for multivariable competing risk model
#'
#'# Output:  Hazard ratio , P, c-index
#
#'@param data Dataset
#'@param cevent The status indicator, normally 0 = alive, 1 = event, 2 = other event
#'@param csurv Follow up time
#'@param mvars one Catagorical Variable
#'@param gnames A text vector of the the group name for output
#'@return Output: a well-formated table ,  Hazard ratio , P
#
#'@examples
#'mvars <- c("age50", "Race", "Sentinel.lymph.node.positivity", "Surgical.margin.positivity", "Vascular.invasion", "Surgery.type", "Radiation.therapy.received", "Neoadjuvant.Chemotherapy" , "Adjuvant.chemotherapy","Overall.clincal.staging..TNM." )
#'multi_out_d<- crisk_multi(NewDat2, "pfs","dcensor",mvars)
#'
#'@export
#'@name crisk_multi
#'

crisk_multi <- function(data, csurv, cevent, mvars, gnames, con_var = NULL, cat_var = NULL){

  # make a  new dataset
  varD  <- data[,c(csurv, cevent,mvars)]
  varD2 <- data[,c(csurv, cevent,mvars)]

  # identify column type
  if (is.null(con_var)){
    con_var   <- colnames(varD)[sapply(varD, mode) == "numeric"]
    #con_names <- colnames(NewDat)[sapply(NewDat, class) == "numeric"]
  }

  if(is.null(cat_var)){
    cat_var   <- colnames(varD)[sapply(varD, mode) == "character"]
    #cat_names <- colnames(NewDat)[sapply(NewDat, class) == "character"]
  }

  # for categorical data, will condiser groups < 10 without event as NA
  n = length(cat_var)
  for(i in cat_var){
    varD[,i]<- ingorfactor_c(varD[,i], data[,cevent])
    # for chat variable   will chose the first level as referece
    # and make a matrix for modeling

    tmpv <- factor2ind(varD[,i])
    colnames(tmpv) <- gsub("varD[, i]", i, colnames(tmpv), fixed = TRUE)

    varD <- cbind(varD, tmpv)
    varD <- varD[,-which(names(varD) == i)]

  }
  blackrow = rep("")
  fit <- crr(varD[,1], varD[,2], varD[,-c(1,2)])
  S <- summary(fit)
  HR <- J.digit(S$coef[, c(2)], 2)
  LCL <- J.digit(S$conf.int[, c(3)], 2)
  UCL <- J.digit(S$conf.int[, c(4)], 2)
  HR95CI <- paste(HR, "(", LCL, ",", UCL, ")")
  result <-
    tibble(
      `Variable` = colnames(varD[,-c(1,2)]),
      `HR (95% CI)` = HR95CI,
      `P` = JS.p(S$coef[, c(5)])
    )
  result
}




