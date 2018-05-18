#' A helper fucntion for competing risk functions
#'
#' for categorical data, will condiser groups < 10 without event as NA, Since the crr function will fail to run in this case
#'
#'@param x Variable need to be cleaned
#'@param Event The status indicator, normally 0=censored, 1=event, 2 = death

#'@return x variable with some NA Values
#'@examples
#'ingorfactor_c(NewDat$Race, NewDat$dcensor)
#'@export
#'@name ingorfactor_c
#'
#'
ingorfactor_c <- function(x,event){
  # exclude the level of variable which has no risk event
  del_factor  <- names(x %>% table)[which(table(x, event)[,2] <1)]
  del_factor2 <- names(x %>% table)[table(x) < 10]
  x[((x %in% del_factor)&(x %in% del_factor2))]<- NA
  x
}




