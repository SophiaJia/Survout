#' Formatting Decimal places
#' 
#'J.digit format the numbers       
#'@param x Number needed to be formated
#'@param k the number of decimals to show. 
#'@return rounded number with k decimals 
#'@examples
#'specify_decimal(1234, 5)
#'"1234.00000"
#'specify_decimal(0.1234, 5)
#' "0.12340"
#'@export 
#'@name J.digit
#' 
J.digit <- function(x, k) format(round(x, k), nsmall=k)
