#'Format P value 
#'
#'JS.p2 is intended for formatting p-value , the difference with JS.p is will output all numeric data
#'@param value numeric vector of p-value (possibly with NAs )
#'@return A numeric vector of corrected p-value
#'@examples
#'JS.p(0.060)
#'JS.p(0.0050)
#'JS.p(0.00001)
#'@export 
#'@name JS.p2 
#'
JS.p2 <-function (value) {
        
        f <- function(p) {
                if (is.na(p)) 
                        p <- NA
                else if (p < 0 || p > 1) 
                        stop(paste(p, "is not a valid probability"))
                else if (p > 0.99) 
                        p <- 0.99
                else if (p > 0.1) 
                        p <- round(p, 2)
                else if (p > 0.001) 
                        p <- round(p, 3)
                else p <- 0.001
                return(p)
        }
        p <- sapply(value, f)
        return(p)
}
