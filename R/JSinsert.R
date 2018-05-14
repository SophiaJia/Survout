#' Insert rows into dataset
#' 
#'JS.insert inserts rows into dataset
#'@param locs  Row locations to insert the row
#'@param inrows Rows that need to be inserted
#'@param df Dataset 
#'@return  A dataset with inserted rows
#'@examples
#'aa <- data.frame(1:6, col = 2)
#'a <- c(7,7)
#'JS.insert (c(1,2),c(7,7),aa) 
#'
#'
#'@export 
#'@name JS.insert
#' 
#'
JS.insert <- function(locs, inrows, df, rep = T){
        #locs = insert locations
        #inrows = insert rows
        #df = dataframe
        
        #check if inrows and df are the same type
        #if (class(inrows) != class(df)){print("Insert rows should be the same type as data")}
        
        if ( rep == T ) {
                for (i in 1:length(locs)){   
                        #the inrows can be only one column or  multiple columns 
                        #insert.n <- length(inrows[,1])
                        insert.n <- 1
                        loc <- locs[i] + insert.n * i - 1
                        if (loc == 0) {
                                df <- rbind(inrows, df)
                        }
                        else{
                                df <- rbind(df[1:loc - 1, ], inrows, df[loc:nrow(df), ])
                        }
                }
        }
        else
        {
                for (i in 1:length(locs)){
                        loc <- locs[i] + i - 1
                        .out <- rbind(df[0:loc - 1, ], inrows[i], df[loc:nrow(df), ])
                }
        }
        return (df)
}