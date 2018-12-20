#' clearn up the all the char variable in the dataset , return a dataset
#
#'@param Data Dataset
#'@return Output: a well-formated table , with N , N of event. , N of competing event, 1-year, 2-year, 5-year, n- year incidence rate, Hazard ratio , P, c-index
#'@examples
#' D <- fixchar(D)
#'
#'@export
#'@name fixchar
#'
#'
#'
#'

fixchar <- function(Data){
  Data %>%
    mutate_if(is.character, toupper)%>%
    mutate_if(is.character,trimws) %>%
    mutate_if(is.character,funs(replace(., . == "",NA))) %>%
    mutate_if(is.character,funs(replace(., . == "UNKNOWN",NA))) %>%
    mutate_if(is.character,funs(replace(., . == "NA",NA)))
}
