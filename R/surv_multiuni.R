#' A complete table for univariable survival analysis
#'
#'surv_multiuni output the table with general survival analysis result with Number of total patients,
#'Number of Events, Estimated Median, 1,2,5 year rate, HR (95\% Confidence Interval),P value This function only change the format of the output table.
#'@param Data A data.frame in which to interpret the variables
#'@param Event The status indicator, normally 0=alive, 1=dead
#'@param Stime This is the follow up time
#'@param catSvars A vector of cat variables
#'@param conSvars A vector of con variables
#'@param y1 1-year survival rate (yes/no)
#'@param y2 2-year survival rate(yes/no)
#'@param y5 5-year survival rate(yes/no)
#'@return A tibble of survival output
#'@examples
#'conSvars <- c("Age","KPS")
#'catSvars <- c("Gender")
#'surv_multiuni(D, "pfs", "rcensor", catSvars, conSvars,y2 = F, y5 = F, y1 = F)# %>% View
#'@export
#'@name surv_multiuni
#'

surv_multiuni <- function (Data, Stime, Event, catSvars = NULL, conSvars = NULL, y1 = T, y2 = T, y5 = T)
{
  rs.all <- tibble()

  if(!is.null(catSvars)){
    for (i in 1:length(catSvars)) {
     rs <- surv_uni_cat(Data, Stime, Event, catSvars[i], y1 = y1, y2 = y2, y5 = y5)
     rs.all <- bind_rows(rs.all, rs)
     }
  }

  if(!is.null(conSvars)){
    for (i in 1:length(conSvars)) {
      rs <- surv_uni_con(Data, Stime, Event, conSvars[i])
      rs.all <- bind_rows(rs.all, rs)
    }

  }

  return(rs.all)
}
