#' A complete table for univariable compeiting risk analysis
#'
#'crisk_multiuni output the table with general survival analysis result with Number of total patients,
#'Number of Events, Estimated Median, 1,2,5 year rate, HR (95\% Confidence Interval),P value This function only change the format of the output table.
#'@param Data A data.frame in which to interpret the variables
#'@param Event The status indicator, normally 0=alive, 1=dead
#'@param Stime This is the follow up time
#'@param catSvars A vector of cat variables
#'@param conSvars A vector of con variables
#'@param ordSvars A vector of ordinal variables
#'@param y1 1-year survival rate (yes/no)
#'@param y2 2-year survival rate(yes/no)
#'@param y5 5-year survival rate(yes/no)
#'@return A tibble of survival output
#'@examples
#'con_var <- colnames(NewDat)[sapply(NewDat, class) == "numeric"][c(-1, -(6:11))]
#'ord_var <- colnames(NewDat)[sapply(NewDat, class) == "factor"]
#'cat_var <- colnames(NewDat)[sapply(NewDat, class) == "character"][c(-2, -8,-23, -25, -26, -c(10:17))]
#'uni_out <- crisk_multiuni(NewDat, "pfs", "dcensor", catSvars = cat_var, ordSvars = ord_var, conSvars =con_var)
#'@export
#'@name crisk_multiuni
#'
crisk_multiuni <- function (Data, Stime, Event, catSvars = NULL, conSvars = NULL, ordSvars = NULL, y1 = T, y2 = T, y5 = T, month = 0){
  # create output tibble
  rs.all <- tibble()

  # transform name to var
  csurv <- Data[[Stime]]
  cevent <-Data[[Event]]

  if(!is.null(catSvars)){
    print("**** Cat Variables: **** ")
    for (i in 1:length(catSvars)) {
      print("    ")
      print(catSvars[i])
      cvars <- as.matrix(Data[catSvars[i]])
      rs <- crisk_cat(csurv, cevent, cvars, gnames = catSvars[i],  y1 = y1, y2 = y2, y5 = y5, month = month)
      rs.all <- bind_rows(rs.all, rs)
    }
  }

  if(!is.null(ordSvars)){
    print("**** Ordinal Variables: **** ")
    for (i in 1:length(ordSvars)) {
      print("    ")
      print(ordSvars[i])
      cvars <- Data[[ordSvars[i]]]
      rs <- crisk_ord(csurv, cevent, cvars, gnames = ordSvars[i], y1 = y1, y2 = y2, y5 = y5, month = month)
      rs.all <- bind_rows(rs.all, rs)
    }

  }

  if(!is.null(conSvars)){
    print("**** Continous Variables: **** ")
    for (i in 1:length(conSvars)) {
      print("    ")
      print(conSvars[i])
      cvars <- Data[[conSvars[i]]]
      rs <- crisk_con(csurv, cevent, cvars, conSvars[i])
      rs.all <- bind_rows(rs.all, rs)
    }

  }
  return(rs.all)
}




