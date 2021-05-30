#'clean up date and type of treatment
#'
#' This function take multiple lines of treatment, identify which patient has wanted drugs, output the drug name. start date and end date of this line of treatment
#'@param Dat A data.frame includes all the data
#'@param durgname1 durgnames(can be multiple)
#'@param durgname2 durgnames(can be multiple)
#'@param trlines A vector that includes all treatment variable names
#'@param trlines_dates A vector that includes all treatment date varaible names
#'@param drugDup a boolean indicates if duplicate treatment for a patients is allowed
#'@return A list includes three elements: drug group, that start date and end date of the treatment
#'@examples
#'drugname1 = c("erlotinib","gefitinib")
#'drugname2 = c("osimertinib")
#'trlinesd   = c("tr2","tr3","tr4","tr5")
#'trlines_datesd   = c("tr2date","tr3date","tr4date","tr5date")
#'tr_info <- clean_drugs(D, drugname1, drugname2,trlines = trlinesd, trlines_dates = trlines_datesd)
#'@export
#'@name clean_drugs
#'
#'

clean_drugs <- function(Dat, drugname1, drugname2, trlines, trlines_dates, drugname3 = NULL,drugDup = FALSE){
  # this function
  #1. find the drug's treatment line, and time
  #browser()
  trs <- Dat[trlines]
  trs_date <- Dat[trlines_dates]
  cdrug1 <- clean_drug(trs,trs_date,drugname1)
  cdrug2 <- clean_drug(trs,trs_date,drugname2)

  dr_name1 <- paste0(drugname1,collapse = "/")
  dr_name2 <- paste0(drugname2,collapse = "/")

  tr_name <- NA
  tr_name[cdrug1[[1]] == 1] <- dr_name1
  tr_name[cdrug2[[1]] == 1] <- dr_name2

  tr_date_start <- NA
  tr_date_end <- NA
  class(tr_date_start) <- "Date"
  class(tr_date_end) <- "Date"
  tr_date_start[cdrug1[[1]] == 1] <- cdrug1[[2]][cdrug1[[1]] == 1]
  tr_date_start[cdrug2[[1]] == 1] <- cdrug2[[2]][cdrug2[[1]] == 1]
  tr_date_end[cdrug1[[1]] == 1] <- cdrug1[[3]][cdrug1[[1]] == 1]
  tr_date_end[cdrug2[[1]] == 1] <- cdrug2[[3]][cdrug2[[1]] == 1]

  if(!is.null(drugname3)){
    cdrug3 <- clean_drug(trs,trs_date,drugname3)
    dr_name3 <- paste0(drugname3,collapse = "/")
    tr_name[cdrug3[[1]] == 1] <- dr_name3
    tr_date_start[cdrug3[[1]] == 1] <- cdrug3[[2]][cdrug3[[1]] == 1]
    tr_date_end[cdrug3[[1]] == 1] <- cdrug3[[3]][cdrug3[[1]] == 1]
  }

  if(drugDup == FALSE){
    tt1 = which(cdrug1[[1]] == 1 & cdrug2[[1]] == 1)
    tr_name[tt1] <- NA
    tr_date_start[tt1] <- NA
    tr_date_end[tt1] <- NA

    if(!is.null(drugname3)){
      tt2 <- which(cdrug3[[1]] == 1 & cdrug2[[1]] == 1)
      tt3 <- which(cdrug1[[1]] == 1 & cdrug3[[1]] == 1)
      tr_name[tt2] <- NA
      tr_name[tt3] <- NA
      tr_date_start[tt2] <- NA
      tr_date_end[tt2] <- NA
      tr_date_start[tt3] <- NA
      tr_date_end[tt3] <- NA
    }

  }
  return(list(tr_name, tr_date_start, tr_date_end))
}

