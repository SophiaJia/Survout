#'clean up date and type of treatment for one type of drug
#'
#' This function take multiple lines of treatment, identify which patient has a certain drug, output the drug name. start date and end date of this line of treatment
#'@param trs A dataframe that only includes the treatment medication
#'@param trs_date A dataframe that only includes the treatment dates.
#'@param durgname durgnames(can be multiple)
#'@return A list includes three elements: whether the drug is included, that start date and end date of the treatment
#'@examples
#'drugname1 = c("erlotinib","gefitinib")
#'trlinesd   = c("tr2","tr3","tr4","tr5")
#'trlines_datesd   = c("tr2date","tr3date","tr4date","tr5date")
#'trs <- Dat[trlines]
#'trs_date <- Dat[trlines_dates]
#'cdrug1 <- clean_drug(trs,trs_date,drugname1)
#'@export
#'@name clean_drug
#'
#'


clean_drug <- function(trs, trs_date, drugname){
  ## a vector of names of the targeted drug
  ## drug indicator
  ## line of dates
  ## return - drugname(drug, NA), start_date, end_date (the next line fo treatment, the last line return NA)

  drug1_ind <- matrix(F, nrow(trs), ncol(trs))
  for (di  in drugname){
    drug1_ind = drug1_ind|sapply(trs, function(x) grepl(di, x))
  }
  ## the first line of the treatment when take the drug
  drug1_yn <- apply(drug1_ind, 1, sum)
  drug_1_line_ind <- NA
  drug_1_date_start <-NA
  class(drug_1_date_start) <- "Date"
  drug_1_date_end <-NA
  class(drug_1_date_end) <- "Date"

  for (ti in order(seq(1:ncol(trs)), decreasing = T)){
    drug_1_line_ind[which(drug1_ind[,ti])] <- ti
    drug_1_date_start[which(drug1_ind[,ti])] <- trs_date[which(drug1_ind[,ti]),ti]
    if(ti == ncol(trs)){
      drug_1_date_end[which(drug1_ind[,ti])] <- NA
    }else{
      drug_1_date_end[which(drug1_ind[,ti])] <- trs_date[which(drug1_ind[,ti]),ti+1]
    }
  }

  re_list <- list(drug1_yn, drug_1_date_start, drug_1_date_end)
  return(re_list)
}
