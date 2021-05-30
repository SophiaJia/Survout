#'get pfs variable when multiple end dates excists
#'
#' Take the last date for treatment, all other dates, date of death and last follow up and return pfs ttf variable
#'@param start_date vector, start date for a treatment
#'@param end_date vector, end date for the treatment
#'@param other_dates data frame. other protencial progression dates, eg srs
#'@param date_death vector
#'@param date_fup vector
#'@return A list includes three elements: progression dates, progression sensor variable , pfs
#'@examples
#'cod <-  c("date.srs1","date.srs2","date.srs3","date.srs4","date.srs5","date.srs6")
#'DT_other_datas <- D[cod]
#'EGFR25_pfs <- get_tr_pfs(D$date_25start, EGFR25_info[[3]],DT_other_datas,D$date_death, D$date_lastfollowup)
#'@export
#'@name get_tr_pfs
#'
#'

get_tr_pfs <- function(start_date, end_date, other_dates, date_death, date_fup){
  ### take a couple dates and find the pfs end date
  #2. calcluate PFS in this way :
  # - if there is another line of therapy given after the drug we are analyzing, use the start date of the next therapy as the date of progression (for example if erlotinib is the 1st line after diagnosis of brain mets, and pembrolizumab is given as 2nd line, the start date of pembrolizumab can count as the date progression for erlotinib).
  # - if SRS is given after the start of the drug we are analyzing (for example if erlotinib is started on 1/1/20 but SRS is given on 4/1/20, then 4/1/20 is the date of progression)
  # - if the patient dies without any SRS or changes in therapy, then date of death would be considered date of progression
  # - if the patient is alive and stable without any SRS or changes in therapy, then the date of last follow up would be considered date of progression.
  #browser()
  if(is.null(start_date)){
    errorCondition("No Start Date")
  }
  if(is.null(end_date)){
    errorCondition("No end_date")
  }
  if(is.null(date_death)){
    errorCondition("No date_death")
  }
  if(is.null(date_fup)){
    errorCondition("no date_fup")
  }

  for(i in seq(ncol(other_dates))){
    other_dates[which(!other_dates[,i] > start_date),i] <- NA
  }
  end_date_fix <- end_date
  end_date_fix[which(end_date_fix < start_date)] <- NA
  end_date_fix[which(end_date_fix == start_date)] <- NA

  pfs_end<- pmin(do.call(pmin,c(other_dates,list(na.rm = T))),end_date_fix, date_death,na.rm = T)
  pscensor<- as.numeric(!is.na(pfs_end))
  pfs_month_tr <- (ifelse(pscensor == 1, (pfs_end - start_date), (date_fup-start_date)))/30
  return(list(pfs_end,pscensor,pfs_month_tr))
}
