#' A complete table for univariable survival analysis
#'
#'JS.uni output the table with general survival analysis result with Number of total patients,
#'Number of Events, Estimated Median, 1,2,5 year rate, HR (95\% Confidence Interval),P value, AIC and C index. This function only change the format of the output table.
#'@param D A data.frame in which to interpret the variables
#'@param Event The status indicator, normally 0=alive, 1=dead
#'@param Stime This is the follow up time
#'@param Svar A vector of group
#'@param month month-rate of survival
#'@param y1 1-year survival rate (yes/no)
#'@param y2 2-year survival rate(yes/no)
#'@param y5 5-year survival rate(yes/no)
#'@return A tibble of survival output
#'@examples
#'surv_uni_cat(Dat, "surv", "scensor", "DS-GPA")
#'@export
#'@name surv_uni_cat
#'
crisk_cat <- function(csurv, cevent, cvars, gnames, month = 0, y1 = T, y2 = T, y5 = T){

  # exclude missing for making model.matrix
  datmp <- cbind(cvars, csurv, cevent)

  if(sum(is.na(cvars)) > 0 ){
    var_missing <- which(is.na(cvars))
    datmp <- datmp[-var_missing,]
  }

  # exclude the level of variable which has no risk event
  del_factor  <- names(datmp[,1] %>% table)[which(table(datmp[,1], datmp[,3])[,2] <1)]
  del_factor2 <- names(datmp[,1] %>% table)[table(datmp[,1]) < 10]
  datmp <-datmp[!((datmp[,1] %in% del_factor)&(datmp[,1] %in% del_factor2)),]

  cvars  <- datmp[,1]
  csurv  <- as.numeric(datmp[,2])
  cevent <- datmp[,3]
  cvars <- factor(cvars)
  var.matrix <- model.matrix(~cvars)[,-1]


  # n and event
  result <- tibble(
    `Varaible Levels` = table(cvars) %>% names,
    `N` = table(cvars),
    `Number of event` = table(cvars, cevent)[,2],
    `Number of competing event` = table(cvars, cevent)[,3]
  )



  # build model
  fit = cmprsk::crr(csurv, cevent, var.matrix)
  fit2 <- cmprsk::cuminc(csurv, cevent, var.matrix)

  ## time range (the min follow up time of these levels)
  tmp.df <- cbind(csurv, cvars) %>% as.tibble()
  complete_df <- tmp.df[complete.cases(tmp.df),]
  complete_df %>% group_by(cvars) %>% summarise(Value = max(csurv,na.rm = TRUE)) -> .tmp
  time_min = .tmp$Value %>% min()

  #incident rate:
  n <- result %>% nrow()

  if(time_min > 11.999999 & y1 == T){
    cc <- cmprsk::timepoints(fit2, times = 12)
    est <- cc$est[1:n] %>% round(2)
    car <- cc$var[1:n] %>% round(2)

    result <- result %>%
      mutate(`1-year rate` = paste(est * 100, "%", "\u00B1",car * 100, "%") %>% t %>% t )
  }


  if(time_min > 23.999999 & y2 == T){
    cc <- cmprsk::timepoints(fit2, times = 24)
    est <- cc$est[1:n] %>% round(2)
    car <- cc$var[1:n] %>% round(2)

    result <- result %>%
      mutate(`2-year rate` = paste(est * 100, "%", "\u00B1",car * 100, "%") %>% t %>% t )
  }

  if(time_min > 59.999999 & y5 == T){
    cc <- cmprsk::timepoints(fit2, times = 60)
    est <- cc$est[1:n] %>% round(2)
    car <- cc$var[1:n] %>% round(2)

    result <- result %>%
      mutate(`5-year rate` = paste(est * 100, "%", "\u00B1",car * 100, "%") %>% t %>% t )
  }

  if(month !=0){
    if(month > time_min){
      warning("The month your choose is beyond the time range, choose a smaller one")
    }
    else{
      cc <- cmprsk::timepoints(fit2, times = month)
      est <- cc$est[1:n] %>% round(2)
      car <- cc$var[1:n] %>% round(2)

      result <- result %>%
        mutate(`N-year rate` = paste(est * 100, "%", "\u00B1",car * 100, "%") %>% t %>% t )

    }}


  blackrow = rep("")
  ##HR (95\% Confidence Interval)
  S <- summary(fit)
  HR <- J.digit(S$coef[, c(2)], 2)
  LCL <- J.digit(S$conf.int[, c(3)], 2)
  UCL <- J.digit(S$conf.int[, c(4)], 2)
  HR95CI <- paste(HR, "(", LCL, ",", UCL, ")")
  result <-
    result %>%
    mutate(`HR (95% CI)` = c(blackrow,HR95CI)) %>%
    mutate(`P` = c(blackrow, JS.p(S$coef[, c(5)])))

  ## AIC, Cindex, (D index maybe)

  return(result)
}



