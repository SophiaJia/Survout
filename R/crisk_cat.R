#' A output table for univariable risk analysis (catigorical only)
#'
#'# Output: a well-formated table , with N , N of event. , N of competing event,
# 1-year, 2-year, 5-year, n- year incidence rate, Hazard ratio , P, c-index
#
#'@param cevent The status indicator, normally 0 = alive, 1 = event, 2 = other event
#'@param csurv Follow up time
#'@param cvars one Catagorical Variable
#'@param gnames A text vector of the the group name for output
#'@return Output: a well-formated table , with N , N of event. , N of competing event, 1-year, 2-year, 5-year, n- year incidence rate, Hazard ratio , P, c-index
#
#'@examples
#'output <- crisk_cat(NewDat$pfs, NewDat$dcensor, NewDat$Histologic.type, "gnames")
#'
#'@export
#'@name crisk_cat
#'

crisk_cat <- function(csurv, cevent, cvars, gnames, month = 0, y1 = T, y2 = T, y5 = T){

  var_missing <- which(is.na(cvars))
  cvars  <- cvars[-var_missing]
  csurv  <- csurv[-var_missing]
  cevent <- cevent[-var_missing]
  var.matrix <- model.matrix(~cvars)[,-1]

  fit = cmprsk::crr(csurv, cevent, var.matrix)
  fit2 <- cmprsk::cuminc(csurv, cevent, var.matrix)

  # n and event
  result <- tibble(
    `Varaible Levels` = table(cvars) %>% names,
    `N` = table(cvars),
    `Number of event` = table(cvars, cevent)[,2],
    `Number of competing event` = table(cvars, cevent)[,3]
  )

  ## time range (the min follow up time of these levels)
  tmp.df <- cbind(csurv, cvars) %>% as.tibble()
  complete_df <- tmp.df[complete.cases(tmp.df),]
  complete_df %>% group_by(cvars) %>% summarise(Value = max(csurv,na.rm = TRUE)) -> .tmp
  time_min = .tmp$Value %>% min()

  #incident rate:

  if(time_min > 11.999999 & y1 == T){
    cc <- cmprsk::timepoints(fit2, times = 12)
    est <- cc$est[1:4] %>% round(2)
    car <- cc$var[1:4] %>% round(2)

    result <- result %>%
      mutate(`1-year rate` = paste(est * 100, "%", "\u00B1",car * 100, "%") %>% t %>% t )
  }


  if(time_min > 23.999999 & y2 == T){
    cc <- cmprsk::timepoints(fit2, times = 24)
    est <- cc$est[1:4] %>% round(2)
    car <- cc$var[1:4] %>% round(2)

    result <- result %>%
      mutate(`2-year rate` = paste(est * 100, "%", "\u00B1",car * 100, "%") %>% t %>% t )
  }

  if(time_min > 59.999999 & y5 == T){
    cc <- cmprsk::timepoints(fit2, times = 60)
    est <- cc$est[1:4] %>% round(2)
    car <- cc$var[1:4] %>% round(2)

    result <- result %>%
      mutate(`5-year rate` = paste(est * 100, "%", "\u00B1",car * 100, "%") %>% t %>% t )
  }

  if(month !=0){
    if(month > time_min){
      warning("The month your choose is beyond the time range, choose a smaller one")
    }
    else{
      cc <- cmprsk::timepoints(fit2, times = month)
      est <- cc$est[1:4] %>% round(2)
      car <- cc$var[1:4] %>% round(2)

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


