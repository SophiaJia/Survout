#' A output table for univariable and multivariable competing risk analysis (ordinal only)
#'
#'# Output: a well-formated table , with N , N of event. , N of competing event,
# 1-year, 2-year, 5-year, n- year incidence rate, Hazard ratio , P, c-index
#
#'@param cevent The status indicator, normally 0 = alive, 1 = event, 2 = other event
#'@param csurv Follow up time
#'@param cvars A Matrix of groups variables (continuous and ordinal only)
#'@param gnames A text vector of the the group name for output
#'@return Output: a well-formated table , with N , N of event. , N of competing event, 1-year, 2-year, 5-year, n- year incidence rate, Hazard ratio , P, c-index
#
#'@examples
#'output <- crisk_ord(NewDat$pfs, NewDat$dcensor, NewDat$ECOG.Score, "gnames")
#'
#'@export
#'@name crisk_ord
#'


crisk_ord <- function (csurv, cevent, cvars, gnames, month = 0, y1 = T, y2 = T, y5 = T ){

  # This function takes the ordinal variable as input.

  # Output: a well-formated table , with N , N of event. , N of competing event,
  # 1-year, 2-year, 5-year, n- year incidence rate, Hazard ratio , P, c-index
  #
  # model
  fit = cmprsk::crr(csurv, cevent, cvars)
  fit2 <- cmprsk::cuminc(csurv, cevent, cvars)

  nlevel <- levels(cvars) %>% length

  # n and event
  result <- tibble(
    `Variable` =  c(gnames,rep("", nlevel - 1)),
    `Varaible Levels` = levels(cvars),
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

  blackrow = rep("",nrow(result)-1)
  ##HR (95\% Confidence Interval)
  S <- summary(fit)
  HR <- J.digit(S$coef[, c(2)], 2)
  LCL <- J.digit(S$conf.int[, c(3)], 2)
  UCL <- J.digit(S$conf.int[, c(4)], 2)
  HR95CI <- paste(HR, "(", LCL, ",", UCL, ")")
  result <-
    result %>%
    mutate(`HR (95% CI)` = c(HR95CI, blackrow)) %>%
    mutate(`P` = c(JS.p(S$coef[, c(5)]), blackrow))

  ## AIC, Cindex, (D index maybe)

  return(result)
}
