#' A complete table for univariable survival analysis
#'
#'surv_uni_cat output the table with general survival analysis result with Number of total patients,
#'Number of Events, Estimated Median (95%CI), 1,2,5 year rate, HR (95\% Confidence Interval),P value, AIC and C index. This function only change the format of the output table.
#'@param D A data.frame in which to interpret the variables
#'@param Event The status indicator, normally 0=alive, 1=dead
#'@param Stime This is the follow up time
#'@param Svar A vector of group
#'@param month month-rate of survival
#'@param y1 1-year survival rate (yes/no)
#'@param y2 2-year survival rate(yes/no)
#'@param y5 5-year survival rate(yes/no)
#'@param AIC show AIC and C index (yes/no)
#'@return A tibble of survival output
#'@examples
#'surv_uni_cat(Dat, "surv", "scensor", "DS-GPA")
#'@export
#'@name surv_uni_cat
#'
surv_uni_cat <- function(Data, Stime, Event, Svar, month = 0, medianCI = F, y1 = T, y2 = T, y5 = T, AIC = F){
  ## univariable analysis for one factor variable.
  ## input : Data, Survival time, Event, Testing variable, AsFactor or not, Month of survival rate, and Rho (type of logtest)
  ## output: N, N.event, median survival , 1-year rate(95%CI), 2-year rate(95%CI), 5-year rate(95%CI), other rate, HR(95%CI), P, AIC, and C index
  ## require library: survival , tidyverse
  ## can it comes with a plot in this function?

  D <- Data %>% select(Stime = Stime, Event = Event, Svar = Svar)
  # D <- na.omit(D)

  # default log-rank test
  fit0 <- survdiff(Surv(Stime, Event) ~ Svar, data = D)
  fit1 <- coxph(Surv(Stime, Event) ~ as.factor(Svar), data = D)
  fit2 <- survfit(Surv(Stime, Event) ~ Svar, data = D, conf.type="log-log")

  # N and event N
  result = tibble(
    `Variable Name` = c(Svar, rep(" ", length(fit0$n)-1)),
    `Variable Level` =  D$Svar %>% table() %>% names(),
    `No Obs` = fit0$n,
    `No Event` = fit0$obs)

  ## median survival
  result %<>% mutate(`Estimated Median (month)` = summary(fit2 )$table[,c('median')] %>% format(.,digits = 3))

  if (medianCI == T){
  result %<>% mutate(`Estimated Median (lower 0.95CI)` = summary(fit2 )$table[,c('0.95LCL')] %>% format(.,digits = 3))
  result %<>% mutate(`Estimated Median (upper 0.95CI)` = summary(fit2 )$table[,c('0.95UCL')] %>% format(.,digits = 3))
  }

  ##time range
  D %>% group_by(Svar) %>% summarise(Value = max(Stime,na.rm = TRUE)) -> .tmp
  time_min = .tmp$Value %>% min()

  ## survival rate

  if(time_min > 11.999999 & y1 == T){
    result %<>% mutate(`1-year rate` = paste(J.digit(summary(fit2, time = 12)$surv * 100 , 0), '%(' ,
                                             J.digit(summary(fit2, time = 12)$lower * 100, 0), '%, ',
                                             J.digit(summary(fit2, time = 12)$upper * 100, 0),  '%)'))
  }
  if(time_min > 23.999999 & y2 == T){
    result %<>% mutate(`2-year rate` =  paste(J.digit(summary(fit2, time = 24)$surv * 100 , 0), '%(' ,
                                              J.digit(summary(fit2, time = 24)$lower * 100, 0), '%, ',
                                              J.digit(summary(fit2, time = 24)$upper * 100, 0),  '%)'))
  }
  if(time_min > 59.999999 & y5 == T){
    result %<>% mutate(`5-year rate` = paste(J.digit(summary(fit2, time = 60)$surv * 100 , 0), '%(' ,
                                             J.digit(summary(fit2, time = 60)$lower * 100, 0), '%, ',
                                             J.digit(summary(fit2, time = 60)$upper * 100, 0),  '%)'))
  }
  if(month !=0){
    if(month > time_min){
      warning("The month your choose is beyond the time range, choose a smaller one")
    }
    else{
      result %<>% mutate(`N-year rate` = paste(J.digit(summary(fit2, time = month)$surv * 100 , 0), '%(' ,
                                               J.digit(summary(fit2, time = month)$lower * 100, 0), '%, ',
                                               J.digit(summary(fit2, time = month)$upper * 100, 0),  '%)'))

    }
  }

  ##HR (95\% Confidence Interval)
  result %<>%
    mutate(`HR (95%CI)` = c( "Ref",paste(J.digit(summary(fit1)$conf.int[, 1], 2), '(' ,
                                         J.digit(summary(fit1)$conf.int[, 3], 2), ',' ,
                                         J.digit(summary(fit1)$conf.int[, 4], 2), ')' ))) %>%
    mutate(`P` = c( "", summary(fit1)$coefficients[,5] %>% JS.p))

  ## AIC , C index (D index maybe)
  if(AIC == T){
  blackrow = rep("",nrow(result)-1)
  result %<>%
    mutate(`C Index` =  c(J.digit(summary(fit1)$concordance[1],2),blackrow)) %>%
    mutate(`AIC` = c(J.digit(AIC(fit1),2),blackrow))
  }
  return(result)
}
