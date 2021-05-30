#' A complete table for univariable survival analysis
#'
#'JS.uni output the table with general survival analysis result with Number of total patients,
#'Number of Events, Estimated Median, 1,2,5 year rate, HR (95\% Confidence Interval),P value, AIC and C index. This function only change the format of the output table.
#'@param D A data.frame in which to interpret the variables
#'@param Event The status indicator, normally 0=alive, 1=dead
#'@param Stime This is the follow up time
#'@param Svar A vector of group
#'@return A tibble of survival output
#'@examples
#'surv_uni_con(Dat, "surv", "scensor", "age")
#'@export
#'@name surv_uni_con
#'
surv_uni_con <- function(Data, Stime, Event, Svar){
  ## univariable analysis for con variable.
  ## input : Data, Survival time, Event, Testing variable,
  ## output: HR(95%CI), and P, AIC, C index
  ## require library: survival , tidyverse
  ## what else can be added?

  D1 <- Data %>% select(Stime = Stime, Event = Event, Svar = Svar)
  # D <- na.omit(D)

  # cox model
  fit1 <- coxph(Surv(Stime, Event) ~ Svar, data = D1)

  # assign name to result tibble
  l <- list("Variable Name" = Svar)
  result <- as_tibble(l)

  ##HR (95\% Confidence Interval)
  result %<>%
    mutate(`HR (95%CI)` = c(paste(J.digit(summary(fit1)$conf.int[, 1], 2), '(' ,
                                  J.digit(summary(fit1)$conf.int[, 3], 2), ',' ,
                                  J.digit(summary(fit1)$conf.int[, 4], 2), ')' ))) %>%
    mutate(`P` = c(summary(fit1)$coefficients[,5] %>% JS.p))

  ## AIC , C index (D index maybe)
  # blackrow = rep("",nrow(result)-1)
  # result %<>%
  #   mutate(`C Index` =  c(J.digit(summary(fit1)$concordance[1],2),blackrow)) %>%
  #   mutate(`AIC` = c(J.digit(AIC(fit1),2),blackrow))

  return(result)
}
