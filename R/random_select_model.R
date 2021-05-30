#' A output table for multivariable survival analysis
#'
#'random_select_model use random forest for variable selection, then push top variables into JS.multi
#'@param Data A data.frame in which to interpret the variables
#'@param Event The status indicator, normally 0=alive, 1=dead
#'@param Stime This is the follow up time
#'@param Svar A vector of group
#'@return A Matrix of JS.multi
#'@examples
#'Svar <- c(cat,"first.line.systemic.therapies","age","SRS_total_number")
#'ttt <- random_select_model(D2,"os_month_srs1", "scensor",Svar,"first.line.systemic.therapies")
#'
#'@export
#'@name random_select_model
#'
#'

JS.multi_modify <- function(data, multi_result){
  valcol <- data.frame()
  Dat3 <- data[3:ncol(data)]
  tmp <- as.data.frame(multi_result)
  for(i in seq(1:ncol(Dat3))){
   # print(i)
    varname  = colnames(Dat3)[i]
    varvalue = Dat3[[i]]
    if(class(varvalue) == "factor"){
      nn = nlevels(varvalue) -1
      nlevel= levels(varvalue)
      valcol<- rbind(valcol,cbind("Variable Name" = c(varname,rep("",nn)), Level = nlevel,rbind(c("Ref"," "),tmp[1:nn,c(2,3)])))
      tmp <- tmp[(nn+1):nrow(tmp), ]
    }else if(class(varvalue) == "numeric"){
      valcol<- rbind(valcol,cbind("Variable Name" = varname,Level ="",tmp[1,c(2,3)]))
      tmp <- tmp[2:nrow(tmp), ]
    }
  }
  valcol
}

random_select_model<- function(Data,Stime,Event,Svar,ImpV){
  Dat <- Data %>% select(Stime = Stime, Event = Event, Svar = Svar)
  colnames(Dat) <- c("Stime","Event",Svar)
  Dat[sapply(Dat, is.character)] <- lapply(Dat[sapply(Dat, is.character)], as.factor)
  fit_rf <- rfsrc(Surv(Stime, Event) ~ .  , Dat, na.action = "na.impute", nimpute = 3)
  rt_select <- var.select.rfsrc(fit_rf)
  seleV <- unique(c(rt_select$topvars,ImpV))

  Dat2 <- Dat %>% select(Stime, Event, all_of(seleV))
  ttt = JS.multi(Surv(Stime, Event) ~ .  , Dat2)
  JS.multi_modify(Dat2,ttt)
}


