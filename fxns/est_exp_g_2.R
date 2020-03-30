# estimate the exponential growth rate by county
# uses the nls function rather than lm of the log. 
###NOT WORKING####

est_exp_g2 <- function(case.data, cutoff){
  # cases is a dataframe of cases with columns: date, county, cases
  # cutoff = remove counties with less the X rows
  # note this should be for only one county
  # returns a list with a dataframe of predictions and parameters
  
  require(magrittr)
  require(tidyverse)
  
  
  dat1 <- case.data %>% arrange(county, date)
  dat1$county <- as.character(dat1$county)
  co <- unique(dat1$county) # counties in the dataset
  dat1$day <- 1  # rescaled day, pre-populated as 1
  
  for (i in 1:length(co)){
    tmprows <- which(dat1$county == co[i])
    n.days <- as.numeric(max(dat1$date[tmprows]) - min(dat1$date[tmprows]) + 1)
    dat1$day[tmprows] <- seq(1, n.days, 1)
  }
  
  co.days <- dat1 %>% group_by(county) %>% summarize(n = max(day)) # n rows / co
  keep <- co.days$county[which(co.days$n >= cutoff)] # remove these
  
  dat2 <- dat1 %>% filter(county %in% keep) # only those above the cutoff
  browser()
  # run the log liner model
  # NOTE: note sure whether to force it through the origin. 
  aug.dat <- dat2 %>% 
    group_by(county) %>% 
    do(fit = nlsLM(cases ~ N * exp(r * day), star=list(r=0,N=1), 
                   control = nls.lm.control(maxiter=1000)), data = .) %>% 
    augment(fit) %>% mutate(cases = exp(log.cases.), fit.cases = exp(.fitted), 
                            se.cases = exp(.se.fit))
  # do it again for the parameters
  params <- dat2 %>% 
    group_by(county) %>% 
    do(tidy(nlsLM(cases ~ N * exp(r * day), star=list(r=0,N=1), 
                  control = nls.lm.control(maxiter=1000)), data = .))

  out <- list(aug.dat = aug.dat, params = params)
  return(out)
}