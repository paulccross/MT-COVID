# estimate the exponential growth rate by county

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
  dat1$d <- 1  # rescaled day, pre-populated as 1
  
  for (i in 1:length(co)){
    tmprows <- which(dat1$county == co[i])
    n.days <- as.numeric(max(dat1$date[tmprows]) - min(dat1$date[tmprows]) + 1)
    dat1$d[tmprows] <- seq(1, n.days, 1)
  }
  
  co.days <- dat1 %>% group_by(county) %>% summarize(n = max(d)) # n rows / co
  keep <- co.days$county[which(co.days$n >= cutoff)] # remove these
  
  dat2 <- dat1 %>% filter(county %in% keep) # only those above the cutoff
  
  counties <- unique(dat2$county)
  dat2$r <- NA
  
  # growth rate
  for(i in 1:length(counties)){
    tmprows <- which(dat2$county == counties[i])
    n <- length(tmprows)
    dat2$r[tmprows[2:n]] <- log(dat2$cases[tmprows[2:n]] / 
                                  dat2$cases[tmprows[1:(n-1)]])
  }

  params <- dat2 %>% group_by(county) %>% 
    summarize(avg.r = mean(r, na.rm = T), sd.r = sd(r, na.rm = T))
  
  tmp <- dat2 %>% group_by(county) %>% tally()
  params$n <- tmp$n - 1
  
  # calculate the doubling time. (need a double check)  
  params$se.r <- params$sd.r / params$n^0.5
  params$t.double <- round(log(2) / log(1 + params$avg.r), 1)
  params$lo <- round(log(2) / log(1 + params$avg.r - 1.96*params$se.r),1)
  params$hi <- round(log(2) / log(1 + params$avg.r + 1.96*params$se.r),1)
  
  dat2$fit <- 1
  dat2$fit.hi <- 1
  dat2$fit.lo <- 1
  
  # calculate fitted growth
  for(i in 1:length(counties)){
    tmprows <- which(dat2$county == counties[i])
    n <- length(tmprows)
    dat2$fit[tmprows[1]] <- dat2$cases[tmprows[1]] #starting pop
    dat2$fit.hi[tmprows[1]] <- dat2$cases[tmprows[1]] #starting pop
    dat2$fit.lo[tmprows[1]] <- dat2$cases[tmprows[1]] #starting pop
    
    for(j in 2:n){
      dat2$fit[tmprows[j]] <- dat2$fit[tmprows[j-1]] * (1 + params$avg.r[i])
      dat2$fit.hi[tmprows[j]] <- dat2$fit.hi[tmprows[j-1]] *
        (1 + params$avg.r[i]+1.96*params$se.r[i])
      dat2$fit.lo[tmprows[j]] <- dat2$fit.lo[tmprows[j-1]] * 
        (1 + params$avg.r[i]-1.96*params$se.r[i])
    }
  }
  
  
  out <- list(aug.dat = dat2, params = params)
  return(out)
}