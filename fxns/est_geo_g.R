# estimate the exponential growth rate by county

est_geo_g <- function(case.data, length.cutoff, n.cutoff){
  # cases is a dataframe of cases with columns: date, county, cases
  # length.cutoff = remove counties with less the X rows
  # n.cutoff = remove counties with less the X rows
  require(magrittr)
  require(tidyverse)
  
  if(missing(length.cutoff)==T){
    warning("missing length cutoff, using default")
    length.cutoff <- 1}
  if(missing(n.cutoff)==T){
    warning("missing n cutoff, using default")
    n.cutoff <- 1}
  
  
  dat1 <- case.data %>% arrange(county, date) %>% filter(cases >= n.cutoff)
  dat1$county <- as.character(dat1$county)
  co <- unique(dat1$county) # counties in the dataset
  dat1$d <- 1  # rescaled day, pre-populated as 1
  
  for (i in 1:length(co)){
    tmprows <- which(dat1$county == co[i])
    n.days <- as.numeric(max(dat1$date[tmprows]) - min(dat1$date[tmprows]) + 1)
    dat1$d[tmprows] <- seq(1, n.days, 1)
  }
  #browser()
  co.days <- dat1 %>% group_by(county) %>% summarize(n = max(d)) # n rows / co
  keep <- co.days$county[which(co.days$n >= n.cutoff)] # remove these
  
  dat2 <- dat1 %>% filter(county %in% keep) # only those above the cutoff
  
  counties <- unique(dat2$county)
  dat2$r <- NA
  dat2$new.cases <- NA
  # growth rate
  for(i in 1:length(counties)){
    tmprows <- which(dat2$county == counties[i])
    n <- length(tmprows)
    dat2$new.cases[tmprows[2:n]] <- dat2$cases[tmprows[2:n]] - 
                                      dat2$cases[tmprows[1:(n-1)]]
    
    dat2$r[tmprows[2:n]] <- log(dat2$cases[tmprows[2:n]] / 
                                  dat2$cases[tmprows[1:(n-1)]])
  }
  
  #Geometric mean
  params <- dat2 %>% group_by(county) %>% 
    summarize(avg.r = exp(mean(log(r + 0.00001), na.rm = T)), 
              sd.r = exp(sd(log(r + 0.00001), na.rm = T)))
  
  tmp <- dat2 %>% group_by(county) %>% tally()
  params$n <- tmp$n - 1
  
  # calculate the doubling time. (need a double check)  
  params$se.r <- params$sd.r / params$n^0.5
  params$t.double <- round(log(2) / log(1 + params$avg.r), 1)
  params$lo <- round(log(2) / log(1 + params$avg.r + 1.96*params$se.r),1)
  params$hi <- round(log(2) / log(1 + params$avg.r - 1.96*params$se.r),1)
  
  out <- list(aug.dat = dat2, params = params)
  return(out)
}