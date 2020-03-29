# plot the cases by each county

plot_cases <- function(case.data, cutoff, date.scaled, log.scaled){
  # cases is a dataframe of cases with columns: county, date, cases
  # cutoff is the minimum # of cases in order to be included in the plot
  # date.scaled is TRUE if you want to rescale the date days since the first day 
  # each county reached the cutoff. FALSE = raw date
  # log.scaled = T for log scale the y. otherwise natural scale
  
  dat1 <- case.data %>% filter(cases >= cutoff) %>% arrange(county, date)
  dat1$county <- as.character(dat1$county)
  co <- unique(dat1$county) # counties in the dataset
  if(date.scaled == F){
    #plot
    p <- ggplot(dat1, aes(x = date, y = cases, color = county)) + 
      geom_line()
    if(log.scaled == T){
      p <- p + scale_y_continuous(trans = 'log10') + 
      labs(x = "Date", y = "Log cases") 
    }
    if(log.scaled == F){
      p <- p + labs(x = "Date", y = "Raw cases") 
    }
  }
  
  if(date.scaled == T){
    dat1$day <- 1  # rescaled day, pre-populated as 1
    for (i in 1:length(co)){
      tmprows <- which(dat1$county == co[i])
      n.days <- as.numeric(max(dat1$date[tmprows]) - min(dat1$date[tmprows]) + 1)
      dat1$day[tmprows] <- seq(1, n.days, 1)
    }
    #plot
    p <- ggplot(dat1, aes(x = day, y = cases, color = county)) + 
      geom_line() 
      if(log.scaled == T){
        p <- p + scale_y_continuous(trans = 'log10') + 
          labs(x = paste("Days since", cutoff, "cases", sep = " "), 
               y = "Log cases")
        }
    if(log.scaled == F){
      p <- p + labs(x = paste("Days since", cutoff, "cases", sep = " "), 
                    y = "Log cases")
    }
  }
  
  p <- p + theme_bw() + theme(legend.position="none")  
  ggplotly(p, dynamicTicks = T)  

}
