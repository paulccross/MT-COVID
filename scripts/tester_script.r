library(tidyverse)
library(magrittr)
library(broom)
library(lubridate)
library(reshape2)
library(ggplot2)
library(plotly)
library(ggpmisc)

source("./fxns/plot_cases.r")
source("./fxns/est_exp_g.r")
source("./fxns/est_geo_g.r")

# read in NYTimes dataset
nyt.cases <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# subset to mt
mt.cases <- nyt.cases %>% filter(state == "Montana") %>% 
  mutate(county = as.factor(county))
# plot it
plot_cases(mt.cases, cutoff = 5, date.scaled = F, log.scaled = F)
plot_cases(mt.cases, cutoff = 5, date.scaled = T, log.scaled = T)

# estimate growth rates
out <- est_exp_g(mt.cases, cutoff = 5)

r.est <- out$params %>% filter(term == "day") %>% 
  select(county, estimate, std.error)

# calculate the doubling time. (need a double check)  
r.est$t.double <- round(log(2) / log(1 + r.est$estimate), 1)
r.est$lo <- round(log(2) / log(1 + r.est$estimate - 1.96*r.est$std.error),1)
r.est$hi <- round(log(2) / log(1 + r.est$estimate + 1.96*r.est$std.error),1)
table <- r.est  %>% filter(estimate >0) %>% select(county, t.double, lo, hi)

#now need to plot these
p <- ggplot(out$aug.dat, aes(day, cases, color = county)) + 
  geom_point() + scale_y_continuous(trans = 'log10') +
  geom_line(aes(x = day, y = fit.cases, color = county)) + 
  theme_bw() + theme(legend.position="none")  

ggplotly(p, dynamicTicks = T) 

# geomtable not available for plotly
# not going to work when we get lot's of counties
p + annotate(geom = "table", x = 1, y = 200, label = list(table), 
             vjust = 1, hjust = 0)

# Geometric mean growth rate estimates
out2 <- est_geo_g(mt.cases, n.cutoff = 5, length.cutoff = 5)
# probably the counts and number of days is still too low to estimate this way for MT

out2$params
out2$aug.dat

