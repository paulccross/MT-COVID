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

# read in data
nyt.cases <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
# subset to mt
mt.cases <- nyt.cases %>% filter(state == "Montana") %>% 
  mutate(county = as.factor(county))
# plot it
plot_cases(mt.cases, cutoff = 1, date.scaled = F, log.scaled = F)
plot_cases(mt.cases, cutoff = 1, date.scaled = T, log.scaled = T)

# estimate growth rates
out <- est_exp_g(mt.cases, cutoff = 3)

r.est <- out$params %>% filter(term == "day") %>% 
  select(county, estimate, std.error)

# calculate the doubling time. (need a double check)  
r.est$t.double <- round(log(2) / log(1 + r.est$estimate), 1)
r.est$lo <- round(log(2) / log(1 + r.est$estimate - 1.96*r.est$std.error),1)
r.est$hi <- round(log(2) / log(1 + r.est$estimate + 1.96*r.est$std.error),1)
table <- r.est  %>% filter(estimate >0) %>% select(county, t.double, lo, hi)

#now need to plot these
p <- ggplot(out$aug.dat, aes(day, cases, color = county)) + 
  geom_point() + 
  geom_line(aes(x = day, y = fit.cases, color = county)) + 
  theme_bw() + theme(legend.position="none")  

ggplotly(p, dynamicTicks = T) 

# geomtable not available for plotly
# not going to work when we get lot's of counties
p + annotate(geom = "table", x = 1, y = 40, label = list(table), 
             vjust = 1, hjust = 0)

# alt. growth rate estimates
out2 <- est_exp_g2(mt.cases, cutoff = 3)

out2$params

#now need to plot these
p <- ggplot(out2$aug.dat, aes(d, cases, color = county)) + 
  geom_point() + 
  geom_line(aes(x = d, y = fit, color = county)) + 
  theme_bw() + theme(legend.position="none")  

ggplotly(p, dynamicTicks = T) 

#DOESN"T SEEM CORRECT!