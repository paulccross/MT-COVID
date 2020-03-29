library(tidyverse)
library(magrittr)
library(broom)
library(lubridate)
library(reshape2)
library(ggplot2)
library(plotly)

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

#now need to plot these
plot()
