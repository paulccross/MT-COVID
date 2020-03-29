rm(list=ls())
gc()
library(deSolve)
library(parallel)
library(data.table)
library(ggplot2)
library(magrittr)
library(ggpubr)
library(HistDAWass)

X <- readRDS('data/JH_CSSE_COVID_dataset_clean')
countries <- c('China','Cruise Ship','Iran','Italy','France','Spain','Germany','Korea, South',
               'Netherlands','United Kingdom','Belgium','Switzerland')
dp_dates <- function(cntry,X.=X,output.plot=FALSE){
  x <- X[country==cntry,list(Confirmed=sum(Confirmed)),by=date]
  
  trajx <- x$date
  trajy <- log(x$Confirmed)
  mat <- cbind(trajx,trajy)
  mat <- mat[trajy>-Inf,]
  
  dp <- HistDAWass::DouglasPeucker(mat,0.7)
  
  
  if (cntry %in% c('China','Cruise Ship')){
    dates <- x$date[x$date>=dp[1,1] & x$date<=dp[2,1]]
  } else {
    diffs <- diff(x$Confirmed)
    jump_start <- max(which(diffs==0 & log(x$Confirmed[1:(nrow(x)-1)])<4))
    future_vertices <- which(dp[,'x']>=x$date[jump_start])
    n_future_segments <- length(future_vertices)-1
    if (n_future_segments==1){ ## pick the only post-flatline segment
      next_vertex <- dp[future_vertices[1],'x']
      following_vertex <- dp[future_vertices[2],'x']
      dates <- x$date[x$date>=next_vertex & x$date<=following_vertex]
    } else { ## pick the second post-flatline segment
      next_vertex <- dp[future_vertices[2],'x']
      following_vertex <- dp[future_vertices[3],'x']
      dates <- x$date[x$date>=next_vertex & x$date<=following_vertex]
    }
  }
  
  return(data.table('country'=cntry,'start_date'=min(dates),'end_date'=max(dates)))
}

ref=lapply(countries,dp_dates) %>% rbindlist

X <- X[,list(Confirmed=sum(Confirmed)),by=c('country','date')]
setkey(ref,country)
setkey(X,country)
X <- ref[X[country %in% countries]]
X[,GrowthPhase:=date>=start_date & date<=end_date]

X[GrowthPhase==TRUE,GrowthDate:=1:.N,by=country]

ggplot(X,aes(date,Confirmed,color=country,by=country))+
  geom_line(lwd=2,lty=2)+
  geom_line(data=X[GrowthPhase==TRUE],lwd=2,lty=1)+
  facet_wrap(.~country,nrow=3)+
  scale_y_continuous(trans='log',breaks=10^(0:5))+
  theme(legend.position = 'none')
ggsave('figures/DouglasPeucker_algorithm_date_selection.png',width=12,height=8,units='in')


countries_w_exp_growth <- c('China','Cruise Ship','Iran','Italy','Korea, South','Spain','France','Germany',
                            'Netherlands','United Kingdom','Switzerland','Belgium')
### Poisson regression across countries
CountryGrowthRates=sapply(countries_w_exp_growth,
                          FUN=function(c,X.=X) coef(glm(Confirmed~GrowthDate,data=X[country==c],
                                                        family=poisson)) )

CountryGrowthRates <- rbind(CountryGrowthRates,log(2)/CountryGrowthRates['GrowthDate',])
rownames(CountryGrowthRates)[3] <- 'DoublingTime'

saveRDS(CountryGrowthRates,'RDS/Country_growth_rates')



# Visualization -----------------------------------------------------------

r=CountryGrowthRates['GrowthDate',] %>% mean
log(2)/r
r_sd <- CountryGrowthRates['GrowthDate',] %>% sd
xx <- 1:max(X$GrowthDate,na.rm=T)
pred <- data.table('country'='Prediction',
                   'GrowthDate'=xx,
                   'Crel'=exp(r*(xx-1)),
                   'y_max'=exp((r-2*r_sd)*(xx-1)),
                   'y_min'=exp((r+2*r_sd)*(xx-1)))

pred61 <- data.table('country'='6.1 day doubling time',
                   'GrowthDate'=xx,
                   'Crel'=exp(log(2)/6.1*xx))


r_china <- 0.3414565 ## peak forward from file 2_China_COVID_models
pred_China <- data.table('country'='China_SEIR Estimate',
                         'GrowthDate'=xx,
                         'Crel'=exp(r_china*(xx-1))) 

X[GrowthPhase==TRUE,Crel:=Confirmed/Confirmed[1],by=country]
ggplot(X,aes(GrowthDate,Crel,color=country))+
  geom_line()+
  geom_point(cex=2,pch=4)+
  geom_ribbon(data=pred,aes(ymax=y_max,ymin=y_min),alpha=0.2,col='black')+
  geom_line(data=pred,lwd=2,col='red')+
  geom_line(data=pred_China,lwd=2)+
  geom_line(data=pred61,lwd=2,lty=2)+
  scale_y_continuous(trans='log',breaks=c(2^(0:10)),name='Growth Relative to Initial Exp Phase Cases')+
  ggtitle('Growth Rates Across Countries')


ggsave('figures/Growth_rate_comparison.png',width=8,height=8,units='in')





# Manual Curation of curves -----------------------------------------------
### dates of regular exponential growth
# S Korea    2/22 - present
# Iran       2/21 - present
# Italy      2/23 - present
# Germany    2/25 -present
# Diamond Princess   2-7 - 2/19
# China      1/22 - 2/1
# Spain      2/24-present
# France     2/26-present

# We'll trim countries to the time-periods when they exhibit similar rates of growth

### Countries used:
X <- readRDS('RDS/JH_CSSE_COVID_dataset_clean')
x <- X[,list(Confirmed=sum(Confirmed),
             Deaths=sum(deaths),
             NewCases=sum(NewCases)),by=c('country','date')]
cntries <- c('Italy','Korea, South','Iran','China','Germany','Cruise Ship','Spain','France')
top_countries <- x[country %in% cntries]
md <- '2020-03-03'

## this table helps us merge start/stop times for regular growth
ref=data.table('country'=cntries,
               'start'=as.Date(c('2020-02-23','2020-02-22','2020-02-21','2020-01-22','2020-02-25','2020-02-07','2020-02-26','2020-02-26')),
               'stop'=as.Date(c(md,md,md,'2020-01-28',md,'2020-02-19',md,md)))

setkey(ref,country)
setkey(top_countries,country,date)
top_countries <- top_countries[ref]
top_countries[,GrowthPhase:=(date>=start & date<=stop)]
top_countries <- top_countries[date<=as.Date(md)]

### plots
g1=ggplot(top_countries[!is.na(Confirmed)],aes(date,Confirmed,color=country))+
  geom_line(lwd=1.5)+
  scale_y_continuous(trans='log',breaks=c(1,10,100,1000))+
  ggtitle('Top Country Confirmed Case Dynamics, relative to initial count')
g2=ggplot(top_countries,aes(date,Confirmed,color=country))+
  geom_line(lwd=1,lty=2)+
  geom_line(data=top_countries[GrowthPhase==TRUE],lwd=2)+
  scale_y_continuous(trans='log',breaks=c(1,10,100,1000))+
  theme(legend.position = 'none')+
  ggtitle('Periods of regular exponential growth')

top_countries[,GrowthDate:=-10]
top_countries[GrowthPhase==TRUE,GrowthDate:=as.double(1:.N),by=country]
top_countries[GrowthDate<0,GrowthDate:=NA]

coefs=sapply(unique(G$country),
             FUN=function(c,G.=G) coef(glm(Confirmed~GrowthDate,data=top_countries[country==c],family=poisson)) )
coefs <- rbind(coefs,log(2)/coefs['GrowthDate',])
rownames(coefs)[3] <- 'DoublingTime'

saveRDS(coefs,'RDS/Country_growth_rates_manual_curation_march_3')

# Italy growth rates ------------------------------------------------------

X_italy <- read.csv('COVID-19/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv',stringsAsFactors = F)

X_italy$data <- as.Date(X_italy$data)

r <- mean(CountryGrowthRates['GrowthDate',])
pred <- data.frame('data'=X_italy$data,
                   'deceduti'=X_italy$deceduti[1]*exp(r*as.numeric(X_italy$data-min(X_italy$data))))

g1=ggplot(X_italy,aes(data,deceduti))+
  geom_line(lwd=2)+
  geom_point(pch=8,cex=4)+
  geom_line(data=pred,lwd=2,col='red')+
  scale_y_continuous(trans='log',breaks=10^(0:3))+
  geom_vline(xintercept = as.Date('2020-03-08'))+
  ggtitle('Deceduti')

pred <- data.frame('data'=X_italy$data,
                   'terapia_intensiva'=X_italy$terapia_intensiva[1]*exp(r*as.numeric(X_italy$data-min(X_italy$data))))

g2=ggplot(X_italy,aes(data,terapia_intensiva))+
  geom_line(lwd=2)+
  geom_point(pch=8,cex=4)+
  geom_line(data=pred,lwd=2,col='red')+
  scale_y_continuous(trans='log',breaks=10^(0:3))+
  geom_vline(xintercept = as.Date('2020-03-08'))+
  ggtitle('Terapia Intensiva')

ggarrange(g1,g2,nrow=1,ncol=2)
ggsave('figures/Italy_deaths_and_ICU_vs_time.png',height=8,width=12,unit='in')
