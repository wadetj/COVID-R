#read in JHU data and create data frames for each EPA facility/commuting area

rm(list=ls())

library(ggplot2)
library(dplyr)
library(knitr)
library(xtable)
library(timsRstuff)
library(openxlsx)
library(MASS)

source('C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/covidtsmav.R')

#fac<-read.csv("C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/facility_commutes.csv", stringsAsFactors=FALSE)

fac<-read.csv("https://raw.githubusercontent.com/wadetj/COVID-R/master/data/facility_commutes.csv", stringsAsFactors=FALSE)

#read in population data
#pops<-read.csv("C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/covid_county_population_usafacts.csv", stringsAsFactors=FALSE)

pops<-read.csv("https://raw.githubusercontent.com/wadetj/COVID-R/master/data/covid_county_population_usafacts.csv", stringsAsFactors=FALSE)
pops<-filter(pops, pops[,1] %in% fac$FIPS_OUT)

#popsfips<-pops[,1]

#not used
#popsplit<-split(popsfips, fac$FIPS_IN)


facloc<-unique(fac$FIPS_IN)
facloc<-facloc[order(facloc)]

#facsplit<-split(fac$FIPS_OUT, fac$FIPS_IN)
#read.jhu.ts(county=c(facsplit[[2]]))

#rm(list=(grep("FIPS", ls(), value=TRUE)))

dat<- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv", stringsAsFactors=FALSE)

#list of facilities grouped by commuting area
facls<-split(fac$FIPS_OUT, fac$FIPS_IN)

for(i in 1:length(facls)) {
  tname<-paste("FIPS", facloc[[i]], sep="")
  tdat<-read.jhu.ts(county=c(facls[[i]]), read=FALSE)
  popdat<-filter(pops, pops[,1] %in% facls[[i]])
  population<-sum(popdat$population)
  tdat<-cbind.data.frame(tdat, population)
  assign(tname, tdat)
}

#calculate 2 week incidence 
(sum(FIPS37135$newcases[FIPS37135$x14==1])/FIPS37135$population[1])*100000


#two week plot for HSF
FIPS37135 %>%
   filter(x14==1) %>%
     ggplot(aes(date, newcases))+geom_bar(stat="identity")+geom_line(aes(date, newcases.lag), color="red", lwd=2)+geom_smooth(color="blue", lwd=2)+geom_smooth(color="blue", lwd=2)


#autorgression model for HSF
otemp<-lm(newcases.lag~date+lag(newcases.lag, 1)+lag(newcases.lag, 2)+lag(newcases.lag, 3)+lag(newcases.lag, 4)+lag(newcases.lag, 5)+lag(newcases.lag, 6)+lag(newcases.lag, 7), data=FIPS37135, subset=x14==1)
otempstep<-stepAIC(otemp)
summary(otempstep)
glmCIs(otemp)
glmCIs(otempstep)


FIPS37135 %>%
  filter(x21==1) %>%
  ggplot(aes(date, newcases))+geom_bar(stat="identity")+geom_line(aes(date, newcases.lag), color="red", lwd=2)+geom_smooth(color="blue", lwd=2)


FIPS37135 %>%
  filter(x14==1) %>%
  ggplot(aes(date, newcases))+geom_bar(stat="identity")+geom_line(aes(date, newcases.lag), color="red", lwd=2)+geom_smooth(color="blue", lwd=2)


FIPS37135 %>%
  filter(x14==1) %>%
  ggplot(aes(date, newcases.lag))+geom_point()+geom_smooth(color="blue", lwd=2)+geom_smooth(method="lm", color="black", lwd=2)

# Additional examples

# FIPS25017 %>%
#   filter(x21==1) %>%
#   ggplot(aes(date, newcases))+geom_bar(stat="identity")+geom_line(aes(date, newcases.lag), color="red", lwd=2)+geom_smooth(color="blue", lwd=2)+geom_smooth(method="lm", color="black", lwd=2, se=FALSE)
# 
# 
# 
# 
# FIPS25017 %>%
#   filter(x14==1) %>%
#   ggplot(aes(date, newcases))+geom_bar(stat="identity")+geom_line(aes(date, newcases.lag), color="red", lwd=2)+geom_smooth(color="blue", lwd=2)+geom_smooth(method="lm", color="black", lwd=2, se=FALSE)
# 
# 
# 
# FIPS25017 %>%
#   filter(x14==1) %>%
#   ggplot(aes(date, newcases.lag))+geom_bar(stat="identity")+geom_smooth(color="blue", lwd=2)+geom_smooth(method="lm", color="black", lwd=2, se=FALSE)
# 
# 
# 
# 
# FIPS25017 %>%
#   filter(x14==1) %>%
#   ggplot(aes(date, newcases.lag))+geom_bar(stat="identity")+geom_line(aes(date, xx$fitted.values), lwd=2, color="black")+geom_line(aes(date, otemp$fitted.values), lwd=2, color="red")+geom_line(aes(date, xx$fitted.values), lwd=2, color="blue")
# 
# 
# 
# FIPS25017 %>%
#   filter(x14==1) %>%
#   ggplot(aes(date, newcases.lag))+geom_bar(stat="identity")+geom_line(aes(date, otemp$fitted.values), lwd=2, color="red")+geom_line(aes(date, xx$fitted.values), lwd=2, color="blue")
# 
# 
# zz+geom_abline(intercept=100, slope=10, lwd=2, color="black")
# 
