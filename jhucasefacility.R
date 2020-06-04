
rm(list=ls())

library(ggplot2)
library(dplyr)
library(knitr)
library(xtable)
library(timsRstuff)
library(openxlsx)

source('C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/covidtsmav.R')

fac<-read.csv("C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/facility_commutes.csv", stringsAsFactors=FALSE)


facloc<-unique(fac$FIPS_IN)
facloc<-facloc[order(facloc)]

facsplit<-split(fac$FIPS_OUT, fac$FIPS_IN)
read.jhu.ts(count=c(facsplit[[2]]))



dat<- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv", stringsAsFactors=FALSE)

facls<-split(fac$FIPS_OUT, fac$FIPS_IN)
for(i in 1:length(facls)) {
  tname<-paste("FIPS", facloc[[i]], sep="")
  tdat<-read.jhu.ts(county=c(facls[[i]]), read=FALSE)
  assign(tname, tdat)
}


  
#rm(list=(grep("FIPS", ls(), value=TRUE)))


FIPS37135 %>%
   filter(x14==1) %>%
     ggplot(aes(date, newcases))+geom_bar(stat="identity")+geom_line(aes(date, newcases.lag), color="red", lwd=2)+geom_smooth(color="blue", lwd=2)
  



FIPS37135 %>%
  filter(x21==1) %>%
  ggplot(aes(date, newcases))+geom_bar(stat="identity")+geom_line(aes(date, newcases.lag), color="red", lwd=2)+geom_smooth(color="blue", lwd=2)


FIPS37135

