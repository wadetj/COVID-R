rm(list=ls())

library(ggplot2)
library(dplyr)
library(knitr)
library(xtable)
library(reshape)
library(timsRstuff)



read.jhu.ts<-function(county, lag=3, read=TRUE) {
  if(read) dat<- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv", stringsAsFactors=FALSE)
  
  dat<-filter(dat, FIPS %in% county)
  dat<-select(dat, c(FIPS, starts_with("X")))
  
  if(nrow(dat>1)) { 
      temp<-apply(dat[, 2:ncol(dat)], 2, sum, na.rm=T)
      dat<-c(dat$FIPS[1], temp)
      names(dat)[1]<-"FIPS"
      dat<-as.data.frame(t(dat))
      rm(temp)
    }
  
  dat<-melt(dat, id="FIPS")
  dat$date<-substr(dat$variable, 2, 10)
  dat$date<-as.Date(dat$date, "%m.%d.%y")
  newcases<-diff(dat$value)
  dat$newcases<-c(0, newcases)
  dat$newcases.lag<-mav2(dat$newcases, lag, fill=TRUE)
  dat<-dat[, -2]
  x14<-c(rep(0, dim(dat)[1]-14), rep(1, 14))
  x21<-c(rep(0, dim(dat)[1]-21), rep(1, 21))
  dat<-cbind.data.frame(dat, x14, x21)
  dat<-filter(dat, date>as.Date("2020-03-20",  "%Y-%m-%d"))
  return(dat)
}

# 
 #dc<-read.jhu.ts(county=37063)
# oc<-read.jhu.ts(county=37135)
# 
# ggplot(dc, aes(date, newcases))+geom_bar(stat="identity")+geom_line(aes(date, newcases.lag), color="red", lwd=2)+geom_smooth(color="blue", lwd=2)
# dc %>%
# filter(x21==1) %>%
# ggplot(aes(date, newcases))+geom_bar(stat="identity")+geom_line(aes(date, newcases.lag), color="red", lwd=2)+geom_smooth(color="blue", lwd=2)
# 
# dc%>%
# filter(x14==1) %>%
#   ggplot(aes(date, newcases))+geom_bar(stat="identity")+geom_line(aes(date, newcases.lag), color="red", lwd=2)+geom_smooth(color="blue", lwd=2)
# 
# 
# ggplot(oc, aes(date, newcases))+geom_bar(stat="identity")+geom_line(aes(date, newcases.lag), color="red", lwd=2)+geom_smooth(color="blue", lwd=2)
# oc %>%
#   filter(x21==1) %>%
#   ggplot(aes(date, newcases))+geom_bar(stat="identity")+geom_line(aes(date, newcases.lag), color="red", lwd=2)+geom_smooth(color="blue", lwd=2)
#   
#  oc %>%
#     filter(x21==1) %>%  
#    ggplot(aes(date, newcases))+geom_bar(stat="identity")+geom_smooth(method="lm")+geom_point(aes(date, exp(otemp$fitted.values)))
#  
# oc %>%
#    filter(x21==1) %>%  
#    ggplot(aes(date, log(newcases+2)))+geom_bar(stat="identity")+geom_smooth(method="lm")
#  
#   
# oc %>%
#   filter(x14==1) %>%
#   ggplot(aes(date, newcases))+geom_bar(stat="identity")+geom_line(aes(date, newcases.lag), color="red", lwd=2)+geom_smooth(color="blue", lwd=2)
# 
# 
# 
# ggplot(dc, aes(Date, newcases))+geom_bar(stat="identity")+geom_line(aes(Date, newcases.lag), color="red", lwd=2)+geom_smooth(color="blue", lwd=2)
# 
# 
# oc %>% 
#   filter(x14==1) %>%
#   ggplot(aes(Date, newcases))+geom_bar(stat="identity")+geom_line(aes(Date, newcases.lag), color="red", lwd=2)
# 
# 
# 
# 
# 
# rm(list=ls())
# 
# library(ggplot2)
# library(dplyr)
# library(knitr)
# library(xtable)
# library(reshape)
# library(timsRstuff)
# 
# ts<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv", stringsAsFactors=FALSE)
# 
# durham<-ts %>% 
#   filter(FIPS==37063)
# 
# oc<-ts %>%
#   filter(FIPS==37135)
# 
# d2<-select(durham, c(FIPS, starts_with("X")))
# melt(d2, id="FIPS")
# d2l<-melt(d2, id="FIPS")
# d2l$Date<-substr(d2l$variable, 2, 10)
# d2l$Date<-as.Date(d2l$Date, "%m.%d.%y")
# 
# 
# newcases<-diff(d2l$value)
# newcases<-c(0, newcases)
# 
# d2l<-cbind.data.frame(d2l, newcases)
# d2lx<-d2l[40:dim(d2l)[1], ]
# ggplot(d2lx, aes(Date, newcases))+geom_bar(stat="identity")
# 
# newcase5<-mav2(d2lx$newcases, 5, fill=TRUE)
# #newcase5<-c(rep(NA, 4), newcase5)
# 
# ggplot(d2lx, aes(Date, newcases))+geom_bar(stat="identity")+geom_line(aes(Date, newcase5))
# x14<-c(rep(0, dim(d2lx)[1]-14), rep(1, 14))
# d2lx<-cbind.data.frame(d2lx, newcase5, x14)
# 
# d2lx %>% 
#   filter(x14==1) %>%
#   ggplot(aes(Date, newcases))+geom_bar(stat="identity")+geom_line(aes(Date, newcase5), color="red", lwd=2)
# 
# 
# 
# 
# 
# oc2<-select(oc, c(FIPS, starts_with("X")))
# melt(oc2, id="FIPS")
# oc2l<-melt(oc2, id="FIPS")
# oc2l$Date<-substr(oc2l$variable, 2, 10)
# oc2l$Date<-as.Date(oc2l$Date, "%m.%d.%y")
# 
# 
# newcases<-diff(oc2l$value)
# newcases<-c(0, newcases)
# 
# oc2l<-cbind.data.frame(oc2l, newcases)
# oc2lx<-oc2l[40:dim(oc2l)[1], ]
# ggplot(oc2lx, aes(Date, newcases))+geom_bar(stat="identity")
# 
# newcase5<-mav2(oc2lx$newcases, 5, fill=TRUE)
# #newcase5<-c(rep(NA, 4), newcase5)
# 
# ggplot(oc2lx, aes(Date, newcases))+geom_bar(stat="identity")+geom_line(aes(Date, newcase5))
# x14<-c(rep(0, dim(oc2lx)[1]-14), rep(1, 14))
# oc2lx<-cbind.data.frame(oc2lx, newcase5, x14)
# 
# oc2lx %>% 
#   filter(x14==1) %>%
#   ggplot(aes(Date, newcases))+geom_bar(stat="identity")+geom_line(aes(Date, newcase5), color="red", lwd=2)
# 
# 
# 
# 
# 
# fips37063 %>%
#  filter(x14==1) %>%
#  ggplot(aes(Date, newcases)) +geom_bar(stat="identity") +geom_smooth(method="loess", se=FALSE)
# 
# 
# 
# 
# 
# 
# # 
# #  mav2<-function(dat, lag) {
# #   start<-lag+1
# #   lagx<-start-(lag+1)
# #   ln<-length(dat)-lagx
# #   ss<-NULL
# #   
# #   for(i in (start:ln)) { 
# #   
# #   ss<-dat[i-lagx:i]
# #   xx[i]<-mean(ss, na.rm=T)
# #  return(xx)
# #   
# #   }
# #   
# # }
# #   
#  
#  
#  
#  mav2<-function(dat, lagdat) {
#   mn<-NULL
#   ln<-length(dat)
#   
#   for(i in lagdat:ln) {
#     
#     end<-i
#     start<-i-(lagdat-1)
#     #pos<-i-(lagdat-1)
#     mn[start]<-mean(c(dat[start:end]), na.rm=T)
#     
#   }
#   return(mn)
#  }
#  
#    
#  
#  
#  
#   
#   
# mav2(newcases, 5)
# 
