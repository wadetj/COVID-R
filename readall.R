#read in all raw data and create state level data file
#generate plots by state    


rm(list=ls())
library(timsRstuff)
library(data.table)
library(dplyr)
library(ggplot2)


memory.limit(56000)

xtemp1<-fread(file="C:/Users/wadet/Documents/covid/ed_9_16_20.csv", sep=",", na.strings=c("", "NA", "."))
xtemp2<-fread(file="C:/Users/wadet/Documents/covid/ed_9_09_20.csv", sep=",", na.strings=c("", "NA", "."))
xtemp3<-fread(file="C:/Users/wadet/Documents/covid/ed_9_02_20.csv", sep=",", na.strings=c("", "NA", "."))
xtemp4<-fread(file="C:/Users/wadet/Documents/covid/ed_8_26_20.csv", sep=",", na.strings=c("", "NA", "."))
xtemp5<-fread(file="C:/Users/wadet/Documents/covid/ed_8_19_20.csv", sep=",", na.strings=c("", "NA", "."))
xtemp6<-fread(file="C:/Users/wadet/Documents/covid/ed_8_12_20.csv", sep=",", na.strings=c("", "NA", "."))
xtemp7<-fread(file="C:/Users/wadet/Documents/covid/ed_8_05_20.csv", sep=",", na.strings=c("", "NA", "."))
xtemp8<-fread(file="C:/Users/wadet/Documents/covid/ed_7_29_20.csv", sep=",", na.strings=c("", "NA", "."))
xtemp9<-fread(file="C:/Users/wadet/Documents/covid/ed_07_22_20.csv", sep=",", na.strings=c("", "NA", "."))
xtempall<-rbindlist(list(xtemp1, xtemp2, xtemp3, xtemp4, xtemp5, xtemp6, xtemp7, xtemp8, xtemp9), use.names=T)


xtempall<-xtempall[, -c(1:5, 8, 9, 10, 11, 13, 14, 17, 18, 19, 23, 24, 25, 26, 31)]
xtempall[, date:=as.Date(substr(c_visit_date_time, 1, 10))]

statesympsall<-xtempall[, .("cli"=sum(covid_like_illness), "ili"=sum(influenza_like_illness), "N"=.N),  by=.(state, date)]

statesympsall %>%
filter(state=="OR") %>%
ggplot(aes(date, cli))+geom_point()+geom_smooth()
statesympsall %>%
filter(state=="OR") %>%
ggplot(aes(date, N))+geom_point()+geom_smooth()
statesympsall %>%
filter(state=="CA") %>%
ggplot(aes(date, N))+geom_point()+geom_smooth()

statesympsall %>%
filter(state=="CA") %>%
ggplot(aes(date, ili))+geom_point()+geom_smooth()


statesympsall %>%
  filter(state=="CA") %>%
  ggplot(aes(date, cli))+geom_point()+geom_smooth()


