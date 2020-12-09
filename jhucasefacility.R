
#read in JHU data and create data frames for each EPA facility/commuting area
#misc analysis, graphs for Chapel Hill
#read in latest commute file
#add function and print highest and lowest incident rates by facility (calcrates)

rm(list=ls())

library(ggplot2)
library(dplyr)
library(knitr)
library(xtable)
library(timsRstuff)
library(openxlsx)
library(MASS)

#source('C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/covidtsmav.R')
source("https://raw.githubusercontent.com/wadetj/COVID-R/master/covidtsmav.R")



#fac<-read.csv("C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/facility_commutes.csv", stringsAsFactors=FALSE)

#fac<-read.csv("https://raw.githubusercontent.com/wadetj/COVID-R/master/data/facility_commutes.csv", stringsAsFactors=FALSE)

fac<-read.csv("https://raw.githubusercontent.com/wadetj/COVID-R/master/data/commute_results_v9.csv", stringsAsFactors=FALSE)


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
  tname<-paste("FIPS", facloc[i], sep="")
  tdat<-read.jhu.ts(county=c(facls[[i]]), read=FALSE)
  popdat<-filter(pops, pops[,1] %in% facls[[i]])
  population<-sum(popdat$population)
  tdat<-cbind.data.frame(tdat, population)
  assign(tname, tdat)
}

#calculate 2 week incidence 
sum(FIPS37135$newcases[FIPS37135$x14==1])/(FIPS37135$population[1])*100000
#For SAS comparisons
#write.csv(FIPS37135, "C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/FIPS37135.csv", row.names=FALSE, na=".")

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



#autorgression model for HSF
otemp<-lm(newcases.lag~date+lag(newcases.lag, 1)+lag(newcases.lag, 2)+lag(newcases.lag, 3)+lag(newcases.lag, 4)+lag(newcases.lag, 5)+lag(newcases.lag, 6)+lag(newcases.lag, 7), data=FIPS37135, subset=x21==1)
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


#Narragansett


FIPS44009 %>%
  filter(x14==1) %>%
  ggplot(aes(date, newcases))+geom_bar(stat="identity")+geom_line(aes(date, newcases.lag), color="red", lwd=2)+geom_smooth(color="blue", lwd=2)+geom_smooth(color="blue", lwd=2)


FIPS44009 %>%
  filter(x28==1) %>%
  ggplot(aes(date, newcases))+geom_bar(stat="identity")+geom_line(aes(date, newcases.lag), color="red", lwd=2)+geom_smooth(color="blue", lwd=2)+geom_smooth(color="blue", lwd=2)


#use fips lists to calculate and print highest and lowest facilities

fipsin<-unique(fac$FIPS_IN)
fipslist<-paste("FIPS", fipsin, sep="")


calcrates<-function(fipsdf, win=14){
  if(win==14) ir<-sum(fipsdf$newcases[fipsdf$x14==1])/(fipsdf$population[1])*100000
  if(win==21) ir<-sum(fipsdf$newcases[fipsdf$x21==1])/(fipsdf$population[1])*100000
  if(win==28) ir<-sum(fipsdf$newcases[fipsdf$x28==1])/(fipsdf$population[1])*100000
  if(win==7) ir<-sum(fipsdf$newcases[fipsdf$x7==1])/(fipsdf$population[1])*100000
  fipsname<-fipsdf$FIPS[1]
  return(list(ir=ir, fips=fipsname, window=win))
}

calcrates(FIPS11001)



#irs<-data.frame(fips=NA, ir=NA)
#for all FIPS codes
irs<-NULL


for(i in 1:length(fipslist)){
  tname<-get(fipslist[i])
  xtemp<-calcrates(tname, win=14)
  #irs$fips[i]<-xtemp$fips
  irs[i]<-xtemp$ir
}


irdf<-cbind.data.frame(fipslist, irs)
irdf$fipscode<-gsub("FIPS", "", irdf$fipslist)
facshort<-dplyr::select(fac, c("FIPS_IN", "Work_State_Name", "Work_County_Name", "Facility"))
facshort<-unique(facshort)
irfac<-merge(irdf, facshort, by.x="fipscode", by.y="FIPS_IN")
irfac<-irfac[irfac$ir!=Inf, ]

ord<-order(irfac$irs)
ord<-rev(ord)
irfac<-irfac[ord, ]

irfac

#top 20 highest
irfac %>%
  dplyr::select(c("irs", "Work_State_Name", "Facility")) %>%
  dplyr::slice_head(n=20)


#top 20 lowest
irfac %>%
  dplyr::select(c("irs", "Work_State_Name", "Facility")) %>%
  dplyr::slice_tail(n=20)

 
  
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


#Cincinnati FIPS39061


FIPS39061 %>%
  filter(x21==1) %>%
  ggplot(aes(date, newcases))+geom_bar(stat="identity")+geom_line(aes(date, newcases.lag), color="red", lwd=2)+geom_smooth(color="blue", lwd=2)




sel<-grep("District", fac$Work_State_Name)
dc<-fac[sel, ]

dcfips<-dc$FIPS_OUT
dcfips<-unique(dcfips)

for(i in 1:length(dcfips)) {
  tname<-paste("c", dcfips[i], sep="")
  tdat<-read.jhu.ts(county=dcfips[i], read=FALSE)
  popdat<-filter(pops, pops[,1] %in% dcfips[i])
  population<-sum(popdat$population)
  tdat<-cbind.data.frame(tdat, population)
  assign(tname, tdat)
}

IR<-NULL
for(i in 1:length(dcfips)) {
xname<-paste("c", dcfips[i], sep="")
xdat<-get(xname)
IR[i]<-round(sum(xdat$newcases[xdat$x14==1])/(xdat$population[1])*100000, 2)
#print(paste(xdat, "=",  IR, sep=""))
print(paste(dcfips[i], "=", IR[i]), quote=FALSE)

}

dcIR<-round(sum(FIPS11001$newcases[xdat$x14==1])/(FIPS11001$population[1])*100000, 2)
print(paste("DC=", dcIR), quote=FALSE)

IR<-cbind.data.frame(dcfips, IR)


for(i in 1:length(facls)) {
  for(j in 1:length(facls[[i]])) {
      facfips<-paste(facls[[i]]][j], sep="")
      tcdat<-read.jhu.ts(county=facfips, read=FALSE)
      popdat<-filter(pops, pops[,1] %in% facfips)
      population<-sum(popdat$population)
      tdat<-cbind.data.frame(tdat, population)
      assign(tname, tdat)
}

IR<-NULL
for(i in 1:length(dcfips)) {
  xname<-paste("c", dcfips[i], sep="")
  xdat<-get(xname)
  IR[i]<-round(sum(xdat$newcases[xdat$x14==1])/(xdat$population[1])*100000, 2)
  #print(paste(xdat, "=",  IR, sep=""))
  print(paste(dcfips[i], "=", IR[i]), quote=FALSE)
  
}

dcIR<-round(sum(FIPS11001$newcases[xdat$x14==1])/(FIPS11001$population[1])*100000, 2)
print(paste("DC=", dcIR), quote=FALSE)

IR<-cbind.data.frame(dcfips, IR)
print(IR)


