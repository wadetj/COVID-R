 #reads new HHS symptom file ed_data_county_aggregates_timeseries
#start with new data file 9/30/2020
#HHS aggregates ILI, CLI and ED visits at county

#Input files: updated to commute_results_v8.csv,  10/14/2020
#HHS changed data names and added fields, had to modify lines 44-47

# ed_08_05_20.csv (download from HHS protect)
# ZIP_COUNTY_032020.csv (from HUD to impute missing county fips and state
# statefipscode.txt (map missing state data)
# previous symptom file (ILI_CLI_by_facility XXXX)
#  updated to commute_results9.csv, 10-28-2020

#NOTES UPDATES AND CHANGES
#adds new commute file- v9 10/28/2020   
#Note for week of 11-9, data pulled on 11-10 due to holiday on 11-11
#calculates state level ILI and CLI for facilities with missing data
#Missing facilities
#add variable called stateflag to indicate state data used when no local data
#ILI_CLI_by_facility.txt saved as ILI_CLI_by_facilityS.txt to indicate 
#state imputation (only for 8/12/2020 file)
#01/06/2021, changed date format to YYYYMMDD to save files
#For 01/06/2021, read in 20 days of data from HHS due to taking off last week in Dec
#For 01/06/2021- read in previous file is Dec 23 (two weeks prior)
#For next week- add underscore before date in file names
#Note it appears for week of 01/06 there were no reports from the state of GA after 12/21
#Changed no report criteria to 5 days of no reporting instead of 0
#For 1/13/20 downloaded additional data through 12/20-to account for missing data over holiday
#Missing data for GA seems represented now
#For 1/13 Ohio did not report symptoms since Jan 1
#OH facilities with Kentucky counties in communiting areas still had data
#Added check for states with missing data
#For 1/20- Ohio still not reporting anything from 1/1/21 through 1/19/21
#2/3/21- still no reports for state of Ohio
#2/10/21- still no reports for state of Ohio since Jan 1
#2/24/21- still no reports for Ohio since Jan 1
#03/03/21- still no reports for Ohio since Jan 1
#03/10/21- no reports from Ohio since Jan 1 

rm(list=ls())
starttime<-Sys.time()
library(timsRstuff)
library(data.table)
library(dplyr)

#changed to communte_results_v9 on 10/18/2020
com<-fread(input="https://raw.githubusercontent.com/wadetj/COVID-R/master/data/commute_results_v9.csv", sep=",",  na.strings=c("", "NA", "."))
#read from local directory until git hub is updated
#com<-fread(input="C:/Users/wadet/Documents/covid/commute_results_v9.csv", sep=",",  na.strings=c("", "NA", "."))


#commute file with just work facility and unique FIPS code
comuni<-com[, c("FIPS_IN", "Work_State_Name", "Work_County_Name", "Facility")]
comuni<-unique(comuni)

###EDIT THIS FILE-note change to file name format YYYYMMDD
xtemp<-fread(file="C:/Users/wadet/Documents/covid/ed_20210310.csv", sep=",", na.strings=c("", "NA", "."))

xtemp<-xtemp[!is.na(hospital_county_fips)]

#xtemp<-fread(file="C:/Users/wadet/Documents/covid/ed_data_county_aggregates_timeseries.csv", sep=",", na.strings=c("", "NA", "."))
xtemp[, date:=as.Date(date)]


#drop unnecessary vars
#10/21/2020- raw data set was changed with new fields added, variable selection modified 
#xtemp<-xtemp[, -c(22:45)]
xtemp<-xtemp[, c("hospital_county_fips", "hospital_state_abbreviation",  "daily_covid_like_illness", "daily_influenza_like_illness", "daily_er_visits", "date")]


#fips seems to be in correct format and numeric

setnames(xtemp, c("hospital_county_fips", "hospital_state_abbreviation",  "daily_covid_like_illness", "daily_influenza_like_illness", "daily_er_visits"), c("fips", "state", "cli", "ili", "total"))
symps<-xtemp[, c("fips", "state", "date", "cli", "ili", "total")]
                                                                        
#confirm no missing fips codes
table(is.na(symps$fips))

#confirm dates are represented
symps[, .N, by=.(date)]
#xtemp<-rbindlist(list(xtemp, nazips), use.names=TRUE)
symps<-symps[order(fips, date)]
symps[, total:=ifelse(is.na(total), 0, total)]


#Add state totals for later merging
statesymps<-xtemp[, .("cli"=sum(ili), "ili"=sum(cli), "total"=sum(total)),  by=.(state, date)]

#fips from facilities that are not present in hospital data
fipsc<-c(symps$fips, com$FIPS_OUT)
fipcs<-na.omit(fipsc)
fipsc<-unique(fipsc)

ds<-seq.Date(min(symps$date), max(symps$date), "days")
fills<-expand.grid(fipsc, ds)

names(fills)<-c("fips", "date")
fills<-as.data.table(fills)

symps<-merge(symps, fills, by=c("fips", "date"), all=T)

symps[, cli:=ifelse(is.na(cli), 0, cli)]
symps[, ili:=ifelse(is.na(ili), 0, ili)]
symps[, reported:=ifelse(is.na(total), 0, 1)]
symps[, total:=ifelse(is.na(total), 0, total)]

sympscom<-merge(com, symps, by.x="FIPS_OUT", by.y="fips", all.x=T)
sympscom<-sympscom[!is.na(date)]

#fill ins for states with missing data
#add DC, PR and VI to state.abb and state.name
stateab<-c(state.abb, "DC", "VI", "PR")
statename2<-c(state.name, "District of Columbia", "Virgin Islands", "Puerto Rico")
fillstates<-expand.grid(stateab, ds)
fillstates<-as.data.table(fillstates)
names(fillstates)<-c("state", "date")

#setnames(statesymps, "N", "total")
statesymps<-merge(fillstates, statesymps, by=c("state", "date"), all.x=T)
statesymps[, cli:=ifelse(is.na(cli), 0, cli)]
statesymps[, ili:=ifelse(is.na(ili), 0, ili)]
statesymps[, reported:=ifelse(is.na(total), 0, 1)]
statesymps[, total:=ifelse(is.na(total), 0, total)]
statesymps[, clipct:=(cli/total)*100]
statesymps[, ilipct:=(ili/total)*100] 

#change names for state data
setnames(statesymps, c("cli", "ili", "total", "reported", "clipct", "ilipct"), c("cli.state", "ili.state", "total.state", "reported.state", "clipct.state", "ilipct.state"))
#check for missing and unreported data at state level
#Ignore HI, PR, VI
#flag anything else >5 or so
table(statesymps$state[statesymps$total==0])
#statesymps[state=="OH"]


#NEED TO ACCOUNT FOR MULTIPLE FACILITIES IN SAME FIPS_IN

sympscom<-sympscom[, .("cli"=sum(cli), "ili"=sum(ili), "total"=sum(total), "FIPS_IN"=mean(FIPS_IN), "reported"=sum(reported)), by=.(Facility, date)]
sympscom[, clipct:=(cli/total)*100]
sympscom[, ilipct:=(ili/total)*100]
sympscom<-merge(sympscom, comuni, by="FIPS_IN", allow.cartesian=TRUE)
sympscom<-sympscom[, -c("Facility.y")]

sympscom<-setnames(sympscom, "Facility.x", "Facility")

sympscom<-sympscom[order(Facility, date)]

sympscom<-unique(sympscom)

sympscom$reported=ifelse(sympscom$reported>0, 1, 0)

#flag facilities with no reports for week
#For week of 1/06/2021, changed noreport to less than 5
#will need to revisit this to account for no reporting from Georgia

noreport<-sympscom[, .("report"=sum(reported), "fips"=mean(FIPS_IN)), by=.(Facility)]
#just check facilities with less than 10 reports
noreport[report<10,]
noreport[report<15,]
#for 1/13 include two ohio facilties with 12 missing reports
#hand checked, missing from 1/1
#Steinmart Plaza and Islander Park - both Ohio, use state data
# no data for ohio in 2021 so far
noreport<-noreport[report<10, ]

print(noreport)
# merge state data to symptoms and then substitute for non-reporting?
#this  converts state names to abbreviations, except PR, DC, VI
sympscom$stateab<-stateab[match(sympscom$Work_State_Name, statename2)]


#sympscom2 merges state data, sympscom does not

sympscom2<-merge(statesymps, sympscom, by.x=c("state", "date"), by.y=c("stateab", "date"), all.y=TRUE)
sympscom2$noreport<-sympscom2$Facility %in% noreport$Facility

# update with state level data for sites without data
# for sites with no reported data
# substitute state level data for ili cli total, etc.

sympscom2$ili<-ifelse(sympscom2$noreport==TRUE, sympscom2$ili.state, sympscom2$ili)
sympscom2$cli<-ifelse(sympscom2$noreport==TRUE, sympscom2$cli.state, sympscom2$cli)
sympscom2$ilipct<-ifelse(sympscom2$noreport==TRUE, sympscom2$ilipct.state, sympscom2$ilipct)
sympscom2$clipct<-ifelse(sympscom2$noreport==TRUE, sympscom2$clipct.state, sympscom2$clipct)
sympscom2$total<-ifelse(sympscom2$noreport==TRUE, sympscom2$total.state, sympscom2$total)


#define minimal ili and cli

group19<-c("Connecticut|Maine|Massachusetts|New Hampshire|Rhode Island|Vermont|Delaware|District of Columbia|Maryland|Pennsylvania|Virginia|West Virginia|Illinois|Indiana|Michigan|Minnesota|Ohio|Wisconsin")
group24<-c("Alabama|Florida|Georgia|Kentucky|Mississippi|North Carolina|South Carolina|Tennessee|Arizona|California|Hawaii|Nevada")
group32<-c("New Jersey|New York|Puerto Rico|Virgin Islands")
group38<-c("Arkansas|Louisiana|New Mexico|Oklahoma|Texas")
group17<-c("Iowa|Kansas|Missouri|Nebraska")
group15<-c("Alaska|Idaho|Oregon|Washington")
group27<-c("Colorado|Montana|North Dakota|South Dakota|Utah|Wyoming")

#sets sympscom to sympscom2 to include imputed state data
sympscom<-sympscom2

sympscom$minili<-0
sympscom$minili<-ifelse(grepl(group19, sympscom$Work_State_Name) & sympscom$ilipct<=1.9, 1, 0)
sympscom$minili<-ifelse(grepl(group24, sympscom$Work_State_Name) & sympscom$ilipct<=2.4, 1, sympscom$minili)
sympscom$minili<-ifelse(grepl(group32, sympscom$Work_State_Name) & sympscom$ilipct<=3.2, 1, sympscom$minili)
sympscom$minili<-ifelse(grepl(group38, sympscom$Work_State_Name) & sympscom$ilipct<=3.8, 1, sympscom$minili)
sympscom$minili<-ifelse(grepl(group17, sympscom$Work_State_Name) & sympscom$ilipct<=1.7, 1, sympscom$minili)
sympscom$minili<-ifelse(grepl(group15, sympscom$Work_State_Name) & sympscom$ilipct<=1.5, 1, sympscom$minili)
sympscom$minili<-ifelse(grepl(group27, sympscom$Work_State_Name) & sympscom$ilipct<=2.7, 1, sympscom$minili)

sympscom$minili<-ifelse(is.na(sympscom$ilipct), NA, sympscom$minili)


sympscom$mincli=0
sympscom$mincli<-ifelse(sympscom$clipct<=1.77, 1, 0)
sympscom$mincli<-ifelse(is.na(sympscom$clipct), NA, sympscom$mincli)

sympscom$stateflag<-ifelse(sympscom$noreport==TRUE, "YES", "NO")
#print records where state data was imputed
sympscom[stateflag=="YES" & !is.na(clipct), ]
table(sympscom$Facility[sympscom$stateflag=="YES" & !is.na(sympscom$clipct)])


#read in prior file for autoregression and format 

#For 1/13 will need to update to new file naming convention YYYYMMDD
#prevsymp<-read.table("C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/Symptoms/ILI_CLI_by_facility_7_22_20.txt", sep=";", stringsAsFactors=FALSE, na.strings=c("", "NA", "."), header=TRUE, quote="\"")
prevsymp<-read.table("C:/Users/wadet/Documents/covid/ILI_CLI_by_facility_20210303.txt", sep=";", stringsAsFactors=FALSE, na.strings=c("", "NA", "."), header=TRUE, quote="\"")

names(sympscom)

sympscomili<-dplyr::select(sympscom, c("Facility", "date", "ili", "ilipct", "minili", "total", "Work_County_Name", "Work_State_Name", "reported", "stateflag"))
names(sympscomili)<-c("Facility", "ed_date", "count", "percent", "minimal", "total_ed_visits", "Work_County_Name", "Work_State_Name", "reported", "stateflag")  
sympscomili$symptom="ILI"

sympscomcli<-dplyr::select(sympscom, c("Facility", "date", "cli", "clipct", "mincli", "total", "Work_County_Name", "Work_State_Name", "reported", "stateflag"))
names(sympscomcli)<-c("Facility", "ed_date", "count", "percent", "minimal", "total_ed_visits", "Work_County_Name", "Work_State_Name", "reported", "stateflag")  
sympscomcli$symptom="CLI"

allsymps<-rbind.data.frame(sympscomili, sympscomcli)

colorder<-c("Facility", "Work_County_Name", "Work_State_Name", "ed_date", "total_ed_visits", "count", "percent", "minimal","stateflag", "reported", "symptom")

allsymps<-allsymps[, ..colorder]
#change to factors for merging with previous data and consistency in output
allsymps$reported<-factor(allsymps$reported, labels=c("NO", "YES"))
allsymps$minimal<-factor(allsymps$minimal, labels=c("NO", "YES"))

allsymps<-allsymps[order(Facility, ed_date, symptom)]
allsymps$index=1

prevsymp$reported<-factor(prevsymp$reported)
prevsymp$minimal<-factor(prevsymp$minimal)

prevsymp$ed_date<-as.Date(prevsymp$ed_date, "%d %b %Y")

#resets stateflag to NA if it does not exist, otherwise keeps it
if("stateflag" %in% names(prevsymp)==FALSE) prevsymp$stateflag=NA


#$stateflag<-ifelse("stateflag" %in% names(prevsymp), prevsymp$stateflag, NA)
#prevsymp$stateflag=NA
prevsymp$index=0

allsymps2<-rbind.data.frame(allsymps, prevsymp)

temp<-cbind.data.frame(allsymps2$Facility, allsymps2$ed_date, allsymps2$symptom)
allsymps2$dupflag<-ifelse(duplicated(temp)|duplicated(temp, fromLast=TRUE), 1, 0)
drops<-allsymps2$dupflag==1 & allsymps2$index==0
allsymps2<-allsymps2[!drops, ]

allsymps2<-allsymps2[, -c("index", "dupflag")]

allsymps2<-allsymps2[order(Facility, ed_date, symptom)]

#EDIT THIS EVERY TIME keep dates within at least 5 weeks
allsymps2<-allsymps2[allsymps2$ed_date>=as.Date("2021-01-17"), ]


#format dates like SAS
allsymps2$ed_date<-toupper(format(allsymps2$ed_date, "%d%b%Y"))

#CHANGE/EDIT FILE NAMES EVERY RUN
#note change to date format in file name
#write.table(allsymps2, "C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/Symptoms/allsymps2729.txt", row.names=FALSE, na="", sep=";", quote=FALSE)
write.table(allsymps2, "C:/Users/wadet/Documents/covid/allsymps20210310.txt", row.names=FALSE, na="", sep=";", quote=FALSE)
write.table(allsymps2, "C:/Users/wadet/Documents/covid/ILI_CLI_by_facility_20210310.txt", row.names=FALSE, na="", sep=";", quote=FALSE)


#allsymps2[ed_date=="01MAR2021" & minimal=="NO"]

#table(allsymps2$minimal[allsymps2$ed_date=="01MAR2021"])

#table(allsymps2$minimal[allsymps2$ed_date=="02JAN2021"])



#endtime<-Sys.time()
#endtime-starttime


#code to check with raw aggregated data file

# xx<-filter(xtemp, date==as.Date("2020-09-01") & (fips ==17031 | fips==17043 | fips==17089 |fips==17097 | fips==17111 | fips==17197 | fips==18089))
# sum(xx$total)
# sum(xx$cli)
# sum(xx$ili)

#code to compare with SAS output
#My code does not keep last day of period when there is no data (in this case 7-22)

# xx<-read.delim(file="C:/Users/wadet/Documents/covid/ILI_CLI_by_facility_7_29_20.txt", sep=";")
# 
# # xx<-read.delim(file="C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/Symptoms/ILI_CLI_by_facility_7_22_20.txt", sep=";")
# xx$date<-as.Date(xx$ed_date, "%d %b %Y")
#  xx2<-xx[xx$date>=as.Date("2020-07-14"), ]
 #xx2<-xx2[xx2$symptom=="ILI", ]
# xx3<-merge(xx2, sympscom, by=c("Facility", "date"), all=T)
# describe(symmpsconv$percent)
# describe(xx3$ilipct)
# table(xx2$minimal)
# table(sympscom$minili)
# table(xx2$minimal)
# 
#Compared 7/29/2020 output, matched exactly for all intents and purposes
# only differences were my file has 0 for counts for days without reports
# and in number of decimal places

# CODE TO COMPARE STATE AND FACILITY LEVEL ILI/CLI
# library(ggplot2)
# dev.off()
# 
# pdf(file="C:/Users/wadet/Documents/covid/CAcli.pdf")
# sympscom2 %>%
# filter(state=="CA") %>%
# ggplot(aes(date, clipct)) + geom_point(color="blue") + geom_point(aes(date, clipct.state), color="red")
# dev.off()
# 
# pdf(file="C:/Users/wadet/Documents/covid/CAili.pdf")
# sympscom2 %>%
#         filter(state=="CA") %>%
#         ggplot(aes(date, ilipct)) + geom_point(color="blue") + geom_point(aes(date, ilipct.state), color="red")
# dev.off()
# 
# pdf(file="C:/Users/wadet/Documents/covid/MNili.pdf")
# sympscom2 %>%
# filter(state=="MN") %>%
# ggplot(aes(date, ilipct)) + geom_point(color="blue") + geom_point(aes(date, ilipct.state), color="red")
# dev.off()
# 
# pdf(file="C:/Users/wadet/Documents/covid/MNcli.pdf")
# sympscom2 %>%
# filter(state=="MN") %>%
# ggplot(aes(date, clipct)) + geom_point(color="blue") + geom_point(aes(date, clipct.state), color="red")
# dev.off()
# 
# pdf(file="C:/Users/wadet/Documents/covid/OKili.pdf")
# sympscom2 %>%
#         filter(state=="OK") %>%
#         ggplot(aes(date, ilipct)) + geom_point(color="blue") + geom_point(aes(date, ilipct.state), color="red")
# dev.off()
# 
# pdf(file="C:/Users/wadet/Documents/covid/OKcli.pdf")
# sympscom2 %>%
#         filter(state=="OK") %>%
#         ggplot(aes(date, clipct)) + geom_point(color="blue") + geom_point(aes(date, clipct.state), color="red")
# dev.off()
# 
