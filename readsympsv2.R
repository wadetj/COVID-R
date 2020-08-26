
#Input files: commute_results_v6,csv
# ed_08_05_20.csv (download from HHS protect)
# ZIP_COUNTY_032020.csv (from HUD to impute missing county fips and state
# statefipscode.txt (map missing state data)
# previous symptom file (ILI_CLI_by_facility XXXX)

#Uses zipcode to impute county for records with missing counties
#Also imputes missing state data for these records

#calculates state level ILI and CLI for facilities with missing data
#Missing facilities

#add variable called stateflag to indicate state data used when no local data
#ILI_CLI_by_facility.txt saved as ILI_CLI_by_facilityS.txt to indicate 
#state imputation (only for 8/12/2020 file)

rm(list=ls())
#starttime<-Sys.time()
library(timsRstuff)
library(data.table)
library(dplyr)

#com<-fread(file="C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/commute_results_v6.csv", sep=",",  na.strings=c("", "NA", "."))
#com<-fread(file="C:/Users/wadet/Documents/covid/commute_results_v6.csv", sep=",",  na.strings=c("", "NA", "."))
com<-fread(input="https://raw.githubusercontent.com/wadetj/COVID-R/master/data/commute_results_v6.csv", sep=",",  na.strings=c("", "NA", "."))

#commute file with just work facility and unique FIPS code
comuni<-com[, c("FIPS_IN", "Work_State_Name", "Work_County_Name", "Facility")]
comuni<-unique(comuni)


###EDIT THIS FILE
#xtemp<-fread(file="C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/Symptoms/ed_7_29_20.csv", sep=",", na.strings=c("", "NA", "."))
xtemp<-fread(file="C:/Users/wadet/Documents/covid/ed_8_26_20.csv", sep=",", na.strings=c("", "NA", "."))

xtemp<-xtemp[, -c(1:5, 8, 9, 10, 11, 13, 14, 17, 18, 19, 23, 24, 25, 26, 31)]
xtemp[, date:=as.Date(substr(c_visit_date_time, 1, 10))]
xtemp<-xtemp[, -c("c_visit_date_time", "onset_date", "hospital_fema_region")]

xtemp[, hospital_county_fips:=as.numeric(gsub("\\[|\\]", "",  hospital_county_fips))]
xtemp[, hospital_names:=gsub("\\[|\\]", "",  hospital_names)]
xtemp[, hospital_zips:=as.numeric(gsub("\\[|\\]", "",  hospital_zips))]
xtemp[, hospital_state_abbreviation:=gsub("\\[|\\]", "",  hospital_state_abbreviation)]

setnames(xtemp, "hospital_state_abbreviation", "state")
setnames(xtemp, "hospital_county_fips", "fips")

#add in facilities without zipcode
#uses HUD database available here: https://www.huduser.gov/portal/datasets/usps_crosswalk.html
 #zipstofips<-fread(file="C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/ZIP_COUNTY_032020.csv", sep=",",  na.strings=c("", "NA", ".", "#DIV/0!"))

zipstofips<-fread(input="https://raw.githubusercontent.com/wadetj/COVID-R/master/data/ZIP_COUNTY_032020.csv", 
                        sep=",",  na.strings=c("", "NA", ".", "#DIV/0!"))

#add state information for missing state data
# most if not all records with missing fips are missing state
#although some zips may map to multiple states, we will just use 80% to determine states
#data loss is minimal
#state fips code text file here: https://www.census.gov/library/reference/code-lists/ansi.html#par_textimage_3
 

#define state and merge to zipstofips file
 #zipstofips$strcounty<-as.character(zipstofips$COUNTY)
zipstofips$st<-as.numeric(ifelse(nchar(zipstofips$COUNTY)==4, substr(zipstofips$COUNTY, 1, 1), substr(zipstofips$COUNTY, 1, 2)))
statefips<-read.table("C:/Users/wadet/Documents/covid/statefipscodes.txt", stringsAsFactors=FALSE, sep="|", header=TRUE)
 
zipstofips<-merge(zipstofips, statefips, by.x="st", by.y="STATE", all.x=T)
 
 #select data without fips
nas<-xtemp[is.na(fips)]

 # select county where zips match >80% to county
zipshi<-zipstofips[zipstofips$TOT_RATIO>0.8, ]

nazips<-merge(zipshi, nas, by.x="ZIP", by.y="hospital_zips", all.y=T)

nazips$fips<-ifelse(is.na(nazips$fips), nazips$COUNTY, nazips$fips)
nazips$hospital_zips<-nazips$ZIP
nazips$state<-nazips$STUSAB

nazips<-nazips[, -c(1:10)]

xtemp<-xtemp[!is.na(fips)]

#xtemp<-rbind.data.table(xtemp, nazips)

#check dimensions 
dim(xtemp)
#confirm dates are represented
xtemp[, .N, by=.(date)]
xtemp<-rbindlist(list(xtemp, nazips), use.names=TRUE)


#Add state totals for later merging

edtot<-xtemp[, .N, by=.(fips, date)]
#statetot<-xtemp[, .N, by=.(state, date)]
#cli<-xtemp[covid_like_illness==1, .N, by=.(fips, date)]
#ili<-xtemp[influenza_like_illness==1, .N, by=.(fips, date)]
symps<-xtemp[, .("cli"=sum(covid_like_illness), "ili"=sum(influenza_like_illness)), by=.(fips, date)]
symps<-symps[!is.na(fips)]
symps<-symps[order(fips, date)]

#note code can be modified to include totals in one command as below
statesymps<-xtemp[, .("cli"=sum(covid_like_illness), "ili"=sum(influenza_like_illness), "N"=.N),  by=.(state, date)]

#symps<-xtemp[, .("cli"=sum(covid_like_illness), by=.(fips, date))]

edtot<-edtot[order(fips, date)]
setnames(edtot, "N", "total")
edtot[, total:=ifelse(is.na(total), 0, total)]
edtot<-edtot[!is.na(fips)]

symps<-merge(symps, edtot, all=TRUE)

symps<-symps[!is.na(fips)]


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

setnames(statesymps, "N", "total")
statesymps<-merge(fillstates, statesymps, by=c("state", "date"), all.x=T)
statesymps[, cli:=ifelse(is.na(cli), 0, cli)]
statesymps[, ili:=ifelse(is.na(ili), 0, ili)]
statesymps[, reported:=ifelse(is.na(total), 0, 1)]
statesymps[, total:=ifelse(is.na(total), 0, total)]
statesymps[, clipct:=(cli/total)*100]
statesymps[, ilipct:=(ili/total)*100] 

#change names for state data
setnames(statesymps, c("cli", "ili", "total", "reported", "clipct", "ilipct"), c("cli.state", "ili.state", "total.state", "reported.state", "clipct.state", "ilipct.state"))


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
noreport<-sympscom[, .("report"=sum(reported), "fips"=mean(FIPS_IN)), by=.(Facility)]
noreport<-noreport[report==0, ]

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

### EDIT THIS FILE - need to add quote="\""
#For week of 8/19 only- will need to add "S" to file to read in file with state imputed data
#prevsymp<-read.table("C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/Symptoms/ILI_CLI_by_facility_7_22_20.txt", sep=";", stringsAsFactors=FALSE, na.strings=c("", "NA", "."), header=TRUE, quote="\"")
prevsymp<-read.table("C:/Users/wadet/Documents/covid/ILI_CLI_by_facility_8_19_20.txt", sep=";", stringsAsFactors=FALSE, na.strings=c("", "NA", "."), header=TRUE, quote="\"")

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

#EDIT THIS EVERY TIME keep dates within 5 weeks
allsymps2<-allsymps2[allsymps2$ed_date>=as.Date("2020-07-19"), ]


#format dates like SAS
allsymps2$ed_date<-toupper(format(allsymps2$ed_date, "%d%b%Y"))

#CHANGE FILE NAMES EVERY RUN
#S added to indicated state data
#remove S once this is integrated - S removed as of 8/19
#write.table(allsymps2, "C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/Symptoms/allsymps2729.txt", row.names=FALSE, na="", sep=";", quote=FALSE)
write.table(allsymps2, "C:/Users/wadet/Documents/covid/allsymps20826.txt", row.names=FALSE, na="", sep=";", quote=FALSE)
write.table(allsymps2, "C:/Users/wadet/Documents/covid/ILI_CLI_by_facility_8_26_20.txt", row.names=FALSE, na="", sep=";", quote=FALSE)


#endtime<-Sys.time()
#endtime-starttime


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
