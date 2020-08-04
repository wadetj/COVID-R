
#Compared 7/29/2020 output, matched exactly for all intents and purposes
# only differences were my file has 0 for counts for days without reports
# and in number of decimal places
rm(list=ls())
#starttime<-Sys.time()
library(timsRstuff)
library(data.table)
library(dplyr)
library(gdata)

#com<-fread(file="C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/commute_results_v6.csv", sep=",",  na.strings=c("", "NA", "."))
#com<-fread(file="C:/Users/wadet/Documents/covid/commute_results_v6.csv", sep=",",  na.strings=c("", "NA", "."))
com<-fread(input="https://raw.githubusercontent.com/wadetj/COVID-R/master/data/commute_results_v6.csv", sep=",",  na.strings=c("", "NA", "."))



#communte file with just work facility and unique FIPS code
comuni<-com[, c("FIPS_IN", "Work_State_Name", "Work_County_Name", "Facility")]
comuni<-unique(comuni)


###EDIT THIS FILE
#xtemp<-fread(file="C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/Symptoms/ed_7_29_20.csv", sep=",", na.strings=c("", "NA", "."))
xtemp<-fread(file="C:/Users/wadet/Documents/covid/ed_7_29_20.csv", sep=",", na.strings=c("", "NA", "."))


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

xtemp<-rbindlist(list(xtemp, nazips), use.names=TRUE)


edtot<-xtemp[, .N, by=.(fips, date)]
#cli<-xtemp[covid_like_illness==1, .N, by=.(fips, date)]
#ili<-xtemp[influenza_like_illness==1, .N, by=.(fips, date)]
symps<-xtemp[, .("cli"=sum(covid_like_illness), "ili"=sum(influenza_like_illness)), by=.(fips, date)]
symps<-symps[!is.na(fips)]

symps<-symps[order(fips, date)]

#symps<-xtemp[, .("cli"=sum(covid_like_illness), by=.(fips, date))]


edtot<-edtot[order(fips, date)]
#cli<-cli[order(fips, date)]
#ili<-ili[order(fips, date)]

#setnames(cli, "N", "cli")
#setnames(ili, "N", "ili")
setnames(edtot, "N", "total")
edtot[, total:=ifelse(is.na(total), 0, total)]
edtot<-edtot[!is.na(fips)]

#cli[, cli:=ifelse(is.na(cli), 0, cli)]
#ili[, ili:=ifelse(is.na(ili), 0, ili)]

symps<-merge(symps, edtot, all=TRUE)

symps<-symps[!is.na(fips)]


#edit: fips from facilities that are not present in hospital data
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

#NEED TO ACCOUNT FOR MULTIPLE FACILITIES IN SAME FIPS_IN
#I THINK THIS WORKED WAS DOUBLE SUMMING

sympscom<-sympscom[, .("cli"=sum(cli), "ili"=sum(ili), "total"=sum(total), "FIPS_IN"=mean(FIPS_IN), "reported"=sum(reported)), by=.(Facility, date)]
sympscom[, clipct:=(cli/total)*100]
sympscom[, ilipct:=(ili/total)*100]
sympscom<-merge(sympscom, comuni, by="FIPS_IN", allow.cartesian=TRUE)
sympscom<-sympscom[, -c("Facility.y")]

sympscom<-setnames(sympscom, "Facility.x", "Facility")

sympscom<-sympscom[order(Facility, date)]

sympscom<-unique(sympscom)

#define minimal ili and cli

group19<-c("Connecticut|Maine|Massachusetts|New Hampshire|Rhode Island|Vermont|Delaware|District of Columbia|Maryland|Pennsylvania|Virginia|West Virginia|Illinois|Indiana|Michigan|Minnesota|Ohio|Wisconsin")
group24<-c("Alabama|Florida|Georgia|Kentucky|Mississippi|North Carolina|South Carolina|Tennessee|Arizona|California|Hawaii|Nevada")
group32<-c("New Jersey|New York|Puerto Rico|Virgin Islands")
group38<-c("Arkansas|Louisiana|New Mexico|Oklahoma|Texas")
group17<-c("Iowa|Kansas|Missouri|Nebraska")
group15<-c("Alaska|Idaho|Oregon|Washington")
group27<-c("Colorado|Montana|North Dakota|South Dakota|Utah|Wyoming")


#sel<-sympscom[grep(group19, sympscom$Work_State_Name) & sympscom$ilipct<=1.9, ]

#table(sympscom$Work_State_Name[xx])

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

sympscom$reported=ifelse(sympscom$reported>0, 1, 0)

#write.csv(sympscom, "C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/Symptoms/symps.csv", row.names=FALSE)
#Format for Jeremy
#need to check it seems total=0 is same as not reported


#read in prior file for autoregression

### EDIT THIS FILE - need to add quote="\""
#prevsymp<-read.table("C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/Symptoms/ILI_CLI_by_facility_7_22_20.txt", sep=";", stringsAsFactors=FALSE, na.strings=c("", "NA", "."), header=TRUE, quote="\"")
prevsymp<-read.table("C:/Users/wadet/Documents/covid/ILI_CLI_by_facility_7_22_20.txt", sep=";", stringsAsFactors=FALSE, na.strings=c("", "NA", "."), header=TRUE, quote="\"")



names(sympscom)

sympscomili<-dplyr::select(sympscom, c("Facility", "date", "ili", "ilipct", "minili", "total", "Work_County_Name", "Work_State_Name", "reported"))
names(sympscomili)<-c("Facility", "ed_date", "count", "percent", "minimal", "total_ed_visits", "Work_County_Name", "Work_State_Name", "reported")  
sympscomili$symptom="ILI"


sympscomcli<-dplyr::select(sympscom, c("Facility", "date", "cli", "clipct", "mincli", "total", "Work_County_Name", "Work_State_Name", "reported"))
names(sympscomcli)<-c("Facility", "ed_date", "count", "percent", "minimal", "total_ed_visits", "Work_County_Name", "Work_State_Name", "reported")  
sympscomcli$symptom="CLI"

allsymps<-rbind.data.frame(sympscomili, sympscomcli)

colorder<-c("Facility", "Work_County_Name", "Work_State_Name", "ed_date", "total_ed_visits", "count", "percent", "minimal", "reported", "symptom")


allsymps<-allsymps[, ..colorder]
#change to factors for merging with previous data and consistency in output
allsymps$reported<-factor(allsymps$reported, labels=c("NO", "YES"))
allsymps$minimal<-factor(allsymps$minimal, labels=c("NO", "YES"))

allsymps<-allsymps[order(Facility, ed_date, symptom)]
allsymps$index=1

prevsymp$reported<-factor(prevsymp$reported)
prevsymp$minimal<-factor(prevsymp$minimal)
prevsymp$ed_date<-as.Date(prevsymp$ed_date, "%d %b %Y")
prevsymp$index=0

#allsymps2<-merge(allsymps, prevsymp, by=c("Facility", "ed_date", "symptom"), all=TRUE)


allsymps2<-rbind.data.frame(allsymps, prevsymp)



temp<-cbind.data.frame(allsymps2$Facility, allsymps2$ed_date, allsymps2$symptom)
allsymps2$dupflag<-ifelse(duplicated(temp)|duplicated(temp, fromLast=TRUE), 1, 0)
drops<-allsymps2$dupflag==1 & allsymps2$index==0
allsymps2<-allsymps2[!drops, ]



allsymps2<-allsymps2[, -c("index", "dupflag")]

allsymps2<-allsymps2[order(Facility, ed_date, symptom)]

#EDIT THIS EVERY TIME keep dates within 1 month
allsymps2<-allsymps2[allsymps2$ed_date>=as.Date("2020-06-29"), ]


#format dates like SAS
allsymps2$ed_date<-toupper(format(allsymps2$ed_date, "%d%b%Y"))

#write.table(allsymps2, "C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/Symptoms/allsymps2729.txt", row.names=FALSE, na="", sep=";", quote=FALSE)
write.table(allsymps2, "C:/Users/wadet/Documents/covid/allsymps2729.txt", row.names=FALSE, na="", sep=";", quote=FALSE)

endtime<-Sys.time()
endtime-starttime


#Facility;Work_County_Name;Work_State_Name;ed_date;total_ed_visits;count;percent;minimal;reported;symptom


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
# table(allsymps3$ed_date)
# table(allsymps3$minimal)
# table(xx2$minimal)
# describe(xx2$percent)
# describe(allsymps3$percent)
# allsymps3[xx2$minimal!=allsymps3$minimal, ]
# levels(xx2$minimal)
# levels(allsymps3$minimal)
# allsymps3[as.character(xx2$minimal)!=as.character(allsymps3$minimal), ]
# xx2[as.character(xx2$minimal)!=as.character(allsymps3$minimal), ]
# xx2[as.character(xx2$minimal)=="YES" & as.character(allsymps3$minimal)=="NO", ]
# 


# xx4<-xx3[as.Date(xx3$ed_date,"%d %b %Y") <as.Date("2020-07-22"), ]
# table(xx4$reported)
# xx4[xx4$total==0, ]
# xx4[xx4$reported=="NO", ]





#zipstofips<-fread(file="C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/ZIP_COUNTY_032020.csv", sep=",",  na.strings=c("", "NA", ".", "#DIV/0!"))

#zipstofips<-read.xls("C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/ZIP_COUNTY_032020.xlsx",  sep=",",  na.strings=c("", "NA", ".", "#DIV/0!"), stringsAsFactors=FALSE)



#endtime<-Sys.time()

#endtime-starttime

#"C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/ZIP_COUNTY_032020.xlsx"
#"C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/ZIP_COUNTY_032020.xlsx"

# 

# 
# allcli2
# allcli2[1:100]
# allcli2<-allcli2[order(Facility, date)]
# allcli2[1:100]
# xtemp2$date<-as.Date(substr(xtemp2$c_visit_date_time, 1, 10))
# xtemp2[, table(date)]
# xtemp2[, table(covid_like_illness, date)]
# xtemp2[, table(covid_like_illness, date), by=hospital_zips]

#test DT
 # DT = data.table(
 #   ID = c("b","b","b","a","a","c"),
 #   a = 1:6,
 #   b = 7:12,
 #   c = 13:18,
 #   x=  c(rep("Name", 6)),
 #   ID2= c("b","b","Z","Q","Q","b")
 # )
 # 

# DT[, c("ID", "ID2"):="XXX"]
# 
# DT[, ID:=gsub("b", "d", ID)]
# 
# start.time<-Sys.time()
# xtemp2[, date2:=as.Date(substr(c_visit_date_time, 1, 10))]
# end.time<-Sys.time()
# end.time-start.time
# Time difference of 48.51358 secs
# start.time<-Sys.time()
# xtemp2$date<-as.Date(substr(xtemp2$c_visit_date_time, 1, 10))
# end.time<-Sys.time()
# end.time-start.time
# Time difference of 53.33757 secs       



#xtemp2[, hospital_county_fips:=gsub("\\[|\\]", "",  hospital_county_fips)]
# 
# zz[1:100]
# xtemp2[is.na(hospital_county_fips)]
# xtemp2[is.na(hospital_county_fips), .N]
# xtemp2[is.na(hospital_county_fips, table(hospital_names))]
# xtemp2[is.na(hospital_county_fips), table(hospital_names))]
# xtemp2[is.na(hospital_county_fips), table(hospital_names)]
# zz[1:100]
# tail(zz)
# xtemp2[is.na(hospital_county_fips) & state==TX, .N]
# xtemp2[is.na(hospital_county_fips) & state=="TX", .N]
# xtemp2[!is.na(hospital_county_fips) & state=="TX", .N]
# x<-c(1:9, 11:20)
# fill(x)
# library(dplyer)
# library(dplyr)
# fill(x)
# library(fill)
# library(dplyr)
# fill(x)
# DT = data.table(
#   ID = c("b","b","b","a","a","c"),
#   a = 1:6,
#   b = 7:12,
#   c = 13:18,
#   ID2= c("b","b","Z","Q","Q","b")
# )
# DT
# DT = data.table(
#   ID = c("b","b","b","a","a","c"),
#   a = c(1:4, 6, 7)
#   b = 7:12,
#   c = 13:18,
#   ID2= c("b","b","Z","Q","Q","b")
# )
# DT = data.table(
#   ID = c("b","b","b","a","a","c"),
#   a = c(1:5,7)
#   b = 7:12,
#   c = 13:18,
#   ID2= c("b","b","Z","Q","Q","b")
# )
# DT = data.table(
#   ID = c("b","b","b","a","a","c"),
#   a = c(1:5,7),
#   b = 7:12,
#   c = 13:18,
#   ID2= c("b","b","Z","Q","Q","b")
# )
# x = 1:10
# x[c(1:2, 5:6, 9:10)] = NA
# nafill(x, "locf")
# x
# dt = data.table(v1=x, v2=shift(x)/2, v3=shift(x, -1L)/2)
# nafill(dt, "nocb")
# dt
# dt = data.table(v1=x, v2=shift(x)/2, v3=shift(x, -1L)/2)
# dt
# x<-data.frame(a=c(1:5, 7:10), b=c(rep("a", 5), rep("b", 4)))
# x
# ?seq.Date
# min(xtemp2$date)
# max(xtemp2$date)
# xtemp2[, max(date)]
# xtemp2[, min(date)]
# ds<-seq.Date(min(xtemp2$date), max(xtemp2$date))
# ds<-seq.Date(min(xtemp2$date), max(xtemp2$date), "days")
# ds
# fips<-unique(xtemp2$hospital_county_fips)
# ds<-c(rep(ds, length(fips)))
# length(ds)
# fips<-unique(!is.na(xtemp2$hospital_county_fips))
# fips<-unique(xtemp2$hospital_county_fips)
# fips<-na.omit(fips)
# ds<-seq.Date(min(xtemp2$date), max(xtemp2$date), "days")
# ds<-c(rep(ds, length(fips)))
# ?expand
# ??expand
# ds<-seq.Date(min(xtemp2$date), max(xtemp2$date), "days")
# fips<-rep(fips, length(ds))
# length(fips)
# fips<-unique(xtemp2$hospital_county_fips)
# fips2<-rep(fips, length(ds))
# ds<-c(rep(ds, length(fips)))
# length(ds)
# ds<-seq.Date(min(xtemp2$date), max(xtemp2$date), "days")
# ds2<-c(rep(ds, length(fips)))
# fips2<-c(rep(fips, length(ds)))
# length(ds2)
# length(fips2)
# fips2
# ds2
# fips2<-order(fips2)
# fips2
# table(fips2)
# fips
# fips2<-c(rep(fips, length(ds)))
# fips2
# table(duplicated(fips2))
# x<-1:10
# rep(x, 3)
# sort(fips2)
# fips2<-c(rep(fips, length(ds)))
# fips2<-sort(fips2)
# cbind(ds2, fips2)
# length(ds2)
# length(fips2)
# ds2<-c(rep(ds, length(fips)))
# table(is.na(ds2))
# table(is.na(fips))
# fips<-unique(xtemp2$hospital_county_fips)
# fips<-na.omit(fips)
# ds2<-c(rep(ds, length(fips)))
# fips2<-c(rep(fips, length(ds)))
# length(ds2)
# length(fips2)
# cbind.data.frame(ds2, fips2)
# fips2<-sort(fips2)
# cbind.data.frame(ds2, fips2)
# fills<-cbind.data.frame(ds2, fips2)
# fills<-as.data.table(fills)
# head(zz)
# fills
# setnames(fills, "ds2"="date")
# setnames(fills, "ds2", "date")
# setnames(fills, "fips2", "hospital_county_fips")
# zzz<-merge(fills, zz, all=T)
# zz
# fills
# zzz<-merge(fills, zz, all=T)
# zzz<-merge(fills, zz, by=c(date, hospital_county_fips), all=T)
# names(zz)
# names(fills)
# zzz<-merge(zz, fills,  by=c(date, hospital_county_fips), all=T)
# zzz<-merge(zz, fills,  by=.(date, hospital_county_fips), all=T)
# zzz<-merge(zz, fills,  by=c("date", "hospital_county_fips)", all=T)
# )
# zz$hospital_county_fips
# fills$hospital_county_fips
# zzz<-merge(zz, fills,  by(date, hospital_county_fips), all=T)
# zzz<-merge(zz, fills,  by(date, hospital_county_fips), all=T))
# zzz<-merge(zz, fills,  by=date, all=T))
# zzz<-merge(zz, fills,  by=date, all=T)
# names(fills)
# names(zz)
# zzDT<-as.data.table(zz)
# zzz<-merge(zz, fills,  by=(date, hospital_county_fips), all=T)
# zzz<-merge(zzDT, fills,  by=(date, hospital_county_fips), all=T)
# zzz<-merge(zzDT, fills,  by=c(date, hospital_county_fips), all=T)

# 
# totcom<-zzfillcom[, sum(total), by=.(FIPS_IN, date)]
# totcli<-zzfillcom[, sum(cli), by=.(FIPS_IN, date)]
# totcli
# totcli[1:100]
# totcli<-setnames("V1", "cli")
# totcli<-setnames(totcli, "V1", "cli")
# totcom<-setnames(totcli, "V1", "total")
# totcom<-setnames(totcom, "V1", "total")
# totcom<-na.omit(totcom)
# totcom
# totcli<-na.omit(totcli)
# totcli
# allcli<-merge(totcli, totcom)
# allci
# allcli
# allcli<-allcli[, clipct:=(cli/total)*100]
# allcli
# allcli
# allcli2<-merge(allcli, com, by=FIPS_IN, all.x=T)
# allcli2<-merge(allcli, com, by="FIPS_IN", all.x=T)
# com
# allcli[1:10]
# comuni<-com[, c("FIPS_IN", "Work_State_Name", "Work_County_Name", "Facility")]
# comuni<-unique(comuni)
# comuni
# allcli2<-merge(allcli, comuni, by=FIPS_IN, all.x=T)
# allcli2<-merge(allcli, comuni, by="FIPS_IN", all.x=T)
# comuni
# duplicates(comuni$FIPS_IN)
# duplicate(comuni$FIPS_IN)
# duplicated(comuni$FIPS_IN)
# allci
# allcli
# table(comuni$FIPS_IN)
# comuni[FIPS_IN==6075]
=======
#Compared 7/29/2020 output, matched exactly for all intents and purposes
# only differences were my file has 0 for counts for days without reports
# and in number of decimal places
rm(list=ls())
starttime<-Sys.time()
library(timsRstuff)
library(data.table)
library(dplyr)
library(gdata)

#com<-fread(file="C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/commute_results_v6.csv", sep=",",  na.strings=c("", "NA", "."))

com<-fread(file="C:/Users/wadet/Documents/covid/commute_results_v6.csv", sep=",",  na.strings=c("", "NA", "."))



#communte file with just work facility and unique FIPS code
comuni<-com[, c("FIPS_IN", "Work_State_Name", "Work_County_Name", "Facility")]
comuni<-unique(comuni)


###EDIT THIS FILE
#xtemp<-fread(file="C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/Symptoms/ed_7_29_20.csv", sep=",", na.strings=c("", "NA", "."))
xtemp<-fread(file="C:/Users/wadet/Documents/covid/ed_7_29_20.csv", sep=",", na.strings=c("", "NA", "."))


xtemp<-xtemp[, -c(1:5, 8, 9, 10, 11, 13, 14, 17, 18, 19, 23, 24, 25, 26, 31)]
xtemp[, date:=as.Date(substr(c_visit_date_time, 1, 10))]
xtemp<-xtemp[, -c("c_visit_date_time", "onset_date", "hospital_fema_region")]

xtemp[, hospital_county_fips:=as.numeric(gsub("\\[|\\]", "",  hospital_county_fips))]
xtemp[, hospital_names:=gsub("\\[|\\]", "",  hospital_names)]
xtemp[, hospital_zips:=as.numeric(gsub("\\[|\\]", "",  hospital_zips))]
xtemp[, hospital_state_abbreviation:=gsub("\\[|\\]", "",  hospital_state_abbreviation)]

setnames(xtemp, "hospital_state_abbreviation", "state")
setnames(xtemp, "hospital_county_fips", "fips")


# zipstofips<-fread(file="C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/ZIP_COUNTY_032020.csv", sep=",",  na.strings=c("", "NA", ".", "#DIV/0!"))
# 
# #select county where zips match >80% to county
# zipshi<-zipstofips[zipstofips$TOT_RATIO>0.8, ]
# 
# nazips<-merge(zipshi, nas, by.x="ZIP", by.y="hospital_zips", all.y=T)
# nazips$fips<-ifelse(is.na(nazips$fips), nazips$COUNTY, nazips$fips) 
# nazips$hospital_zips<-nazips$ZIP
# 
# nazips<-nazips[, -c(1:6)]
# 

#xtemp<-rbind.data.table(xtemp, nazips)


edtot<-xtemp[, .N, by=.(fips, date)]
#cli<-xtemp[covid_like_illness==1, .N, by=.(fips, date)]
#ili<-xtemp[influenza_like_illness==1, .N, by=.(fips, date)]
symps<-xtemp[, .("cli"=sum(covid_like_illness), "ili"=sum(influenza_like_illness)), by=.(fips, date)]
symps<-symps[!is.na(fips)]

symps<-symps[order(fips, date)]

#symps<-xtemp[, .("cli"=sum(covid_like_illness), by=.(fips, date))]


edtot<-edtot[order(fips, date)]
#cli<-cli[order(fips, date)]
#ili<-ili[order(fips, date)]

#setnames(cli, "N", "cli")
#setnames(ili, "N", "ili")
setnames(edtot, "N", "total")
edtot[, total:=ifelse(is.na(total), 0, total)]
edtot<-edtot[!is.na(fips)]

#cli[, cli:=ifelse(is.na(cli), 0, cli)]
#ili[, ili:=ifelse(is.na(ili), 0, ili)]

symps<-merge(symps, edtot, all=TRUE)

symps<-symps[!is.na(fips)]


#edit: fips from facilities that are not present in hospital data
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

#NEED TO ACCOUNT FOR MULTIPLE FACILITIES IN SAME FIPS_IN
#I THINK THIS WORKED WAS DOUBLE SUMMING

sympscom<-sympscom[, .("cli"=sum(cli), "ili"=sum(ili), "total"=sum(total), "FIPS_IN"=mean(FIPS_IN), "reported"=sum(reported)), by=.(Facility, date)]
sympscom[, clipct:=(cli/total)*100]
sympscom[, ilipct:=(ili/total)*100]
sympscom<-merge(sympscom, comuni, by="FIPS_IN", allow.cartesian=TRUE)
sympscom<-sympscom[, -c("Facility.y")]

sympscom<-setnames(sympscom, "Facility.x", "Facility")

sympscom<-sympscom[order(Facility, date)]

sympscom<-unique(sympscom)

#define minimal ili and cli

group19<-c("Connecticut|Maine|Massachusetts|New Hampshire|Rhode Island|Vermont|Delaware|District of Columbia|Maryland|Pennsylvania|Virginia|West Virginia|Illinois|Indiana|Michigan|Minnesota|Ohio|Wisconsin")
group24<-c("Alabama|Florida|Georgia|Kentucky|Mississippi|North Carolina|South Carolina|Tennessee|Arizona|California|Hawaii|Nevada")
group32<-c("New Jersey|New York|Puerto Rico|Virgin Islands")
group38<-c("Arkansas|Louisiana|New Mexico|Oklahoma|Texas")
group17<-c("Iowa|Kansas|Missouri|Nebraska")
group15<-c("Alaska|Idaho|Oregon|Washington")
group27<-c("Colorado|Montana|North Dakota|South Dakota|Utah|Wyoming")


#sel<-sympscom[grep(group19, sympscom$Work_State_Name) & sympscom$ilipct<=1.9, ]

#table(sympscom$Work_State_Name[xx])

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

sympscom$reported=ifelse(sympscom$reported>0, 1, 0)

#write.csv(sympscom, "C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/Symptoms/symps.csv", row.names=FALSE)
#Format for Jeremy
#need to check it seems total=0 is same as not reported


#read in prior file for autoregression

### EDIT THIS FILE - need to add quote="\""
#prevsymp<-read.table("C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/Symptoms/ILI_CLI_by_facility_7_22_20.txt", sep=";", stringsAsFactors=FALSE, na.strings=c("", "NA", "."), header=TRUE, quote="\"")
prevsymp<-read.table("C:/Users/wadet/Documents/covid/ILI_CLI_by_facility_7_22_20.txt", sep=";", stringsAsFactors=FALSE, na.strings=c("", "NA", "."), header=TRUE, quote="\"")



names(sympscom)

sympscomili<-dplyr::select(sympscom, c("Facility", "date", "ili", "ilipct", "minili", "total", "Work_County_Name", "Work_State_Name", "reported"))
names(sympscomili)<-c("Facility", "ed_date", "count", "percent", "minimal", "total_ed_visits", "Work_County_Name", "Work_State_Name", "reported")  
sympscomili$symptom="ILI"


sympscomcli<-dplyr::select(sympscom, c("Facility", "date", "cli", "clipct", "mincli", "total", "Work_County_Name", "Work_State_Name", "reported"))
names(sympscomcli)<-c("Facility", "ed_date", "count", "percent", "minimal", "total_ed_visits", "Work_County_Name", "Work_State_Name", "reported")  
sympscomcli$symptom="CLI"

allsymps<-rbind.data.frame(sympscomili, sympscomcli)

colorder<-c("Facility", "Work_County_Name", "Work_State_Name", "ed_date", "total_ed_visits", "count", "percent", "minimal", "reported", "symptom")


allsymps<-allsymps[, ..colorder]
#change to factors for merging with previous data and consistency in output
allsymps$reported<-factor(allsymps$reported, labels=c("NO", "YES"))
allsymps$minimal<-factor(allsymps$minimal, labels=c("NO", "YES"))

allsymps<-allsymps[order(Facility, ed_date, symptom)]
allsymps$index=1

prevsymp$reported<-factor(prevsymp$reported)
prevsymp$minimal<-factor(prevsymp$minimal)
prevsymp$ed_date<-as.Date(prevsymp$ed_date, "%d %b %Y")
prevsymp$index=0

#allsymps2<-merge(allsymps, prevsymp, by=c("Facility", "ed_date", "symptom"), all=TRUE)


allsymps2<-rbind.data.frame(allsymps, prevsymp)



temp<-cbind.data.frame(allsymps2$Facility, allsymps2$ed_date, allsymps2$symptom)
allsymps2$dupflag<-ifelse(duplicated(temp)|duplicated(temp, fromLast=TRUE), 1, 0)
drops<-allsymps2$dupflag==1 & allsymps2$index==0
allsymps2<-allsymps2[!drops, ]



allsymps2<-allsymps2[, -c("index", "dupflag")]

allsymps2<-allsymps2[order(Facility, ed_date, symptom)]

#EDIT THIS EVERY TIME keep dates within 1 month
allsymps2<-allsymps2[allsymps2$ed_date>=as.Date("2020-06-29"), ]


#format dates like SAS
allsymps2$ed_date<-toupper(format(allsymps2$ed_date, "%d%b%Y"))

#write.table(allsymps2, "C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/Symptoms/allsymps2729.txt", row.names=FALSE, na="", sep=";", quote=FALSE)
write.table(allsymps2, "C:/Users/wadet/Documents/covid/allsymps2729.txt", row.names=FALSE, na="", sep=";", quote=FALSE)

endtime<-Sys.time()
endtime-starttime


#Facility;Work_County_Name;Work_State_Name;ed_date;total_ed_visits;count;percent;minimal;reported;symptom


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
# table(allsymps3$ed_date)
# table(allsymps3$minimal)
# table(xx2$minimal)
# describe(xx2$percent)
# describe(allsymps3$percent)
# allsymps3[xx2$minimal!=allsymps3$minimal, ]
# levels(xx2$minimal)
# levels(allsymps3$minimal)
# allsymps3[as.character(xx2$minimal)!=as.character(allsymps3$minimal), ]
# xx2[as.character(xx2$minimal)!=as.character(allsymps3$minimal), ]
# xx2[as.character(xx2$minimal)=="YES" & as.character(allsymps3$minimal)=="NO", ]
# 


# xx4<-xx3[as.Date(xx3$ed_date,"%d %b %Y") <as.Date("2020-07-22"), ]
# table(xx4$reported)
# xx4[xx4$total==0, ]
# xx4[xx4$reported=="NO", ]





#zipstofips<-fread(file="C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/ZIP_COUNTY_032020.csv", sep=",",  na.strings=c("", "NA", ".", "#DIV/0!"))

#zipstofips<-read.xls("C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/ZIP_COUNTY_032020.xlsx",  sep=",",  na.strings=c("", "NA", ".", "#DIV/0!"), stringsAsFactors=FALSE)



#endtime<-Sys.time()

#endtime-starttime

#"C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/ZIP_COUNTY_032020.xlsx"
#"C:/Users/twade/OneDrive - Environmental Protection Agency (EPA)/Coronavirus/data/ZIP_COUNTY_032020.xlsx"

# 

# 
# allcli2
# allcli2[1:100]
# allcli2<-allcli2[order(Facility, date)]
# allcli2[1:100]
# xtemp2$date<-as.Date(substr(xtemp2$c_visit_date_time, 1, 10))
# xtemp2[, table(date)]
# xtemp2[, table(covid_like_illness, date)]
# xtemp2[, table(covid_like_illness, date), by=hospital_zips]

#test DT
 # DT = data.table(
 #   ID = c("b","b","b","a","a","c"),
 #   a = 1:6,
 #   b = 7:12,
 #   c = 13:18,
 #   x=  c(rep("Name", 6)),
 #   ID2= c("b","b","Z","Q","Q","b")
 # )
 # 

# DT[, c("ID", "ID2"):="XXX"]
# 
# DT[, ID:=gsub("b", "d", ID)]
# 
# start.time<-Sys.time()
# xtemp2[, date2:=as.Date(substr(c_visit_date_time, 1, 10))]
# end.time<-Sys.time()
# end.time-start.time
# Time difference of 48.51358 secs
# start.time<-Sys.time()
# xtemp2$date<-as.Date(substr(xtemp2$c_visit_date_time, 1, 10))
# end.time<-Sys.time()
# end.time-start.time
# Time difference of 53.33757 secs       



#xtemp2[, hospital_county_fips:=gsub("\\[|\\]", "",  hospital_county_fips)]
# 
# zz[1:100]
# xtemp2[is.na(hospital_county_fips)]
# xtemp2[is.na(hospital_county_fips), .N]
# xtemp2[is.na(hospital_county_fips, table(hospital_names))]
# xtemp2[is.na(hospital_county_fips), table(hospital_names))]
# xtemp2[is.na(hospital_county_fips), table(hospital_names)]
# zz[1:100]
# tail(zz)
# xtemp2[is.na(hospital_county_fips) & state==TX, .N]
# xtemp2[is.na(hospital_county_fips) & state=="TX", .N]
# xtemp2[!is.na(hospital_county_fips) & state=="TX", .N]
# x<-c(1:9, 11:20)
# fill(x)
# library(dplyer)
# library(dplyr)
# fill(x)
# library(fill)
# library(dplyr)
# fill(x)
# DT = data.table(
#   ID = c("b","b","b","a","a","c"),
#   a = 1:6,
#   b = 7:12,
#   c = 13:18,
#   ID2= c("b","b","Z","Q","Q","b")
# )
# DT
# DT = data.table(
#   ID = c("b","b","b","a","a","c"),
#   a = c(1:4, 6, 7)
#   b = 7:12,
#   c = 13:18,
#   ID2= c("b","b","Z","Q","Q","b")
# )
# DT = data.table(
#   ID = c("b","b","b","a","a","c"),
#   a = c(1:5,7)
#   b = 7:12,
#   c = 13:18,
#   ID2= c("b","b","Z","Q","Q","b")
# )
# DT = data.table(
#   ID = c("b","b","b","a","a","c"),
#   a = c(1:5,7),
#   b = 7:12,
#   c = 13:18,
#   ID2= c("b","b","Z","Q","Q","b")
# )
# x = 1:10
# x[c(1:2, 5:6, 9:10)] = NA
# nafill(x, "locf")
# x
# dt = data.table(v1=x, v2=shift(x)/2, v3=shift(x, -1L)/2)
# nafill(dt, "nocb")
# dt
# dt = data.table(v1=x, v2=shift(x)/2, v3=shift(x, -1L)/2)
# dt
# x<-data.frame(a=c(1:5, 7:10), b=c(rep("a", 5), rep("b", 4)))
# x
# ?seq.Date
# min(xtemp2$date)
# max(xtemp2$date)
# xtemp2[, max(date)]
# xtemp2[, min(date)]
# ds<-seq.Date(min(xtemp2$date), max(xtemp2$date))
# ds<-seq.Date(min(xtemp2$date), max(xtemp2$date), "days")
# ds
# fips<-unique(xtemp2$hospital_county_fips)
# ds<-c(rep(ds, length(fips)))
# length(ds)
# fips<-unique(!is.na(xtemp2$hospital_county_fips))
# fips<-unique(xtemp2$hospital_county_fips)
# fips<-na.omit(fips)
# ds<-seq.Date(min(xtemp2$date), max(xtemp2$date), "days")
# ds<-c(rep(ds, length(fips)))
# ?expand
# ??expand
# ds<-seq.Date(min(xtemp2$date), max(xtemp2$date), "days")
# fips<-rep(fips, length(ds))
# length(fips)
# fips<-unique(xtemp2$hospital_county_fips)
# fips2<-rep(fips, length(ds))
# ds<-c(rep(ds, length(fips)))
# length(ds)
# ds<-seq.Date(min(xtemp2$date), max(xtemp2$date), "days")
# ds2<-c(rep(ds, length(fips)))
# fips2<-c(rep(fips, length(ds)))
# length(ds2)
# length(fips2)
# fips2
# ds2
# fips2<-order(fips2)
# fips2
# table(fips2)
# fips
# fips2<-c(rep(fips, length(ds)))
# fips2
# table(duplicated(fips2))
# x<-1:10
# rep(x, 3)
# sort(fips2)
# fips2<-c(rep(fips, length(ds)))
# fips2<-sort(fips2)
# cbind(ds2, fips2)
# length(ds2)
# length(fips2)
# ds2<-c(rep(ds, length(fips)))
# table(is.na(ds2))
# table(is.na(fips))
# fips<-unique(xtemp2$hospital_county_fips)
# fips<-na.omit(fips)
# ds2<-c(rep(ds, length(fips)))
# fips2<-c(rep(fips, length(ds)))
# length(ds2)
# length(fips2)
# cbind.data.frame(ds2, fips2)
# fips2<-sort(fips2)
# cbind.data.frame(ds2, fips2)
# fills<-cbind.data.frame(ds2, fips2)
# fills<-as.data.table(fills)
# head(zz)
# fills
# setnames(fills, "ds2"="date")
# setnames(fills, "ds2", "date")
# setnames(fills, "fips2", "hospital_county_fips")
# zzz<-merge(fills, zz, all=T)
# zz
# fills
# zzz<-merge(fills, zz, all=T)
# zzz<-merge(fills, zz, by=c(date, hospital_county_fips), all=T)
# names(zz)
# names(fills)
# zzz<-merge(zz, fills,  by=c(date, hospital_county_fips), all=T)
# zzz<-merge(zz, fills,  by=.(date, hospital_county_fips), all=T)
# zzz<-merge(zz, fills,  by=c("date", "hospital_county_fips)", all=T)
# )
# zz$hospital_county_fips
# fills$hospital_county_fips
# zzz<-merge(zz, fills,  by(date, hospital_county_fips), all=T)
# zzz<-merge(zz, fills,  by(date, hospital_county_fips), all=T))
# zzz<-merge(zz, fills,  by=date, all=T))
# zzz<-merge(zz, fills,  by=date, all=T)
# names(fills)
# names(zz)
# zzDT<-as.data.table(zz)
# zzz<-merge(zz, fills,  by=(date, hospital_county_fips), all=T)
# zzz<-merge(zzDT, fills,  by=(date, hospital_county_fips), all=T)
# zzz<-merge(zzDT, fills,  by=c(date, hospital_county_fips), all=T)

# 
# totcom<-zzfillcom[, sum(total), by=.(FIPS_IN, date)]
# totcli<-zzfillcom[, sum(cli), by=.(FIPS_IN, date)]
# totcli
# totcli[1:100]
# totcli<-setnames("V1", "cli")
# totcli<-setnames(totcli, "V1", "cli")
# totcom<-setnames(totcli, "V1", "total")
# totcom<-setnames(totcom, "V1", "total")
# totcom<-na.omit(totcom)
# totcom
# totcli<-na.omit(totcli)
# totcli
# allcli<-merge(totcli, totcom)
# allci
# allcli
# allcli<-allcli[, clipct:=(cli/total)*100]
# allcli
# allcli
# allcli2<-merge(allcli, com, by=FIPS_IN, all.x=T)
# allcli2<-merge(allcli, com, by="FIPS_IN", all.x=T)
# com
# allcli[1:10]
# comuni<-com[, c("FIPS_IN", "Work_State_Name", "Work_County_Name", "Facility")]
# comuni<-unique(comuni)
# comuni
# allcli2<-merge(allcli, comuni, by=FIPS_IN, all.x=T)
# allcli2<-merge(allcli, comuni, by="FIPS_IN", all.x=T)
# comuni
# duplicates(comuni$FIPS_IN)
# duplicate(comuni$FIPS_IN)
# duplicated(comuni$FIPS_IN)
# allci
# allcli
# table(comuni$FIPS_IN)
# comuni[FIPS_IN==6075]
>>>>>>> f60b5fb5b40120ae1c77b9d6f2ed26517c3330f7
# allcli2<-merge(allcli, comuni, by="FIPS_IN")