# Rename columns to be R friendly

newname<-c("sample_name","date_and_time","Q_m3s","Al_mgL","Ca_mgL","Fe_mgL","K_mgL","Mg_mgL","Mn_mgL","Na_mgL","SO42_mgL","pH","EC")
stormevents<-read.csv(file="StormEvents.csv",header=TRUE)
colnames(stormevents)<-newname

# Pull out date and time

library(lubridate)
stormevents$newdate<-mdy_hm(stormevents$date_and_time)
stormevents$year<-year(stormevents$newdate)
stormevents$doy<-yday(stormevents$newdate)
stormevents$hour<-hour(stormevents$newdate)
stormevents$min<-minute(stormevents$newdate)
stormevents$time<-stormevents$hour + stormevents$min/60
