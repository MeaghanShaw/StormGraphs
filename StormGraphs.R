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

# Transform mg/L to mmol/L

stormevents$Al_mmolL<-(stormevents$Al_mgL/26.982)
stormevents$Ca_mmolL<-(stormevents$Ca_mgL/40.08)
stormevents$Fe_mmolL<-(stormevents$Fe_mgL/55.845)
stormevents$K_mmolL<-(stormevents$K_mgL/39.099)
stormevents$Mg_mmolL<-(stormevents$Mg_mgL/24.305)
stormevents$Mn_mmolL<-(stormevents$Mn_mgL/54.938)
stormevents$Na_mmolL<-(stormevents$Na_mgL/22.99)
stormevents$SO42_mmolL<-(stormevents$SO42_mgL/96.06)

# Scatter plot matrix for Al to SO42
pairs(stormevents[20:27])

# Plot July 7 event for Fe and Mn
Jul717storm<-stormevents[which(stormevents$doy == "188"),]
library(ggplot2)
yscale<-10

two_ys<-ggplot(Jul717storm,aes(x=newdate))+
  geom_line(aes(y=Q_m3s),colour="blue",size=1.2)+
  geom_point(aes(y=Fe_mmolL/yscale,fill="orange"),colour="black",pch=21,size=2)+
  geom_point(aes(y=Mn_mmolL/yscale,fill="green"),pch=21,colour="black",size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="Concentration"))+
  scale_fill_manual(guide="legend",name="ions",
                    values=c("orange","green"),labels=c("Fe","Mn"))+
  labs(y="Discharge",
       x="Date")+
  theme_bw()

two_ys

#Pull out processed data sheet
write.csv(Jul717storm,"July717storm.csv")
