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
yscale<-21

Jul_7_stormFe<-ggplot(Jul717storm,aes(x=newdate))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_line(aes(y=Q_m3s),colour="blue",size=1.2)+
  geom_point(aes(y=Fe_mmolL/yscale,fill="orange"),colour="black",pch=21,size=2)+
  geom_point(aes(y=Mn_mmolL/yscale,fill="green"),pch=21,colour="black",size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="Concentration (mmol "~L^-1~")"))+
  scale_fill_manual(guide="legend",name=NULL,
                    values=c("orange","green"),labels=c("Fe","Mn"))+
  labs(y="Q ("~m^3~s^-1~")",
       x="Date")+
  theme_bw()

Jul_7_stormFe

#Pull out processed data sheet
write.csv(Jul717storm,"July717storm.csv")

# Plot July 13 event for Fe and Mn
Jul1317storm<-stormevents[which(stormevents$doy == "194"),]
library(ggplot2)
yscale6<-15

Jul_13_stormFe<-ggplot(Jul1317storm,aes(x=newdate))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_line(aes(y=Q_m3s),colour="blue",size=1.2)+
  geom_point(aes(y=Fe_mmolL/yscale6,fill="orange"),colour="black",pch=21,size=2)+
  geom_point(aes(y=Mn_mmolL/yscale6,fill="green"),pch=21,colour="black",size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="Concentration (mmol "~L^-1~")"))+
  scale_fill_manual(guide="legend",name=NULL,
                    values=c("orange","green"),labels=c("Fe","Mn"))+
  labs(y="Q ("~m^3~s^-1~")",
       x="Date")+
  theme_bw()

Jul_13_stormFe

#Pull out processed data sheet
write.csv(Jul1317storm,"July1317storm.csv")

# Plot October 8-9 event for Fe and Mn
Oct8917storm<-stormevents[which(stormevents$doy == "281"
                         | stormevents$doy == "282"),]
library(ggplot2)
yscale3<-30

Oct_8_9_stormFe<-ggplot(Oct8917storm,aes(x=newdate))+
  geom_line(aes(y=Q_m3s),colour="blue",size=1.2)+
  geom_point(aes(y=Fe_mmolL/yscale3,fill="orange"),colour="black",pch=21,size=2)+
  geom_point(aes(y=Mn_mmolL/yscale3,fill="green"),pch=21,colour="black",size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="Concentration (mmol "~L^-1~")"))+
  scale_fill_manual(guide="legend",name=NULL,
                    values=c("orange","green"),labels=c("Fe","Mn"))+
  labs(y="Q ("~m^3~s^-1~")",
       x="Date")+
  theme_bw()

Oct_8_9_stormFe

#Pull out processed data sheet
write.csv(Oct8917storm,"October8917storm.csv")

# Plot October 11 event for Fe and Mn
Oct1117storm<-stormevents[which(stormevents$doy == "284"),]
library(ggplot2)
yscale7<-25

Oct_11_stormFe<-ggplot(Oct1117storm,aes(x=newdate))+
  geom_line(aes(y=Q_m3s),colour="blue",size=1.2)+
  geom_point(aes(y=Fe_mmolL/yscale7,fill="orange"),colour="black",pch=21,size=2)+
  geom_point(aes(y=Mn_mmolL/yscale7,fill="green"),pch=21,colour="black",size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="Concentration (mmol "~L^-1~")"))+
  scale_fill_manual(guide="legend",name=NULL,
                    values=c("orange","green"),labels=c("Fe","Mn"))+
  labs(y="Q ("~m^3~s^-1~")",
       x="Date")+
  theme_bw()

Oct_11_stormFe

#Pull out processed data sheet
write.csv(Oct1117storm,"October1117storm.csv")

# Plot November 15-16 event for Fe and Mn
Nov151617storm<-stormevents[which(stormevents$doy == "319"
                                | stormevents$doy == "320"),]
library(ggplot2)
yscale8<-30

Nov_15_16_stormFe<-ggplot(Nov151617storm,aes(x=newdate))+
  geom_line(aes(y=Q_m3s),colour="blue",size=1.2)+
  geom_point(aes(y=Fe_mmolL/yscale8,fill="orange"),colour="black",pch=21,size=2)+
  geom_point(aes(y=Mn_mmolL/yscale8,fill="green"),pch=21,colour="black",size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="Concentration (mmol "~L^-1~")"))+
  scale_fill_manual(guide="legend",name=NULL,
                    values=c("orange","green"),labels=c("Fe","Mn"))+
  labs(y="Q ("~m^3~s^-1~")",
       x="Date")+
  theme_bw()

Nov_15_16_stormFe

#Pull out processed data sheet
write.csv(Nov151617storm,"November151617storm.csv")

# Plot July 7 event

library(ggplot2)
yscale1<-400

Jul_7_stormCa<-ggplot(Jul717storm,aes(x=newdate))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_line(aes(y=Q_m3s),colour="blue",size=1.2)+
  geom_point(aes(y=Ca_mmolL/yscale1,fill="yellow"),colour="black",pch=21,size=2)+
  geom_point(aes(y=Mg_mmolL/yscale1,fill="purple"),pch=21,colour="black",size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="Concentration (mmol "~L^-1~")"))+
  scale_fill_manual(guide="legend",name=NULL,
                    values=c("yellow","purple"),labels=c("Ca","Mg"))+
  labs(y="Q ("~m^3~s^-1~")",
       x="Date")+
  theme_bw()

Jul_7_stormCa

# Plot July 13 event 

library(ggplot2)
yscale9<-260

Jul_13_stormCa<-ggplot(Jul1317storm,aes(x=newdate))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_line(aes(y=Q_m3s),colour="blue",size=1.2)+
  geom_point(aes(y=Ca_mmolL/yscale9,fill="yellow"),colour="black",pch=21,size=2)+
  geom_point(aes(y=Mg_mmolL/yscale9,fill="purple"),pch=21,colour="black",size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="Concentration (mmol "~L^-1~")"))+
  scale_fill_manual(guide="legend",name=NULL,
                    values=c("yellow","purple"),labels=c("Ca","Mg"))+
  labs(y="Q ("~m^3~s^-1~")",
       x="Date")+
  theme_bw()

Jul_13_stormCa

#October 89 event
library(ggplot2)
yscale4<-350

Oct_8_9_stormCa<-ggplot(Oct8917storm,aes(x=newdate))+
  geom_line(aes(y=Q_m3s),colour="blue",size=1.2)+
  geom_point(aes(y=Ca_mmolL/yscale4,fill="yellow"),colour="black",pch=21,size=2)+
  geom_point(aes(y=Mg_mmolL/yscale4,fill="purple"),pch=21,colour="black",size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="Concentration (mmol "~L^-1~")"))+
  scale_fill_manual(guide="legend",name=NULL,
                    values=c("yellow","purple"),labels=c("Ca","Mg"))+
  labs(y="Q ("~m^3~s^-1~")",
       x="Date")+
  theme_bw()

Oct_8_9_stormCa

# Plot October 11 event 
library(ggplot2)
yscale10<-340

Oct_11_stormCa<-ggplot(Oct1117storm,aes(x=newdate))+
  geom_line(aes(y=Q_m3s),colour="blue",size=1.2)+
  geom_point(aes(y=Ca_mmolL/yscale10,fill="yellow"),colour="black",pch=21,size=2)+
  geom_point(aes(y=Mg_mmolL/yscale10,fill="purple"),pch=21,colour="black",size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="Concentration (mmol "~L^-1~")"))+
  scale_fill_manual(guide="legend",name=NULL,
                    values=c("yellow","purple"),labels=c("Ca","Mg"))+
  labs(y="Q ("~m^3~s^-1~")",
       x="Date")+
  theme_bw()

Oct_11_stormCa

#November 1516 event
library(ggplot2)
yscale11<-320

Nov_15_16_stormCa<-ggplot(Nov151617storm,aes(x=newdate))+
  geom_line(aes(y=Q_m3s),colour="blue",size=1.2)+
  geom_point(aes(y=Ca_mmolL/yscale11,fill="yellow"),colour="black",pch=21,size=2)+
  geom_point(aes(y=Mg_mmolL/yscale11,fill="purple"),pch=21,colour="black",size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="Concentration (mmol "~L^-1~")"))+
  scale_fill_manual(guide="legend",name=NULL,
                    values=c("yellow","purple"),labels=c("Ca","Mg"))+
  labs(y="Q ("~m^3~s^-1~")",
       x="Date")+
  theme_bw()

Nov_15_16_stormCa

# Plot July 7 event

library(ggplot2)
yscale2<-30

Jul_7_stormK<-ggplot(Jul717storm,aes(x=newdate))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_line(aes(y=Q_m3s),colour="blue",size=1.2)+
  geom_point(aes(y=K_mmolL/yscale2,fill="red"),colour="black",pch=21,size=2)+
  geom_point(aes(y=Na_mmolL/yscale2,fill="grey"),pch=21,colour="black",size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="Concentration (mmol "~L^-1~")"))+
  scale_fill_manual(guide="legend",name=NULL,
                    values=c("red","grey"),labels=c("K","Na"))+
  labs(y="Q ("~m^3~s^-1~")",
       x="Date")+
  theme_bw()

Jul_7_stormK

# Plot July 13 event 

library(ggplot2)
yscale12<-25

Jul_13_stormK<-ggplot(Jul1317storm,aes(x=newdate))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_line(aes(y=Q_m3s),colour="blue",size=1.2)+
  geom_point(aes(y=K_mmolL/yscale12,fill="red"),colour="black",pch=21,size=2)+
  geom_point(aes(y=Na_mmolL/yscale12,fill="grey"),pch=21,colour="black",size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="Concentration (mmol "~L^-1~")"))+
  scale_fill_manual(guide="legend",name=NULL,
                    values=c("red","grey"),labels=c("K","Na"))+
  labs(y="Q ("~m^3~s^-1~")",
       x="Date")+
  theme_bw()

Jul_13_stormK

#October 89 event
library(ggplot2)
yscale5<-25

Oct_8_9_stormK<-ggplot(Oct8917storm,aes(x=newdate))+
  geom_line(aes(y=Q_m3s),colour="blue",size=1.2)+
  geom_point(aes(y=K_mmolL/yscale5,fill="red"),colour="black",pch=21,size=2)+
  geom_point(aes(y=Na_mmolL/yscale5,fill="grey"),pch=21,colour="black",size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="Concentration (mmol "~L^-1~")"))+
  scale_fill_manual(guide="legend",name=NULL,
                    values=c("red","grey"),labels=c("K","Na"))+
  labs(y="Q ("~m^3~s^-1~")",
       x="Date")+
  theme_bw()

Oct_8_9_stormK

# Plot October 11 event 
library(ggplot2)
yscale13<-60

Oct_11_stormK<-ggplot(Oct1117storm,aes(x=newdate))+
  geom_line(aes(y=Q_m3s),colour="blue",size=1.2)+
  geom_point(aes(y=K_mmolL/yscale13,fill="red"),colour="black",pch=21,size=2)+
  geom_point(aes(y=Na_mmolL/yscale13,fill="grey"),pch=21,colour="black",size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="Concentration (mmol "~L^-1~")"))+
  scale_fill_manual(guide="legend",name=NULL,
                    values=c("red","grey"),labels=c("K","Na"))+
  labs(y="Q ("~m^3~s^-1~")",
       x="Date")+
  theme_bw()

Oct_11_stormK

#November 1516 event
library(ggplot2)
yscale14<-25

Nov_15_16_stormK<-ggplot(Nov151617storm,aes(x=newdate))+
  geom_line(aes(y=Q_m3s),colour="blue",size=1.2)+
  geom_point(aes(y=K_mmolL/yscale14,fill="red"),colour="black",pch=21,size=2)+
  geom_point(aes(y=Na_mmolL/yscale14,fill="grey"),pch=21,colour="black",size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="Concentration (mmol "~L^-1~")"))+
  scale_fill_manual(guide="legend",name=NULL,
                    values=c("red","grey"),labels=c("K","Na"))+
  labs(y="Q ("~m^3~s^-1~")",
       x="Date")+
  theme_bw()

Nov_15_16_stormK

# Plot July 7 event for pH
Jul717storm<-stormevents[which(stormevents$doy == "188"),]
library(ggplot2)
yscale15<-200

Jul_7_stormpH<-ggplot(Jul717storm,aes(x=newdate))+
  geom_line(aes(y=Q_m3s),colour="blue",size=1.2)+
  geom_point(aes(y=pH/yscale15,fill="purple"),colour="black",pch=21,size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="pH"))+
  scale_fill_manual(guide="legend",name=NULL,
                    values=c("purple"),labels=c("pH"))+
  labs(y="Q ("~m^3~s^-1~")",
       x="Date")+
  theme_bw()

Jul_7_stormpH

#Pull out processed data sheet
write.csv(Jul717storm,"July717storm.csv")

# Plot July 13 event for pH
Jul1317storm<-stormevents[which(stormevents$doy == "194"),]
library(ggplot2)
yscale17<-310

Jul_13_stormpH<-ggplot(Jul1317storm,aes(x=newdate))+
  geom_line(aes(y=Q_m3s),colour="blue",size=1.2)+
  geom_point(aes(y=pH/yscale17,fill="purple"),colour="black",pch=21,size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="pH"))+
  scale_fill_manual(guide="legend",name=NULL,
                    values=c("purple"),labels=c("pH"))+
  labs(y="Q ("~m^3~s^-1~")",
       x="Date")+
  theme_bw()

Jul_13_stormpH

#Pull out processed data sheet
write.csv(Jul1317storm,"July1317storm.csv")

# Plot October 8-9 event for pH
Oct8917storm<-stormevents[which(stormevents$doy == "281"
                                | stormevents$doy == "282"),]
library(ggplot2)
yscale16<-200

Oct_8_9_stormpH<-ggplot(Oct8917storm,aes(x=newdate))+
  geom_line(aes(y=Q_m3s),colour="blue",size=1.2)+
  geom_point(aes(y=pH/yscale16,fill="purple"),colour="black",pch=21,size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="pH"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(guide="legend",name=NULL,
                    values=c("purple"),labels=c("pH"))+
  labs(y="Q ("~m^3~s^-1~")",
       x="Date")+
  theme_bw()

Oct_8_9_stormpH

#Pull out processed data sheet
write.csv(Oct8917storm,"October8917storm.csv")

# Plot October 11 event for pH
Oct1117storm<-stormevents[which(stormevents$doy == "284"),]
library(ggplot2)
yscale18<-250

Oct_11_stormpH<-ggplot(Oct1117storm,aes(x=newdate))+
  geom_line(aes(y=Q_m3s),colour="blue",size=1.2)+
  geom_point(aes(y=pH/yscale18,fill="purple"),colour="black",pch=21,size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="pH"))+
  scale_fill_manual(guide="legend",name=NULL,
                    values=c("purple"),labels=c("pH"))+
  labs(y="Q ("~m^3~s^-1~")",
       x="Date")+
  theme_bw()

Oct_11_stormpH

#Pull out processed data sheet
write.csv(Oct1117storm,"October1117storm.csv")

# Plot November 15-16 event for pH
Nov151617storm<-stormevents[which(stormevents$doy == "319"
                                  | stormevents$doy == "320"),]
library(ggplot2)
yscale19<-260

Nov_15_16_stormpH<-ggplot(Nov151617storm,aes(x=newdate))+
  geom_line(aes(y=Q_m3s),colour="blue",size=1.2)+
  geom_point(aes(y=pH/yscale19,fill="purple"),colour="black",pch=21,size=2)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="pH"))+
  scale_fill_manual(guide="legend",name=NULL,
                    values=c("purple"),labels=c("pH"))+
  labs(y="Q ("~m^3~s^-1~")",
       x="Date")+
  theme_bw()

Nov_15_16_stormpH

#Put all storm plots in one figure
library(gridExtra)
library(lubridate)
Jul_7_storm.1<-Jul_7_stormFe+guides(fill=FALSE,shape=FALSE)
Oct_8_9_storm.1<-Oct_8_9_stormFe
Jul_7_storm.2<-Jul_7_stormCa+guides(fill=FALSE,shape=FALSE)
Oct_8_9_storm.2<-Oct_8_9_stormCa
Jul_7_storm.3<-Jul_7_stormK+guides(fill=FALSE,shape=FALSE)
Oct_8_9_storm.3<-Oct_8_9_stormK
grid.arrange(arrangeGrob(Jul_7_storm.1,Oct_8_9_storm.1,Jul_7_storm.2,Oct_8_9_storm.2,Jul_7_storm.3,Oct_8_9_storm.3, ncol=2, widths=c(1,1)))

#Save to PDF
pdf("Stormevents.pdf",height=10,width=10)
grid.arrange(arrangeGrob(Jul_7_storm.1,Oct_8_9_storm.1,Jul_7_storm.2,Oct_8_9_storm.2,Jul_7_storm.3,Oct_8_9_storm.3, ncol=2, widths=c(1,1)))
dev.off()

#Put all storm pH plots in one figure
library(gridExtra)
Jul_7_storm.1<-Jul_7_stormpH+guides(fill=FALSE,shape=FALSE)
Oct_8_9_storm.1<-Oct_8_9_stormpH
grid.arrange(arrangeGrob(Jul_7_storm.1,Oct_8_9_storm.1, ncol=2, widths=c(1,1)))

#Save to PDF
pdf("StormeventspH.pdf",height=4,width=10)
grid.arrange(arrangeGrob(Jul_7_storm.1,Oct_8_9_storm.1, ncol=2, widths=c(1,1)))
dev.off()

#Put all storm tail plots in one figure
library(gridExtra)
Jul_13_storm.1<-Jul_13_stormFe+guides(fill=FALSE,shape=FALSE)
Oct_11_storm.1<-Oct_11_stormFe+guides(fill=FALSE,shape=FALSE)
Nov_15_16_storm.1<-Nov_15_16_stormFe
Jul_13_storm.2<-Jul_13_stormCa+guides(fill=FALSE,shape=FALSE)
Oct_11_storm.2<-Oct_11_stormCa+guides(fill=FALSE,shape=FALSE)
Nov_15_16_storm.2<-Nov_15_16_stormCa
Jul_13_storm.3<-Jul_13_stormK+guides(fill=FALSE,shape=FALSE)
Oct_11_storm.3<-Oct_11_stormK+guides(fill=FALSE,shape=FALSE)
Nov_15_16_storm.3<-Nov_15_16_stormK
grid.arrange(arrangeGrob(Jul_13_storm.1,Oct_11_storm.1,Nov_15_16_storm.1,Jul_13_storm.2,Oct_11_storm.2,Nov_15_16_storm.2,Jul_13_storm.3,Oct_11_storm.3,Nov_15_16_storm.3, ncol=3, widths=c(1,1,1)))

#Save to PDF
pdf("Stormtails.pdf",height=10,width=10)
grid.arrange(arrangeGrob(Jul_13_storm.1,Oct_11_storm.1,Nov_15_16_storm.1,Jul_13_storm.2,Oct_11_storm.2,Nov_15_16_storm.2,Jul_13_storm.3,Oct_11_storm.3,Nov_15_16_storm.3, ncol=3, widths=c(1,1,1)))
dev.off()

#Put all storm tail pH plots in one figure
library(gridExtra)
Jul_13_storm.1<-Jul_13_stormpH+guides(fill=FALSE,shape=FALSE)
Oct_11_storm.1<-Oct_11_stormpH+guides(fill=FALSE,shape=FALSE)
Nov_15_16_storm.1<-Nov_15_16_stormpH
grid.arrange(arrangeGrob(Jul_13_storm.1,Oct_11_storm.1,Nov_15_16_storm.1, ncol=3, widths=c(1,1,1)))

#Save to PDF
pdf("StormtailspH.pdf",height=4,width=10)
grid.arrange(arrangeGrob(Jul_13_storm.1,Oct_11_storm.1,Nov_15_16_storm.1, ncol=3, widths=c(1,1,1)))
dev.off()