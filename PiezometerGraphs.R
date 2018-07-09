# Rename columns to be R friendly. 
#Piezocat is cation data from piezometers, BDP, and DP
newname<-c("sample_name","Site","date","Depth_cm","Al_mgL","Ca_mgL","Fe_mgL","K_mgL","Mg_mgL","Mn_mgL","Na_mgL")
piezocat<-read.csv(file="PiezometerData.csv",header=TRUE)
colnames(piezocat)<-newname
#Piezoan is anion data from piezometers, BDP, and DP
icnewname<-c("sample_name","Site","date","Depth_cm","SO42_mgL","Cl_mgL")
piezoan<-read.csv(file="ICPiezometer.csv",header=TRUE)
colnames(piezoan)<-icnewname
#Call pH and EC data
pHandEC<-read.csv(file="pHandEC.csv",header=TRUE)
#Call DOC data
piezoDOC<-read.csv(file="PiezometerDOC.csv",header=TRUE)
#Call averages data
avgchem<-read.csv(file="AverageChem.csv",header=TRUE)

#Transform mg/L to mmol/L

piezocat$Al_mmolL<-(piezocat$Al_mgL/26.982)
piezocat$Ca_mmolL<-(piezocat$Ca_mgL/40.08)
piezocat$Fe_mmolL<-(piezocat$Fe_mgL/55.845)
piezocat$K_mmolL<-(piezocat$K_mgL/39.099)
piezocat$Mg_mmolL<-(piezocat$Mg_mgL/24.305)
piezocat$Mn_mmolL<-(piezocat$Mn_mgL/54.938)
piezocat$Na_mmolL<-(piezocat$Na_mgL/22.99)

piezoan$SO42_mmolL<-(piezoan$SO42_mgL/96.06)
piezoan$Cl_mmolL<-(piezoan$Cl_mgL/35.453)

#Cation std dev and std err
library(plyr)
piezocat.site.summary<-ddply(piezocat, c("sample_name","Depth_cm","Site"), summarise,
                         Almean = mean(Al_mmolL), Alsd = sd(Al_mmolL),
                         Alsem = sd(Al_mmolL)/sqrt(length(Al_mmolL)),
                         Camean = mean(Ca_mmolL), Casd = sd(Ca_mmolL),
                         Casem = sd(Ca_mmolL)/sqrt(length(Ca_mmolL)),
                         Femean = mean(Fe_mmolL), Fesd = sd(Fe_mmolL),
                         Fesem = sd(Fe_mmolL)/sqrt(length(Fe_mmolL)),
                         Kmean = mean(K_mmolL), Ksd = sd(K_mmolL),
                         Ksem = sd(K_mmolL)/sqrt(length(K_mmolL)),
                         Mgmean = mean(Mg_mmolL), Mgsd = sd(Mg_mmolL),
                         Mgsem = sd(Mg_mmolL)/sqrt(length(Mg_mmolL)),
                         Mnmean = mean(Mn_mmolL), Mnsd = sd(Mn_mmolL),
                         Mnsem = sd(Mn_mmolL)/sqrt(length(Mn_mmolL)),
                         Namean = mean(Na_mmolL), Nasd = sd(Na_mmolL),
                         Nasem = sd(Na_mmolL)/sqrt(length(Na_mmolL)))

#Anion std dev and std err
library(plyr)
piezoan.site.summary<-ddply(piezoan, c("sample_name","Depth_cm","Site"), summarise,
                             SO42mean = mean(SO42_mmolL,na.rm=TRUE), SO42sd = sd(SO42_mmolL,na.rm=TRUE),
                             SO42sem = sd(SO42_mmolL,na.rm=TRUE)/sqrt(length(SO42_mmolL)),
                             Clmean = mean(Cl_mmolL,na.rm=TRUE), Clsd = sd(Cl_mmolL,na.rm=TRUE),
                             Clsem = sd(Cl_mmolL,na.rm=TRUE)/sqrt(length(Cl_mmolL)))

#pH and EC std dev and std err
library(plyr)
pHandEC.site.summary<-ddply(pHandEC, c("sample_name","Depth_cm","Site"), summarise,
                            pHmean = mean(pH, na.rm=TRUE), pHsd = sd(pH,na.r=TRUE),
                            pHsem = sd(pH,na.rm=TRUE)/sqrt(length(pH)),
                            ECmean = mean(EC,na.rm=TRUE), ECsd = sd(EC,na.rm=TRUE),
                            ECsem = sd(EC,na.rm=TRUE)/sqrt(length(EC)))

#DOC std dev and std err
library(plyr)
DOC.site.summary<-ddply(piezoDOC, c("sample_name","Depth_cm","Site"), summarise,
                            DOCmean = mean(DOC_mmolL, na.rm=TRUE), DOCsd = sd(DOC_mmolL,na.r=TRUE),
                            DOCsem = sd(DOC_mmolL,na.rm=TRUE)/sqrt(length(DOC_mmolL)))

#Access BDP and DP subset of pH and EC data and make file
BDPDPpHEC<-pHandEC.site.summary[which(pHandEC.site.summary$Site == "below dam pool"
                       | pHandEC.site.summary$Site == "dam pool" ),]

#sort pH by depth
BDPDPpHEC<-BDPDPpHEC[order(BDPDPpHEC$Site, BDPDPpHEC$Depth_cm),]

#Plots pH averages for DP and BDP sites
library(ggplot2)
pal<-c("#e6550d","#fdae6b")
shape1<-c(21, 22)
AvgAMDpH<-ggplot(BDPDPpHEC, aes(pHmean,Depth_cm,fill=as.factor(Site),shape=as.factor(Site)))+
  geom_path()+
  geom_errorbarh(data=BDPDPpHEC,aes(y=Depth_cm,x=pHmean,xmin=pHmean-pHsem,xmax=pHmean+pHsem),height=2.5)+
  geom_point(colour="black",size=8)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=20))+
  theme(axis.text=element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\npH")+
  xlim(0,7)+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
AvgAMDpH

#Plots EC averages for DP and BDP sites
library(ggplot2)
pal<-c("#e6550d","#fdae6b")
shape1<-c(21, 22)
AvgAMDEC<-ggplot(BDPDPpHEC, aes(ECmean,Depth_cm,fill=as.factor(Site),shape=as.factor(Site)))+
  geom_path()+
  geom_errorbarh(data=BDPDPpHEC,aes(y=Depth_cm,x=ECmean,xmin=ECmean-ECsem,xmax=ECmean+ECsem),height=2.5)+
  geom_point(colour="black",size=8)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=20))+
  theme(axis.text=element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nSpecific conductance ("~mu~S~cm^-1~")")+
  xlim(0,2000)+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
AvgAMDEC

#Access BDP and DP subset of ion data and make file
BDPDPAMD<-piezocat.site.summary[which(piezocat.site.summary$Site == "below dam pool"
                                      | piezocat.site.summary$Site == "dam pool" ),]
BDPDPan<-piezoan.site.summary[which(piezoan.site.summary$Site == "below dam pool"| piezoan.site.summary$Site == "dam pool"),]

#sort pH by depth
BDPDPAMD<-BDPDPAMD[order(BDPDPAMD$Site, BDPDPAMD$Depth_cm),]
BDPDPan<-BDPDPan[order(BDPDPan$Site, BDPDPan$Depth_cm),]

#Plots Fe averages for DP and BDP sites
library(ggplot2)
pal<-c("#e6550d","#fdae6b")
shape1<-c(21, 22)
AvgAMDFe<-ggplot(BDPDPAMD, aes(Femean,Depth_cm,fill=as.factor(Site),shape=as.factor(Site)))+
  geom_path()+
  geom_errorbarh(data=BDPDPAMD,aes(y=Depth_cm,x=Femean,xmin=Femean-Fesem,xmax=Femean+Fesem),height=2.5)+
  geom_point(colour="black",size=8)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=20))+
  theme(axis.text=element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nFe (mmol "~L^-1~")")+
  xlim(0,3)+
  ylab("Depth (cm)\n")+
  theme(legend.position = "bottom")+
  scale_y_reverse()
#Call the graph
AvgAMDFe

#Plots Mn averages for DP and BDP sites
library(ggplot2)
pal<-c("#e6550d","#fdae6b")
shape1<-c(21, 22)
AvgAMDMn<-ggplot(BDPDPAMD, aes(Mnmean,Depth_cm,fill=as.factor(Site),shape=as.factor(Site)))+
  geom_path()+
  geom_errorbarh(data=BDPDPAMD,aes(y=Depth_cm,x=Mnmean,xmin=Mnmean-Mnsem,xmax=Mnmean+Mnsem),height=2.5)+
  geom_point(colour="black",size=8)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=20))+
  theme(axis.text=element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nMn (mmol "~L^-1~")")+
  xlim(0,1)+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
AvgAMDMn

#Plots Mg averages for DP and BDP sites
library(ggplot2)
pal<-c("#e6550d","#fdae6b")
shape1<-c(21, 22)
AvgAMDMg<-ggplot(BDPDPAMD, aes(Mgmean,Depth_cm,fill=as.factor(Site),shape=as.factor(Site)))+
  geom_path()+
  geom_errorbarh(data=BDPDPAMD,aes(y=Depth_cm,x=Mgmean,xmin=Mgmean-Mgsem,xmax=Mgmean+Mgsem),height=2.5)+
  geom_point(colour="black",size=8)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=20))+
  theme(axis.text=element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nMg (mmol"~L^-1~")")+
  xlim(0,6)+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
AvgAMDMg

#Plots Ca averages for DP and BDP sites
library(ggplot2)
pal<-c("#e6550d","#fdae6b")
shape1<-c(21, 22)
AvgAMDCa<-ggplot(BDPDPAMD, aes(Camean,Depth_cm,fill=as.factor(Site),shape=as.factor(Site)))+
  geom_path()+
  geom_errorbarh(data=BDPDPAMD,aes(y=Depth_cm,x=Camean,xmin=Camean-Casem,xmax=Camean+Casem),height=2.5)+
  geom_point(colour="black",size=8)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=20))+
  theme(axis.text=element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nCa (mmol"~L^-1~")")+
  xlim(0,8)+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
AvgAMDCa

#Plots SO42 averages for DP and BDP sites
library(ggplot2)
pal<-c("#e6550d","#fdae6b")
shape1<-c(21, 22)
AvgAMDSO42<-ggplot(BDPDPan, aes(SO42mean,Depth_cm,fill=as.factor(Site),shape=as.factor(Site)))+
  geom_path()+
  geom_errorbarh(data=BDPDPan,aes(y=Depth_cm,x=SO42mean,xmin=SO42mean-SO42sem,xmax=SO42mean+SO42sem),height=2.5)+
  geom_point(colour="black",size=8)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=20))+
  theme(axis.text=element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nS"~O[4]^-2~" (mmol "~L^-1~")")+
  xlim(0,16)+
  ylab("Depth (cm)\n")+
  theme(legend.position = "bottom")+
  scale_y_reverse()
#Call the graph
AvgAMDSO42

#Put BDP and DP pH, SC, Ca, Mg, SO42, Fe, and Mn plots in one figure
library(gridExtra)
yposition<-5
AvgAMDFe.1<-AvgAMDFe+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0.05,y=yposition,label="E",size=10)
AvgAMDMn.1<-AvgAMDMn+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0.05,y=yposition,label="F",size=10)
AvgAMDMg.1<-AvgAMDMg+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0.25,y=yposition,label="D",size=14)
AvgAMDCa.1<-AvgAMDCa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0.25,y=yposition,label="C",size=14)
AvgAMDpH.1<-AvgAMDpH+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0.25,y=yposition,label="A",size=14)
AvgAMDEC.1<-AvgAMDEC+guides(fill=FALSE,shape=FALSE)+annotate("text",x=50,y=yposition,label="B",size=14)
AvgAMDSO42.1<-AvgAMDSO42+annotate("text",x=0.5,y=yposition,label="G",size=10)
grid.arrange(arrangeGrob(AvgAMDpH.1, AvgAMDEC.1, AvgAMDCa.1, AvgAMDMg.1, AvgAMDFe.1,AvgAMDMn.1,AvgAMDSO42.1, ncol=2, widths=c(1,1)))

#Save to PDF
pdf("BDPDPAVG.pdf",height=20,width=12)
grid.arrange(arrangeGrob(AvgAMDpH.1, AvgAMDEC.1, AvgAMDCa.1, AvgAMDMg.1, AvgAMDFe.1,AvgAMDMn.1,AvgAMDSO42.1, ncol=2, widths=c(1,1)))
dev.off()

#Access BDP subset of pH and EC data and make BDP file
BDPpHEC<-pHandEC[which(pHandEC$sample_name == "BDP"
                       | pHandEC$sample_name == "P1" 
                       | pHandEC$sample_name == "P2"
                       | pHandEC$sample_name == "P3"
                       | pHandEC$sample_name == "P4"),]

#Pulls out day of year to plot as x variable (makes scatter plot)
library(lubridate)
BDPpHEC$newdate<-mdy(BDPpHEC$Date)
BDPpHEC$doy<-yday(BDPpHEC$newdate)

#Plots BDP pH data by date
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
shape1<-c(21, 22, 23, 24, 25)
BDPpH<-ggplot(BDPpHEC, aes(newdate,pH,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  theme(legend.position="top")+
  xlab("\nDate")+
  ylab("pH")
#Call the graph
BDPpH

#Plots BDP EC data by date
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
shape1<-c(21, 22, 23, 24,25)
BDPEC<-ggplot(BDPpHEC, aes(newdate,EC,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  xlab("\nDate")+
  ylab("Specific conductance ("~mu~"S c"~m^-1~")")
#Call the graph
BDPEC

#Put BDP pH and EC plots in one figure
library(gridExtra)
library(lubridate)
startdate<-"2017-03-24"
xposition<-date(startdate)
BDPpH.1<-BDPpH+annotate("text",x=xposition,y=6.4,label="A",size=14)
BDPEC.1<-BDPEC+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=2100,label="B",size=14)
grid.arrange(arrangeGrob(BDPpH.1,BDPEC.1, ncol=1, widths=c(1)))

#Save to PDF
pdf("BDPECpH.pdf",height=10,width=10)
grid.arrange(arrangeGrob(BDPpH.1,BDPEC.1, ncol=1, widths=c(1)))
dev.off()

#Access BDP subset of piezocat data and make BDP file

BDPpiezocat<-piezocat[which(piezocat$sample_name == "BDP"
                      | piezocat$sample_name == "P1"
                      | piezocat$sample_name == "P2" | piezocat$sample_name == "P3"
                      | piezocat$sample_name == "P4"),]

#Access BDP subset of piezoan data and make BDP file

BDPpiezoan<-piezoan[which(piezoan$sample_name == "BDP" | piezoan$sample_name == "P1"
                            | piezoan$sample_name == "P2" | piezoan$sample_name == "P3"
                            | piezoan$sample_name == "P4"),]

#Access BDP subset of piezoDOC data and make BDP file

BDPpiezoDOC<-piezoDOC[which(piezoDOC$sample_name == "BDP" | piezoDOC$sample_name == "P1"
                          | piezoDOC$sample_name == "P2" | piezoDOC$sample_name == "P3"
                          | piezoDOC$sample_name == "P4"),]

#Pulls out day of year to plot as x variable (makes scatter plot)
library(lubridate)
BDPpiezocat$newdate<-mdy(BDPpiezocat$date)
BDPpiezocat$doy<-yday(BDPpiezocat$newdate)
BDPpiezoan$newdate<-mdy(BDPpiezoan$date)
BDPpiezoan$doy<-yday(BDPpiezoan$newdate)
BDPpiezoDOC$newdate<-mdy(BDPpiezoDOC$Date)
BDPpiezoDOC$doy<-yday(BDPpiezoDOC$newdate)

#Plots doy as x and Ca_mmol/L as y
plot(BDPpiezocat$doy,BDPpiezocat$Ca_mmolL)

#Plots BDP Ca data by date and has piezometer data
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
shape1<-c(21, 22, 23, 24,25)
BDPpiezoCa<-ggplot(BDPpiezocat, aes(newdate,Ca_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  xlab("\nDate")+
  ylab("Ca (mmol "~L^-1~")")
#Call the graph
BDPpiezoCa

#Plots BDP Fe data by date and has piezometer depths
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
shape1<-c(21, 22, 23, 24,25)
BDPpiezoFe<-ggplot(BDPpiezocat, aes(newdate,Fe_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  theme(legend.position = "top")+
  xlab("\nDate")+
  ylab("Fe (mmol "~L^-1~")")
#Call the graph
BDPpiezoFe

#Plots BDP K data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
shape1<-c(21, 22, 23, 24,25)
BDPpiezoK<-ggplot(BDPpiezocat, aes(newdate,K_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  ylim(0,0.8)+
  xlab("\nDate")+
  ylab("K (mmol "~L^-1~")")
#Call the graph
BDPpiezoK

#Plots BDP Mg data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
shape1<-c(21, 22, 23, 24,25)
BDPpiezoMg<-ggplot(BDPpiezocat, aes(newdate,Mg_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  ylim(0,8)+
  xlab("\nDate")+
  ylab("Mg (mmol "~L^-1~")")
#Call the graph
BDPpiezoMg

#Plots BDP Mn data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
shape1<-c(21, 22, 23, 24,25)
BDPpiezoMn<-ggplot(BDPpiezocat, aes(newdate,Mn_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  ylim(0,1)+
  xlab("\nDate")+
  ylab("Mn (mmol "~L^-1~")")
#Call the graph
BDPpiezoMn

#Plots BDP Na data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
shape1<-c(21, 22, 23, 24,25)
BDPpiezoNa<-ggplot(BDPpiezocat, aes(newdate,Na_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  xlab("\nDate")+
  ylab("Na (mmol "~L^-1~")")
#Call the graph
BDPpiezoNa

#Plots BDP SO42 data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
shape1<-c(21, 22, 23, 24,25)
BDPpiezoSO42<-ggplot(BDPpiezoan, aes(newdate,SO42_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  xlab("\nDate")+
  ylab("S"~O[4]^-2~" (mmol "~L^-1~")")
#Call the graph
BDPpiezoSO42

#Plots BDP DOC data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
shape1<-c(21, 22, 23, 24,25)
BDPpiezoDOC<-ggplot(BDPpiezoDOC, aes(newdate,DOC_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  xlab("\nDate")+
  ylab("DOC (mmol "~L^-1~")")
#Call the graph
BDPpiezoDOC

#Put all BDP plots in one figure
library(gridExtra)
library(lubridate)
startdate<-"2017-03-25"
xposition<-date(startdate)
BDPCa.1<-BDPpiezoCa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=8.6,label="C",size=14)
BDPFe.1<-BDPpiezoFe+annotate("text",x=xposition,y=1.95,label="A",size=14)
BDPK.1<-BDPpiezoK+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.75,label="E",size=14)
BDPMg.1<-BDPpiezoMg+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=7.5,label="D",size=14)
BDPMn.1<-BDPpiezoMn+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.94,label="B",size=14)
BDPNa.1<-BDPpiezoNa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.75,label="F",size=14)
BDPSO42.1<-BDPpiezoSO42+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=30,label="G",size=14)
BDPDOC.1<-BDPpiezoDOC+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=4,label="H",size=14)
grid.arrange(arrangeGrob(BDPFe.1,BDPMn.1,BDPCa.1,BDPMg.1,BDPK.1,BDPNa.1,BDPSO42.1,BDPDOC.1, ncol=2, widths=c(1,1)))

#Save to PDF
pdf("BDPPiezoGraphs.pdf",height=20,width=20)
grid.arrange(arrangeGrob(BDPFe.1,BDPMn.1,BDPCa.1,BDPMg.1,BDPK.1,BDPNa.1,BDPSO42.1,BDPDOC.1, ncol=2, widths=c(1,1)))
dev.off()

#Access DP subset of pH and EC data and make DP file
DPpHEC<-pHandEC[which(pHandEC$sample_name == "DP"
                       | pHandEC$sample_name == "P5" 
                       | pHandEC$sample_name == "P6"
                       | pHandEC$sample_name == "P7"
                       | pHandEC$sample_name == "P8"),]

#Pulls out day of year to plot as x variable (makes scatter plot)
library(lubridate)
DPpHEC$newdate<-mdy(DPpHEC$Date)
DPpHEC$doy<-yday(DPpHEC$newdate)

#Plots DP pH data by date
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24, 25)
DPpH<-ggplot(DPpHEC, aes(newdate,pH,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  theme(legend.position = "top")+
  xlab("\nDate")+
  ylab("pH")
#Call the graph
DPpH

#Plots DP EC data by date
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24,25)
DPEC<-ggplot(DPpHEC, aes(newdate,EC,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  ylim(0,2000)+
  xlab("\nDate")+
  ylab("Specific conductance ("~mu~"S c"~m^-1~")")+
  theme(legend.position ="bottom")
#Call the graph
DPEC

#Put DP pH and EC plots in one figure
library(gridExtra)
library(lubridate)
startdate<-"2017-03-25"
xposition<-date(startdate)
DPpH.1<-DPpH+annotate("text",x=xposition,y=6.7,label="A",size=14)
DPEC.1<-DPEC+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=1950,label="B",size=14)
grid.arrange(arrangeGrob(DPpH.1,DPEC.1, ncol=1, widths=c(1)))

#Save to PDF
pdf("DPECpH.pdf",height=10,width=10)
grid.arrange(arrangeGrob(DPpH.1,DPEC.1, ncol=1, widths=c(1)))
dev.off()

#Access DP subset of piezocat data and make DP file

DPpiezocat<-piezocat[which(piezocat$sample_name == "DP" | piezocat$sample_name == "P5"
                            | piezocat$sample_name == "P6" | piezocat$sample_name == "P7"
                            | piezocat$sample_name == "P8"),]


#Access DP subset of piezoDOC data and make DP file

DPpiezoDOC<-piezoDOC[which(piezoDOC$sample_name == "DP" | piezoDOC$sample_name == "P5"
                           | piezoDOC$sample_name == "P6" | piezoDOC$sample_name == "P7"
                           | piezoDOC$sample_name == "P8"),]

#Access DP subset of piezoan data and make DP file

DPpiezoan<-piezoan[which(piezoan$sample_name == "DP" | piezoan$sample_name == "P5"
                          | piezoan$sample_name == "P6" | piezoan$sample_name == "P7"
                          | piezoan$sample_name == "P8"),]

#Pulls out day of year to plot as x variable (makes scatter plot)
library(lubridate)
DPpiezocat$newdate<-mdy(DPpiezocat$date)
DPpiezocat$doy<-yday(DPpiezocat$newdate)
DPpiezoan$newdate<-mdy(DPpiezoan$date)
DPpiezoan$doy<-yday(DPpiezoan$newdate)
DPpiezoDOC$newdate<-mdy(DPpiezoDOC$Date)
DPpiezoDOC$doy<-yday(DPpiezoDOC$newdate)

#Plots DP Ca data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24,25)
DPpiezoCa<-ggplot(DPpiezocat, aes(newdate,Ca_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  xlab("\nDate")+
  ylab("Ca (mmol "~L^-1~")")
#Call the graph
DPpiezoCa

#Plots DP Fe data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24,25)
DPpiezoFe<-ggplot(DPpiezocat, aes(newdate,Fe_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  theme(legend.position = "top")+
  xlab("\nDate")+
  ylab("Fe (mmol "~L^-1~")")
#Call the graph
DPpiezoFe

#Plots DP K data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24,25)
DPpiezoK<-ggplot(DPpiezocat, aes(newdate,K_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  xlab("\nDate")+
  ylab("K (mmol "~L^-1~")")
#Call the graph
DPpiezoK

#Plots DP Mg data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24,25)
DPpiezoMg<-ggplot(DPpiezocat, aes(newdate,Mg_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  ylim(0,7)+
  xlab("\nDate")+
  ylab("Mg (mmol "~L^-1~")")
#Call the graph
DPpiezoMg

#Plots DP Mn data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24,25)
DPpiezoMn<-ggplot(DPpiezocat, aes(newdate,Mn_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  xlab("\nDate")+
  ylab("Mn (mmol "~L^-1~")")
#Call the graph
DPpiezoMn

#Plots DP Na data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24,25)
DPpiezoNa<-ggplot(DPpiezocat, aes(newdate,Na_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  ylim(0.1,0.8)+
  xlab("\nDate")+
  ylab("Na (mmol "~L^-1~")")
#Call the graph
DPpiezoNa

#Plots DP SO42 data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24,25)
DPpiezoSO42<-ggplot(DPpiezoan, aes(newdate,SO42_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  xlab("\nDate")+
  ylab("S"~O[4]^-2~" (mmol "~L^-1~")")
#Call the graph
DPpiezoSO42

#Plots DP DOC data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24,25)
DPpiezoDOC<-ggplot(DPpiezoDOC, aes(newdate,DOC_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  xlab("\nDate")+
  ylab("DOC (mmol "~L^-1~")")
#Call the graph
DPpiezoDOC


#Put all DP plots in one figure
library(gridExtra)
library(lubridate)
startdate<-"2017-03-27"
xposition<-date(startdate)
DPCa.1<-DPpiezoCa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=7.6,label="C",size=14)
DPFe.1<-DPpiezoFe+annotate("text",x=xposition,y=2.8,label="A",size=14)
DPK.1<-DPpiezoK+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.3,label="E",size=14)
DPMg.1<-DPpiezoMg+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=6,label="D",size=14)
DPMn.1<-DPpiezoMn+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.75,label="B",size=14)
DPNa.1<-DPpiezoNa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.76,label="F",size=14)
DPSO42.1<-DPpiezoSO42+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=20,label="G",size=14)
DPDOC.1<-DPpiezoDOC+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=6,label="H",size=14)
grid.arrange(arrangeGrob(DPFe.1,DPMn.1,DPCa.1,DPMg.1,DPK.1,DPNa.1,DPSO42.1,DPDOC.1, ncol=2, widths=c(1,1)))

#Save to PDF
pdf("DPPiezoGraphs.pdf",height=20,width=20)
grid.arrange(arrangeGrob(DPFe.1,DPMn.1,DPCa.1,DPMg.1,DPK.1,DPNa.1,DPSO42.1,DPDOC.1, ncol=2, widths=c(1,1)))
dev.off()

#Access SB subset of pH and EC data and make SP file
SBpHEC<-pHandEC[which(pHandEC$sample_name == "P9" 
                      | pHandEC$sample_name == "P10"
                      | pHandEC$sample_name == "P11"
                      | pHandEC$sample_name == "P12"),]

#Pulls out day of year to plot as x variable (makes scatter plot)
library(lubridate)
SBpHEC$newdate<-mdy(SBpHEC$Date)
SBpHEC$doy<-yday(SBpHEC$newdate)

#Plots SB pH data by date
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(22, 23, 24, 25)
SBpH<-ggplot(SBpHEC, aes(newdate,pH,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  theme(legend.position = "top")+
  xlab("\nDate")+
  ylab("pH")
#Call the graph
SBpH

#Plots SB EC data by date
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(22, 23, 24, 25)
SBEC<-ggplot(SBpHEC, aes(newdate,EC,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  ylim(0,2000)+
  xlab("\nDate")+
  ylab("Specific conductance ("~mu~"S c"~m^-1~")")+
  theme(legend.position = "bottom")
#Call the graph
SBEC

#Put SB pH and EC plots in one figure
library(gridExtra)
library(lubridate)
startdate<-"2017-03-27"
xposition<-date(startdate)
SBpH.1<-SBpH+annotate("text",x=xposition,y=6.5,label="A",size=14)
SBEC.1<-SBEC+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=1850,label="B",size=14)
grid.arrange(arrangeGrob(SBpH.1,SBEC.1, ncol=1, widths=c(1)))

#Save to PDF
pdf("SBECpH.pdf",height=10,width=10)
grid.arrange(arrangeGrob(SBpH.1,SBEC.1, ncol=1, widths=c(1)))
dev.off()

#Access SB subset of piezocat data and make SB file

SBpiezocat<-piezocat[which(piezocat$sample_name == "P9" | piezocat$sample_name == "P10"
                           | piezocat$sample_name == "P11" | piezocat$sample_name == "P12"),]

#Access SB subset of piezoan data and make SB file

SBpiezoan<-piezoan[which(piezoan$sample_name == "P9" | piezoan$sample_name == "P10"
                          | piezoan$sample_name == "P11" | piezoan$sample_name == "P12"),]

#Access SB subset of piezoan data and make SB file

SBpiezoDOC<-piezoDOC[which(piezoDOC$sample_name == "P9" | piezoDOC$sample_name == "P10"
                         | piezoDOC$sample_name == "P11" | piezoDOC$sample_name == "P12"),]

#Pulls out day of year to plot as x variable (makes scatter plot)
library(lubridate)
SBpiezocat$newdate<-mdy(SBpiezocat$date)
SBpiezocat$doy<-yday(SBpiezocat$newdate)
SBpiezoan$newdate<-mdy(SBpiezoan$date)
SBpiezoan$doy<-yday(SBpiezoan$newdate)
SBpiezoDOC$newdate<-mdy(SBpiezoDOC$Date)
SBpiezoDOC$doy<-yday(SBpiezoDOC$newdate)

#Plots SB Ca data by date and has all depths
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(22, 23, 24, 25)
SBCa<-ggplot(SBpiezocat, aes(newdate,Ca_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  ylim(0,7)+
  xlab("\nDate")+
  ylab("Ca (mmol "~L^-1~")")
#Call the graph
SBCa


#Plots SB Fe data by date and has all depths
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(22, 23, 24, 25)
SBFe<-ggplot(SBpiezocat, aes(newdate,Fe_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  ylim(0,8)+
  theme(legend.position = "top")+
  xlab("\nDate")+
  ylab("Fe (mmol "~L^-1~")")
#Call the graph
SBFe

#Plots SB K data by date and has all depths
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(22, 23, 24, 25)
SBK<-ggplot(SBpiezocat, aes(newdate,K_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  xlab("\nDate")+
  ylab("K (mmol "~L^-1~")")
#Call the graph
SBK

#Plots SB Mg data by date and has all depths
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(22, 23, 24, 25)
SBMg<-ggplot(SBpiezocat, aes(newdate,Mg_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  xlab("\nDate")+
  ylab("Mg (mmol "~L^-1~")")
#Call the graph
SBMg

#Plots SB Mn data by date and has all depths
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(22, 23, 24, 25)
SBMn<-ggplot(SBpiezocat, aes(newdate,Mn_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  ylim(0,1)+
  xlab("\nDate")+
  ylab("Mn (mmol "~L^-1~")")
#Call the graph
SBMn

#Plots SB Na data by date and has all depths
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(22, 23, 24,25)
SBNa<-ggplot(SBpiezocat, aes(newdate,Na_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  xlab("\nDate")+
  ylim(0,1)+
  ylab("Na (mmol "~L^-1~")")
#Call the graph
SBNa

#Plots SB SO42 data by date and has all depths
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(22, 23, 24, 25)
SBSO42<-ggplot(SBpiezoan, aes(newdate,SO42_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  ylim(0,20)+
  xlab("\nDate")+
  ylab("S"~O[4]^-2~" (mmol "~L^-1~")")+
  theme(legend.position = "bottom")
#Call the graph
SBSO42

#Plots SB DOC data by date and has all depths
library(ggplot2)
pal<-c("#ffffcc","#78c679","#238443")
shape1<-c(22, 24,25)
SBDOC<-ggplot(SBpiezoDOC, aes(newdate,DOC_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  xlab("\nDate")+
  ylab("DOC (mmol "~L^-1~")")+
  theme(legend.position = "bottom")
#Call the graph
SBDOC

#Put all SB plots in one figure
library(gridExtra)
library(lubridate)
startdate<-"2017-03-27"
xposition<-date(startdate)
SBCa.1<-SBCa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=6.4,label="C",size=14)
SBFe.1<-SBFe+annotate("text",x=xposition,y=6.8,label="A",size=14)
SBK.1<-SBK+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.65,label="E",size=14)
SBMg.1<-SBMg+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=4,label="D",size=14)
SBMn.1<-SBMn+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.9,label="B",size=14)
SBNa.1<-SBNa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.9,label="F",size=14)
SBSO42.1<-SBSO42+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=17,label="G",size=12.5)
SBDOC.1<-SBDOC+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=4.5,label="H",size=12.5)
grid.arrange(arrangeGrob(SBFe.1,SBMn.1,SBCa.1,SBMg.1,SBK.1,SBNa.1,SBSO42.1,SBDOC.1, ncol=2, widths=c(1,1)))

#Save to PDF
pdf("SBPiezoGraphs.pdf",height=20,width=20)
grid.arrange(arrangeGrob(SBFe.1,SBMn.1,SBCa.1,SBMg.1,SBK.1,SBNa.1,SBSO42.1,SBDOC.1, ncol=2, widths=c(1,1)))
dev.off()

#sort avg cation chem by depth
avgchem<-piezocat.site.summary[order(piezocat.site.summary$Site, piezocat.site.summary$Depth_cm),]

#sort avg anion chem by depth
avganchem<-piezoan.site.summary[order(piezoan.site.summary$Site, piezoan.site.summary$Depth_cm),]

#sort pH and EC avg chem by depth
avgpHEC<-pHandEC.site.summary[order(pHandEC.site.summary$Site, pHandEC.site.summary$Depth_cm),]

#sort DOC data by depth
avgDOC<-DOC.site.summary[order(DOC.site.summary$Site,DOC.site.summary$Depth_cm),]

#Plots pH averages for all sites
library(ggplot2)
pal<-c("#e6550d","#fdae6b","#f7fcb9")
shape1<-c(21, 22, 23)
AvgpH<-ggplot(avgpHEC, aes(pHmean,Depth_cm,fill=as.factor(Site),shape=as.factor(Site)))+
  geom_path()+
  geom_errorbarh(data=avgpHEC,aes(y=Depth_cm,x=pHmean,xmin=pHmean-pHsem,xmax=pHmean+pHsem),height=2.5)+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\npH")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
AvgpH
#save to pdf
pdf("AvgpHpiezo.pdf",height=6,width=8)
AvgpH
dev.off()

#Plots EC averages for all sites
library(ggplot2)
pal<-c("#e6550d","#fdae6b","#f7fcb9")
shape1<-c(21, 22, 23)
AvgEC<-ggplot(avgpHEC, aes(ECmean,Depth_cm,fill=as.factor(Site),shape=as.factor(Site)))+
  geom_path()+
  geom_errorbarh(data=avgpHEC,aes(y=Depth_cm,x=ECmean,xmin=ECmean-ECsem,xmax=ECmean+ECsem),height=2.5)+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nSpecific Conductance ("~mu~"S"~cm^-1~")")+
  xlim(0,2000)+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
AvgEC

#Plots SO42- averages for all sites
library(ggplot2)
pal<-c("#e6550d","#fdae6b","#f7fcb9")
shape1<-c(21, 22, 23)
AvgSO42<-ggplot(avganchem, aes(SO42mean,Depth_cm,fill=as.factor(Site),shape=as.factor(Site)))+
  geom_path()+
  geom_errorbarh(data=avganchem,aes(y=Depth_cm,x=SO42mean,xmin=SO42mean-SO42sem,xmax=SO42mean+SO42sem),height=2.5)+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\n"~SO[4]^-2~"(mmol "~L^-1~")")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()+
  theme(legend.position="bottom")
#Call the graph
AvgSO42

#Plots Ca averages for all sites
library(ggplot2)
pal<-c("#e6550d","#fdae6b","#f7fcb9")
shape1<-c(21, 22, 23)
AvgCa<-ggplot(avgchem, aes(Camean,Depth_cm,fill=as.factor(Site),shape=as.factor(Site)))+
  geom_path()+
  geom_errorbarh(data=avgchem,aes(y=Depth_cm,x=Camean,xmin=Camean-Casem,xmax=Camean+Casem),height=2.5)+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nCa (mmol "~L^-1~")")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
AvgCa

#Plots Fe averages for all sites
library(ggplot2)
pal<-c("#e6550d","#fdae6b","#f7fcb9")
shape1<-c(21, 22, 23)
AvgFe<-ggplot(avgchem, aes(Femean,Depth_cm,fill=as.factor(Site),shape=as.factor(Site)))+
  geom_path()+
  geom_errorbarh(data=avgchem,aes(y=Depth_cm,x=Femean,xmin=Femean-Fesem,xmax=Femean+Fesem),height=2.5)+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nFe (mmol "~L^-1~")")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()+
  theme(legend.position="top")
#Call the graph
AvgFe

#Plots K averages for all sites
library(ggplot2)
pal<-c("#e6550d","#fdae6b","#f7fcb9")
shape1<-c(21, 22, 23)
AvgK<-ggplot(avgchem, aes(Kmean,Depth_cm,fill=as.factor(Site),shape=as.factor(Site)))+
  geom_path()+
  geom_errorbarh(data=avgchem,aes(y=Depth_cm,x=Kmean,xmin=Kmean-Ksem,xmax=Kmean+Ksem),height=2.5)+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nK (mmol "~L^-1~")")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
AvgK

#Plots Mg averages for all sites
library(ggplot2)
pal<-c("#e6550d","#fdae6b","#f7fcb9")
shape1<-c(21, 22, 23)
AvgMg<-ggplot(avgchem, aes(Mgmean,Depth_cm,fill=as.factor(Site),shape=as.factor(Site)))+
  geom_path()+
  geom_errorbarh(data=avgchem,aes(y=Depth_cm,x=Mgmean,xmin=Mgmean-Mgsem,xmax=Mgmean+Mgsem),height=2.5)+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nMg (mmol "~L^-1~")")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
AvgMg

#Plots Mn averages for all sites
library(ggplot2)
pal<-c("#e6550d","#fdae6b","#f7fcb9")
shape1<-c(21, 22, 23)
AvgMn<-ggplot(avgchem, aes(Mnmean,Depth_cm,fill=as.factor(Site),shape=as.factor(Site)))+
  geom_path()+
  geom_errorbarh(data=avgchem,aes(y=Depth_cm,x=Mnmean,xmin=Mnmean-Mnsem,xmax=Mnmean+Mnsem),height=2.5)+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nMn (mmol "~L^-1~")")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()+
  theme(legend.position="bottom")
#Call the graph
AvgMn

#Plots Na averages for all sites
library(ggplot2)
pal<-c("#e6550d","#fdae6b","#f7fcb9")
shape1<-c(21, 22, 23)
AvgNa<-ggplot(avgchem, aes(Namean,Depth_cm,fill=as.factor(Site),shape=as.factor(Site)))+
  geom_path()+
  geom_errorbarh(data=avgchem,aes(y=Depth_cm,x=Namean,xmin=Namean-Nasem,xmax=Namean+Nasem),height=2.5)+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nNa (mmol "~L^-1~")")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
AvgNa

#Plots DOC averages for all sites
library(ggplot2)
pal<-c("#e6550d","#fdae6b","#f7fcb9")
shape1<-c(21, 22, 23)
AvgDOC<-ggplot(avgDOC, aes(DOCmean,Depth_cm,fill=as.factor(Site),shape=as.factor(Site)))+
  geom_path()+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nDOC (mmol L-1)")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
AvgDOC

#Put all avg plots in one figure
library(gridExtra)
Yposition<-5
AvgCa.1<-AvgCa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=1,y=Yposition,label="C",size=14)
AvgFe.1<-AvgFe+annotate("text",x=0,y=Yposition,label="A",size=14)
AvgK.1<-AvgK+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="E",size=14)
AvgMg.1<-AvgMg+guides(fill=FALSE,shape=FALSE)+annotate("text",x=1,y=Yposition,label="D",size=14)
AvgMn.1<-AvgMn+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="B",size=14)
AvgNa.1<-AvgNa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="F",size=14)
AvgSO42.1<-AvgSO42+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="G",size=14)
AvgDOC.1<-AvgDOC+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="H",size=14)
grid.arrange(arrangeGrob(AvgFe.1,AvgMn.1,AvgCa.1,AvgMg.1,AvgK.1,AvgNa.1,AvgSO42.1,AvgDOC.1, ncol=2, widths=c(1,1)))

#Save to PDF
pdf("SummaryGraphs.pdf",height=20,width=20)
grid.arrange(arrangeGrob(AvgFe.1,AvgMn.1,AvgCa.1,AvgMg.1,AvgK.1,AvgNa.1,AvgSO42.1,AvgDOC.1, ncol=2, widths=c(1,1)))
dev.off()

#Access DP subset of summary data and make DP file

DPAVGCHEM<-avgchem[which(avgchem$Site == "dam pool"),]
DPAVGANCHEM<-avganchem[which(avganchem$Site == "dam pool"),]
DPPHEC<-avgpHEC[which(avgpHEC$Site == "DP"),]
DPDOC<-avgDOC[which(avgDOC$Site == "dam pool"),]

#Plots pH averages for all DP
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24,25)
DPAvgpH<-ggplot(DPPHEC, aes(pHmean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=DPPHEC,aes(y=Depth_cm,x=pHmean,xmin=pHmean-pHsem,xmax=pHmean+pHsem),height=2.5)+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\npH")+
  ylab("Depth (cm)\n")+
  theme(legend.position = "top")+
  scale_y_reverse()
#Call the graph
DPAvgpH

#Plots EC averages for all DP
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24,25)
DPAvgEC<-ggplot(DPPHEC, aes(ECmean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=DPPHEC,aes(y=Depth_cm,x=ECmean,xmin=ECmean-ECsem,xmax=ECmean+ECsem),height=2.5)+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\nSpecific conductance ("~mu~"S c"~m^-1~")")+
  xlim(0,2000)+
  ylab("Depth (cm)\n")+
  theme(legend.position = "bottom")+
  scale_y_reverse()
#Call the graph
DPAvgEC

#Plots SO42- averages for all DP
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24,25)
DPAvgSO42<-ggplot(DPAVGANCHEM, aes(SO42mean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=DPAVGANCHEM,aes(y=Depth_cm,x=SO42mean,xmin=SO42mean-SO42sem,xmax=SO42mean+SO42sem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlim(0,17)+
  xlab("\nS"~O[4]^-2~" (mmol "~L^-1~")")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
DPAvgSO42

#Plots Ca averages for all DP
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24,25)
DPAvgCA<-ggplot(DPAVGCHEM, aes(Camean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=DPAVGCHEM,aes(y=Depth_cm,x=Camean,xmin=Camean-Casem,xmax=Camean+Casem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\nCa (mmol "~L^-1~")")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
DPAvgCA

#Plots Fe averages for all DP
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24,25)
DPAvgFe<-ggplot(DPAVGCHEM, aes(Femean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=DPAVGCHEM,aes(y=Depth_cm,x=Femean,xmin=Femean-Fesem,xmax=Femean+Fesem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlim(0,3)+
  xlab("\nFe (mmol "~L^-1~")")+
  ylab("Depth (cm)\n")+
  theme(legend.position = "top")+
  scale_y_reverse()
#Call the graph
DPAvgFe

#Plots K averages for all DP
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24,25)
DPAvgK<-ggplot(DPAVGCHEM, aes(Kmean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=DPAVGCHEM,aes(y=Depth_cm,x=Kmean,xmin=Kmean-Ksem,xmax=Kmean+Ksem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlim(0,0.3)+
  xlab("\nK (mmol "~L^-1~")")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
DPAvgK

#Plots Mg averages for all DP
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24,25)
DPAvgMg<-ggplot(DPAVGCHEM, aes(Mgmean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=DPAVGCHEM,aes(y=Depth_cm,x=Mgmean,xmin=Mgmean-Mgsem,xmax=Mgmean+Mgsem),height=2.5)+
  geom_point(colour="black",size=6)+
  geom_line()+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\nMg (mmol "~L^-1~")")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
DPAvgMg

#Plots Mn averages for all DP
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24,25)
DPAvgMn<-ggplot(DPAVGCHEM, aes(Mnmean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=DPAVGCHEM,aes(y=Depth_cm,x=Mnmean,xmin=Mnmean-Mnsem,xmax=Mnmean+Mnsem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\nMn (mmol "~L^-1~")")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
DPAvgMn

#Plots Na averages for all DP
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24,25)
DPAvgNa<-ggplot(DPAVGCHEM, aes(Namean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=DPAVGCHEM,aes(y=Depth_cm,x=Namean,xmin=Namean-Nasem,xmax=Namean+Nasem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlim(0.3,0.6)+
  xlab("\nNa (mmol "~L^-1~")")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
DPAvgNa

#Plots DOC average for all DP
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24,25)
DPAvgDOC<-ggplot(DPDOC, aes(DOCmean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=DPDOC,aes(y=Depth_cm,x=DOCmean,xmin=DOCmean-DOCsem,xmax=DOCmean+DOCsem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\nDOC (mmol "~L^-1~")")+
  ylab("Depth (cm)\n")+
  theme(legend.position = "bottom")+
  scale_y_reverse()
#Call the graph
DPAvgDOC

#Put all ion avg plots in one figure
library(gridExtra)
Yposition<-5
DPAvgCA.1<-DPAvgCA+guides(fill=FALSE,shape=FALSE)+annotate("text",x=1,y=Yposition,label="C",size=14)
DPAvgFe.1<-DPAvgFe+annotate("text",x=0,y=Yposition,label="A",size=14)
DPAvgK.1<-DPAvgK+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="E",size=14)
DPAvgMg.1<-DPAvgMg+guides(fill=FALSE,shape=FALSE)+annotate("text",x=1,y=Yposition,label="D",size=14)
DPAvgMn.1<-DPAvgMn+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="B",size=14)
DPAvgNa.1<-DPAvgNa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0.3,y=Yposition,label="F",size=14)
DPAvgSO42.1<-DPAvgSO42+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="G",size=14)
DPAvgDOC.1<-DPAvgDOC+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="H",size=14)
grid.arrange(arrangeGrob(DPAvgFe.1,DPAvgMn.1,DPAvgCA.1,DPAvgMg.1,DPAvgK.1,DPAvgNa.1,DPAvgSO42.1,DPAvgDOC.1, ncol=2, widths=c(1,1)))

#Save to PDF
pdf("DPPiezoavg.pdf",height=20,width=20)
grid.arrange(arrangeGrob(DPAvgFe.1,DPAvgMn.1,DPAvgCA.1,DPAvgMg.1,DPAvgK.1,DPAvgNa.1,DPAvgSO42.1,DPAvgDOC.1, ncol=2, widths=c(1,1)))
dev.off()

#Put DP pH and EC plots in one figure
library(gridExtra)
Yposition<-5
DPAvgpH.1<-DPAvgpH+guides(fill=FALSE,shape=FALSE)+annotate("text",x=3,y=Yposition,label="A",size=14)
DPAvgEC.1<-DPAvgEC+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="B",size=14)
grid.arrange(arrangeGrob(DPAvgpH.1,DPAvgEC.1, ncol=1, widths=c(1)))

#Save to PDF
pdf("DPpHECavg.pdf",height=10,width=10)
grid.arrange(arrangeGrob(DPAvgpH.1,DPAvgEC.1, ncol=1, widths=c(1)))
dev.off()

#Access BDP subset of summary data and make BDP file

BDPAVGCHEM<-avgchem[which(avgchem$Site == "below dam pool"),]
BDPAVGANCHEM<-avganchem[which(avganchem$Site == "below dam pool"),]
BDPPHEC<-avgpHEC[which(avgpHEC$Site == "BDP"),]
BDPDOC<-avgDOC[which(avgDOC$Site == "below dam pool"),]

#Plots pH averages for all BDP
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
shape1<-c(21, 22, 23, 24, 25)
BDPAvgpH<-ggplot(BDPPHEC, aes(pHmean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=BDPPHEC,aes(y=Depth_cm,x=pHmean,xmin=pHmean-pHsem,xmax=pHmean+pHsem),height=2.5)+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\npH")+
  ylab("Depth (cm)\n")+
  theme(legend.position = "top")+
  scale_y_reverse()
#Call the graph
BDPAvgpH

#Plots EC averages for all BDP
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
shape1<-c(21, 22, 23, 24, 25)
BDPAvgEC<-ggplot(BDPPHEC, aes(ECmean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=BDPPHEC,aes(y=Depth_cm,x=ECmean,xmin=ECmean-ECsem,xmax=ECmean+ECsem),height=2.5)+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlim(0,2000)+
  xlab("\nSpecific conductance ("~mu~"S c"~m^-1~")")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()+
  theme(legend.position = "bottom")
#Call the graph
BDPAvgEC

#Plots SO42- averages for all BDP
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
shape1<-c(21, 22, 23, 24, 25)
BDPAvgSO42<-ggplot(BDPAVGANCHEM, aes(SO42mean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=BDPAVGANCHEM,aes(y=Depth_cm,x=SO42mean,xmin=SO42mean-SO42sem,xmax=SO42mean+SO42sem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\nS"~O[4]^-2~" (mmol "~L^-1~")")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
BDPAvgSO42

#Plots Ca averages for all BDP
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
shape1<-c(21, 22, 23, 24, 25)
BDPAvgCA<-ggplot(BDPAVGCHEM, aes(Camean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=BDPAVGCHEM,aes(y=Depth_cm,x=Camean,xmin=Camean-Casem,xmax=Camean+Casem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\nCa (mmol"~L^-1~")")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
BDPAvgCA

#Plots Fe averages for all BDP
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
shape1<-c(21, 22, 23, 24, 25)
BDPAvgFe<-ggplot(BDPAVGCHEM, aes(Femean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=BDPAVGCHEM,aes(y=Depth_cm,x=Femean,xmin=Femean-Fesem,xmax=Femean+Fesem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\nFe (mmol"~L^-1~")")+
  ylab("Depth (cm)\n")+
  theme(legend.position = "top")+
  scale_y_reverse()
#Call the graph
BDPAvgFe

#Plots K averages for all BDP
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
shape1<-c(21, 22, 23, 24, 25)
BDPAvgK<-ggplot(BDPAVGCHEM, aes(Kmean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=BDPAVGCHEM,aes(y=Depth_cm,x=Kmean,xmin=Kmean-Ksem,xmax=Kmean+Ksem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\nK (mmol"~L^-1~")")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
BDPAvgK

#Plots Mg averages for all BDP
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
shape1<-c(21, 22, 23, 24, 25)
BDPAvgMg<-ggplot(BDPAVGCHEM, aes(Mgmean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=BDPAVGCHEM,aes(y=Depth_cm,x=Mgmean,xmin=Mgmean-Mgsem,xmax=Mgmean+Mgsem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlim(2,6)+
  xlab("\nMg (mmol"~L^-1~")")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
BDPAvgMg

#Plots Mn averages for all BDP
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
shape1<-c(21, 22, 23, 24, 25)
BDPAvgMn<-ggplot(BDPAVGCHEM, aes(Mnmean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=BDPAVGCHEM,aes(y=Depth_cm,x=Mnmean,xmin=Mnmean-Mnsem,xmax=Mnmean+Mnsem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\nMn (mmol"~L^-1~")")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
BDPAvgMn

#Plots Na averages for all BDP
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20","#bd0026")
shape1<-c(21, 22, 23, 24, 25)
BDPAvgNa<-ggplot(BDPAVGCHEM, aes(Namean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=BDPAVGCHEM,aes(y=Depth_cm,x=Namean,xmin=Namean-Nasem,xmax=Namean+Nasem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlim(0.3,0.6)+
  xlab("\nNa (mmol"~L^-1~")")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
#Call the graph
BDPAvgNa

#Plots DOC average for all BDP
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24,25)
BDPAvgDOC<-ggplot(BDPDOC, aes(DOCmean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=BDPDOC,aes(y=Depth_cm,x=DOCmean,xmin=DOCmean-DOCsem,xmax=DOCmean+DOCsem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\nDOC (mmol "~L^-1~")")+
  ylab("Depth (cm)\n")+
  theme(legend.position = "bottom")+
  scale_y_reverse()
#Call the graph
BDPAvgDOC

#Put all ion avg plots in one figure
library(gridExtra)
Yposition<-5
BDPAvgCA.1<-BDPAvgCA+guides(fill=FALSE,shape=FALSE)+annotate("text",x=1,y=Yposition,label="C",size=14)
BDPAvgFe.1<-BDPAvgFe+annotate("text",x=0,y=Yposition,label="A",size=14)
BDPAvgK.1<-BDPAvgK+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="E",size=14)
BDPAvgMg.1<-BDPAvgMg+guides(fill=FALSE,shape=FALSE)+annotate("text",x=2,y=Yposition,label="D",size=14)
BDPAvgMn.1<-BDPAvgMn+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="B",size=14)
BDPAvgNa.1<-BDPAvgNa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0.3,y=Yposition,label="F",size=14)
BDPAvgSO42.1<-BDPAvgSO42+guides(fill=FALSE,shape=FALSE)+annotate("text",x=6,y=Yposition,label="G",size=14)
BDPAvgDOC.1<-BDPAvgDOC+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="H",size=14)
grid.arrange(arrangeGrob(BDPAvgFe.1,BDPAvgMn.1,BDPAvgCA.1,BDPAvgMg.1,BDPAvgK.1,BDPAvgNa.1,BDPAvgSO42.1,BDPAvgDOC.1, ncol=2, widths=c(1,1)))

#Put all ion plots in one figure
pdf("BDPPiezoavg.pdf",height=20,width=20)
grid.arrange(arrangeGrob(BDPAvgFe.1,BDPAvgMn.1,BDPAvgCA.1,BDPAvgMg.1,BDPAvgK.1,BDPAvgNa.1,BDPAvgSO42.1,BDPAvgDOC.1, ncol=2, widths=c(1,1)))
dev.off()

#Put pH and EC plots in one figure
library(gridExtra)
Yposition<-5
BDPAvgpH.1<-BDPAvgpH+guides(fill=FALSE,shape=FALSE)+annotate("text",x=3,y=Yposition,label="A",size=14)
BDPAvgEC.1<-BDPAvgEC+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="B",size=14)
grid.arrange(arrangeGrob(BDPAvgpH.1,BDPAvgEC.1, ncol=1, widths=c(1)))

#Save to PDF
pdf("BDPpHECavg.pdf",height=10,width=10)
grid.arrange(arrangeGrob(BDPAvgpH.1,BDPAvgEC.1, ncol=1, widths=c(1)))
dev.off()

#Access SB subset of summary data and make SB file

SBAVGCHEM<-avgchem[which(avgchem$Site == "stream bank"),]
SBAVGANCHEM<-avganchem[which(avganchem$Site == "stream bank"),]
SBPHEC<-avgpHEC[which(avgpHEC$Site == "SB"),]
SBDOC<-avgDOC[which(avgDOC$Site == "stream bank"),]

#Plots pH averages for all SB
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(22, 23, 24, 25)
SBAvgpH<-ggplot(SBPHEC, aes(pHmean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=SBPHEC,aes(y=Depth_cm,x=pHmean,xmin=pHmean-pHsem,xmax=pHmean+pHsem),height=2.5)+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\npH")+
  ylab("Depth (cm)\n")+
  theme(legend.position = "top")+
  ylim(100,15)
#Call the graph
SBAvgpH

#Plots EC averages for all SB
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(22, 23, 24, 25)
SBAvgEC<-ggplot(SBPHEC, aes(ECmean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=SBPHEC,aes(y=Depth_cm,x=ECmean,xmin=ECmean-ECsem,xmax=ECmean+ECsem),height=2.5)+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\nSpecific conductance ("~mu~"S c"~m^-1~")")+
  xlim(0,2000)+
  ylab("Depth (cm)\n")+
  ylim(100,15)
#Call the graph
SBAvgEC

#Plots SO42- averages for all SB
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(22, 23, 24, 25)
SBAvgSO42<-ggplot(SBAVGANCHEM, aes(SO42mean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=SBAVGANCHEM,aes(y=Depth_cm,x=SO42mean,xmin=SO42mean-SO42sem,xmax=SO42mean+SO42sem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\nS"~O[4]^-2~" (mmol "~L^-1~")")+
  ylab("Depth (cm)\n")+
  ylim(100,15)
#Call the graph
SBAvgSO42

#Plots Ca averages for all SB
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(22, 23, 24, 25)
SBAvgCA<-ggplot(SBAVGCHEM, aes(Camean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=SBAVGCHEM,aes(y=Depth_cm,x=Camean,xmin=Camean-Casem,xmax=Camean+Casem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\nCa (mmol"~L^-1~")")+
  ylab("Depth (cm)\n")+
  ylim(100,15)
#Call the graph
SBAvgCA

#Plots Fe averages for all SB
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(22, 23, 24, 25)
SBAvgFe<-ggplot(SBAVGCHEM, aes(Femean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=SBAVGCHEM,aes(y=Depth_cm,x=Femean,xmin=Femean-Fesem,xmax=Femean+Fesem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\nFe (mmol"~L^-1~")")+
  ylab("Depth (cm)\n")+
  theme(legend.position = "top")+
  ylim(100,15)
#Call the graph
SBAvgFe

#Plots K averages for all SB
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(22, 23, 24, 25)
SBAvgK<-ggplot(SBAVGCHEM, aes(Kmean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=SBAVGCHEM,aes(y=Depth_cm,x=Kmean,xmin=Kmean-Ksem,xmax=Kmean+Ksem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\nK (mmol"~L^-1~")")+
  ylab("Depth (cm)\n")+
  ylim(100,15)
#Call the graph
SBAvgK

#Plots Mg averages for all SB
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(22, 23, 24, 25)
SBAvgMg<-ggplot(SBAVGCHEM, aes(Mgmean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=SBAVGCHEM,aes(y=Depth_cm,x=Mgmean,xmin=Mgmean-Mgsem,xmax=Mgmean+Mgsem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\nMg (mmol"~L^-1~")")+
  ylab("Depth (cm)\n")+
  ylim(100,15)
#Call the graph
SBAvgMg

#Plots Mn averages for all SB
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(22, 23, 24, 25)
SBAvgMn<-ggplot(SBAVGCHEM, aes(Mnmean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=SBAVGCHEM,aes(y=Depth_cm,x=Mnmean,xmin=Mnmean-Mnsem,xmax=Mnmean+Mnsem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\nMg (mmol"~L^-1~")")+
  ylab("Depth (cm)\n")+
  ylim(100,15)
#Call the graph
SBAvgMn

#Plots Na averages for all SB
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(22, 23, 24, 25)
SBAvgNa<-ggplot(SBAVGCHEM, aes(Namean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=SBAVGCHEM,aes(y=Depth_cm,x=Namean,xmin=Namean-Nasem,xmax=Namean+Nasem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\nNa (mmol"~L^-1~")")+
  ylab("Depth (cm)\n")+
  ylim(100,15)
#Call the graph
SBAvgNa

#Plots DOC average for all BDP
library(ggplot2)
pal<-c("#ffffcc","#78c679","#238443")
shape1<-c(22, 24, 25)
SBAvgDOC<-ggplot(SBDOC, aes(DOCmean,Depth_cm,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_errorbarh(data=SBDOC,aes(y=Depth_cm,x=DOCmean,xmin=DOCmean-DOCsem,xmax=DOCmean+DOCsem),height=2.5)+
  geom_point(colour="black",size=6)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=25)+
  theme(text = element_text(size=25))+
  theme(axis.text=element_text(size=25))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\nDOC (mmol "~L^-1~")")+
  ylab("Depth (cm)\n")+
  theme(legend.position = "bottom")+
  ylim(100,15)
#Call the graph
SBAvgDOC

#Put all avg plots in one figure
library(gridExtra)
Yposition<-5
SBAvgCA.1<-SBAvgCA+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0.5,y=Yposition,label="C",size=14)
SBAvgFe.1<-SBAvgFe+annotate("text",x=0,y=Yposition,label="A",size=14)
SBAvgK.1<-SBAvgK+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="E",size=14)
SBAvgMg.1<-SBAvgMg+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0.5,y=Yposition,label="D",size=14)
SBAvgMn.1<-SBAvgMn+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="B",size=14)
SBAvgNa.1<-SBAvgNa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="F",size=14)
SBAvgSO42.1<-SBAvgSO42+guides(fill=FALSE,shape=FALSE)+annotate("text",x=1.5,y=Yposition,label="G",size=14)
SBAvgDOC.1<-SBAvgDOC+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0.12,y=Yposition,label="H",size=14)
grid.arrange(arrangeGrob(SBAvgFe.1,SBAvgMn.1,SBAvgCA.1,SBAvgMg.1,SBAvgK.1,SBAvgNa.1,SBAvgSO42.1,SBAvgDOC.1, ncol=2, widths=c(1,1)))

#Save to PDF
pdf("SBPiezoavg.pdf",height=20,width=20)
grid.arrange(arrangeGrob(SBAvgFe.1,SBAvgMn.1,SBAvgCA.1,SBAvgMg.1,SBAvgK.1,SBAvgNa.1,SBAvgSO42.1,SBAvgDOC.1, ncol=2, widths=c(1,1)))
dev.off()

#Put pH and EC plots in one figure
library(gridExtra)
Yposition<-5
SBAvgpH.1<-SBAvgpH+guides(fill=FALSE,shape=FALSE)+annotate("text",x=3,y=Yposition,label="A",size=14)
SBAvgEC.1<-SBAvgEC+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="B",size=14)
grid.arrange(arrangeGrob(SBAvgpH.1,SBAvgEC.1, ncol=1, widths=c(1)))

#Save to PDF
pdf("SBpHECavg.pdf",height=10,width=10)
grid.arrange(arrangeGrob(SBAvgpH.1,SBAvgEC.1, ncol=1, widths=c(1)))
dev.off()

#Merge BDP pH/EC and BDP cation data
BDPchem<-merge(BDPcat,BDPpHEC)

#export csv file
write.csv(BDPchem,"BDPchem.csv")

2# Plot BDP pH and Fe by date
library(ggplot2)
yscale<-0.5

BDP_pHFe_date<-ggplot(BDPchem,aes(x=newdate))+
  geom_point(aes(y=pH),fill="purple",colour="black",pch=21,size=4)+
  geom_point(aes(y=Fe_mmolL/yscale,fill="orange"),colour="black",pch=21,size=4)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="Concentration"))+
  scale_fill_manual(guide="legend",name="ions",
                    values=c("orange"),labels=c("Fe"))+
  labs(y="pH",
       x="Date")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

BDP_pHFe_date
#Save to PDF
pdf("BDP_pHFe_date.pdf",height=6,width=8)
BDP_pHFe_date
dev.off()

#Merge all piezometer data
allpiezoion<-merge(piezocat,piezoan)
allpiezo<-allpiezoion[order(allpiezoion$sample_name,allpiezoion$date),]
allpiezo<-merge(allpiezoion,piezoDOC)
allpiezo<-allpiezo[order(allpiezo$sample_name,allpiezo$date),]
#export csv file
write.csv(allpiezo,"PiezometerCationandAnion.csv")

#Merge average ion, pH, and EC data
avgallpiezoion<-merge(piezocat.site.summary,piezoan.site.summary)
avgallpiezo<-merge(avgallpiezoion,pHandEC.site.summary)
#export to csv file
write.csv(avgallpiezo,"PiezoAvgData.csv")
