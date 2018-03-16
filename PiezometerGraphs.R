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
                             SO42mean = mean(SO42_mmolL), SO42sd = sd(SO42_mmolL),
                             SO42sem = sd(SO42_mmolL)/sqrt(length(SO42_mmolL)),
                             Clmean = mean(Cl_mmolL), Clsd = sd(Cl_mmolL),
                             Clsem = sd(Cl_mmolL)/sqrt(length(Cl_mmolL)))

#pH and EC std dev and std err
library(plyr)
pHandEC.site.summary<-ddply(pHandEC, c("sample_name","Depth_cm","Site"), summarise,
                            pHmean = mean(pH), pHsd = sd(pH),
                            pHsem = sd(pH)/sqrt(length(pH)),
                            ECmean = mean(EC), ECsd = sd(EC),
                            ECsem = sd(EC)/sqrt(length(EC)))

#box and whisker plot of piezometer data and SO42-
plot(piezoan$sample_name,piezoan$SO42_mmolL)

#box and whisker plot of piezometer data and Ca
plot(piezocat$sample_name,piezocat$Ca_mmolL)

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
  xlab("\nDate")+
  ylab("EC")
#Call the graph
BDPEC

#Access BDP subset of piezocat data and make BDP file

BDPpiezocat<-piezocat[which(piezocat$sample_name == "BDP"
                      | piezocat$sample_name == "P1"
                      | piezocat$sample_name == "P2" | piezocat$sample_name == "P3"
                      | piezocat$sample_name == "P4"),]

#Access BDP subset of piezoan data and make BDP file

BDPpiezoan<-piezoan[which(piezoan$sample_name == "BDP" | piezoan$sample_name == "P1"
                            | piezoan$sample_name == "P2" | piezoan$sample_name == "P3"
                            | piezoan$sample_name == "P4"),]

#Pulls out day of year to plot as x variable (makes scatter plot)
library(lubridate)
BDPpiezocat$newdate<-mdy(BDPpiezocat$date)
BDPpiezocat$doy<-yday(BDPpiezocat$newdate)
BDPpiezoan$newdate<-mdy(BDPpiezoan$date)
BDPpiezoan$doy<-yday(BDPpiezoan$newdate)

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
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
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
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
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
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
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
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
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
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
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
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
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
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("SO42- (mmol "~L^-1~")")
#Call the graph
BDPpiezoSO42

#Put all BDP plots in one figure
library(gridExtra)
library(lubridate)
startdate<-"2017-03-25"
xposition<-date(startdate)
BDPCa.1<-BDPpiezoCa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=8.6,label="A",size=14)
BDPFe.1<-BDPpiezoFe+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=1.9,label="E",size=14)
BDPK.1<-BDPpiezoK+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.65,label="C",size=14)
BDPMg.1<-BDPpiezoMg+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=6.3,label="B",size=14)
BDPMn.1<-BDPpiezoMn+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.83,label="F",size=14)
BDPNa.1<-BDPpiezoNa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.75,label="D",size=14)
BDPSO42.1<-BDPpiezoSO42+annotate("text",x=xposition,y=30,label="G",size=14)
grid.arrange(arrangeGrob(BDPCa.1,BDPMg.1,BDPK.1,BDPNa.1,BDPFe.1,BDPMn.1,BDPSO42.1, ncol=2, widths=c(1,1)))

#Save to PDF
pdf("BDPPiezoGraphs.pdf",height=20,width=20)
grid.arrange(arrangeGrob(BDPCa.1,BDPMg.1,BDPK.1,BDPNa.1,BDPFe.1,BDPMn.1,BDPSO42.1, ncol=2, widths=c(1,1)))
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
  xlab("\nDate")+
  ylab("EC")
#Call the graph
DPEC

#Access DP subset of piezocat data and make DP file

DPpiezocat<-piezocat[which(piezocat$sample_name == "DP" | piezocat$sample_name == "P5"
                            | piezocat$sample_name == "P6" | piezocat$sample_name == "P7"
                            | piezocat$sample_name == "P8"),]

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

#Plots doy as x and Ca_mmol/L as y
plot(DPpiezocat$doy,DPpiezocat$Ca_mmolL)

#Plots DP Ca data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
shape1<-c(21, 22, 23, 24,25)
DPpiezoCa<-ggplot(DPpiezocat, aes(newdate,Ca_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
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
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
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
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
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
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
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
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
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
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
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
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("SO42- (mmol "~L^-1~")")
#Call the graph
DPpiezoSO42

#Put all DP plots in one figure
library(gridExtra)
library(lubridate)
startdate<-"2017-03-27"
xposition<-date(startdate)
DPCa.1<-DPpiezoCa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=7.8,label="A",size=14)
DPFe.1<-DPpiezoFe+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=2.8,label="E",size=14)
DPK.1<-DPpiezoK+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.3,label="C",size=14)
DPMg.1<-DPpiezoMg+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=5.8,label="B",size=14)
DPMn.1<-DPpiezoMn+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.75,label="F",size=14)
DPNa.1<-DPpiezoNa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.73,label="D",size=14)
DPSO42.1<-DPpiezoSO42+annotate("text",x=xposition,y=20,label="G",size=14)
grid.arrange(arrangeGrob(DPCa.1,DPMg.1,DPK.1,DPNa.1,DPFe.1,DPMn.1,DPSO42.1, ncol=2, widths=c(1,1)))

#Save to PDF
pdf("DPPiezoGraphs.pdf",height=20,width=20)
grid.arrange(arrangeGrob(DPCa.1,DPMg.1,DPK.1,DPNa.1,DPFe.1,DPMn.1,DPSO42.1, ncol=2, widths=c(1,1)))
dev.off()

#Access SB subset of pH and EC data and make DP file
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
shape1<-c(21, 22, 23, 24)
SBpH<-ggplot(SBpHEC, aes(newdate,pH,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("pH")
#Call the graph
SBpH

#Plots SB EC data by date
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(21, 22, 23, 24)
SBEC<-ggplot(SBpHEC, aes(newdate,EC,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("EC")
#Call the graph
SBEC

#Access SB subset of piezocat data and make SB file

SBpiezocat<-piezocat[which(piezocat$sample_name == "P9" | piezocat$sample_name == "P10"
                           | piezocat$sample_name == "P11" | piezocat$sample_name == "P12"),]
#Access SB subset of piezoan data and make SB file

SBpiezoan<-piezoan[which(piezoan$sample_name == "P9" | piezoan$sample_name == "P10"
                          | piezoan$sample_name == "P11" | piezoan$sample_name == "P12"),]

#Pulls out day of year to plot as x variable (makes scatter plot)
library(lubridate)
SBpiezocat$newdate<-mdy(SBpiezocat$date)
SBpiezocat$doy<-yday(SBpiezocat$newdate)
SBpiezoan$newdate<-mdy(SBpiezoan$date)
SBpiezoan$doy<-yday(SBpiezoan$newdate)

#Plots doy as x and Ca_mmol/L as y
plot(SBpiezocat$doy,SBpiezocat$Ca_mmolL)

#Plots SB Ca data by date and has all depths
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(22, 23, 24, 25)
SBCa<-ggplot(SBpiezocat, aes(newdate,Ca_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
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
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
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
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
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
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
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
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
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
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
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
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("SO42- (mmol "~L^-1~")")
#Call the graph
SBSO42

#Put all SB plots in one figure
library(gridExtra)
library(lubridate)
startdate<-"2017-04-10"
xposition<-date(startdate)
startdate.1<-"2017-11-20"
xposition.1<-date(startdate.1)
SBCa.1<-SBCa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=6,label="A",size=14)
SBFe.1<-SBFe+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=6.8,label="E",size=14)
SBK.1<-SBK+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.6,label="C",size=14)
SBMg.1<-SBMg+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=4,label="B",size=14)
SBMn.1<-SBMn+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition.1,y=0.8,label="F",size=14)
SBNa.1<-SBNa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition.1,y=0.8,label="D",size=14)
SBSO42.1<-SBSO42+annotate("text",x=xposition,y=14,label="G",size=12.5)
grid.arrange(arrangeGrob(SBCa.1,SBMg.1,SBK.1,SBNa.1,SBFe.1,SBMn.1,SBSO42.1, ncol=2, widths=c(1,1)))

#Save to PDF?
pdf("SBPiezoGraphs.pdf",height=20,width=20)
grid.arrange(arrangeGrob(SBCa.1,SBMg.1,SBK.1,SBNa.1,SBFe.1,SBMn.1,SBSO42.1, ncol=2, widths=c(1,1)))
dev.off()

#sort avg cation chem by depth
avgchem<-piezocat.site.summary[order(piezocat.site.summary$Site, piezocat.site.summary$Depth_cm),]

#sort avg anion chem by depth
avganchem<-piezoan.site.summary[order(piezoan.site.summary$Site, piezoan.site.summary$Depth_cm),]

#sort pH and EC avg chem by depth
avgpHEC<-pHandEC.site.summary[order(pHandEC.site.summary$Site, pHandEC.site.summary$Depth_cm),]

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
  xlab("\n"~SO[4]^2~"(mmol "~L^-1~")")+
  ylab("Depth (cm)\n")+
  scale_y_reverse()
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
  theme(legend.position="bottom")
#Call the graph
AvgFe
#save to pdf
pdf("AvgFepiezo.pdf",height=6,width=8)
AvgFe
dev.off()

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

#Put all avg plots in one figure
library(gridExtra)
Yposition<-0
AvgCa.1<-AvgCa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=1,y=Yposition,label="A",size=14)
AvgFe.1<-AvgFe+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="E",size=14)
AvgK.1<-AvgK+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="C",size=14)
AvgMg.1<-AvgMg+guides(fill=FALSE,shape=FALSE)+annotate("text",x=1,y=Yposition,label="B",size=14)
AvgMn.1<-AvgMn+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="F",size=14)
AvgNa.1<-AvgNa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=0,y=Yposition,label="D",size=14)
AvgSO42.1<-AvgSO42+annotate("text",x=6,y=Yposition,label="G",size=14)
grid.arrange(arrangeGrob(AvgCa.1,AvgMg.1,AvgK.1,AvgNa.1,AvgFe.1,AvgMn.1,AvgSO42.1, ncol=2, widths=c(1,1)))

#Save to PDF
pdf("SummaryGraphs.pdf",height=20,width=20)
grid.arrange(arrangeGrob(AvgCa.1,AvgMg.1,AvgK.1,AvgNa.1,AvgFe.1,AvgMn.1,AvgSO42.1, ncol=2, widths=c(1,1)))
dev.off()

#Plots DOC averages for all sites
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679")
shape1<-c(21, 22, 23)
AvgDOC<-ggplot(avgchem, aes(avg_DOC_mmolL,depth_cm,fill=as.factor(site),shape=as.factor(site)))+
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
#save to pdf
pdf("AvgDOCpiezo.pdf",height=6,width=8)
AvgDOC
dev.off()

#Merge BDP pH/EC and BDP cation data
BDPchem<-merge(BDPcat,BDPpHEC)
# Plot BDP pH and Fe by date
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
#export csv file
write.csv(allpiezoion,"PiezometerCationandAnion.csv")
