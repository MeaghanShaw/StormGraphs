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

BDPpiezocat<-piezocat[which(piezocat$sample_name == "P1"
                      | piezocat$sample_name == "P2" | piezocat$sample_name == "P3"
                      | piezocat$sample_name == "P4"),]

BDPcat<-piezocat[which(piezocat$sample_name == "BDP"),]

#Access BDP subset of piezoan data and make BDP file

BDPpiezoan<-piezoan[which(piezoan$sample_name == "P1"
                            | piezoan$sample_name == "P2" | piezoan$sample_name == "P3"
                            | piezoan$sample_name == "P4"),]

BDPan<-piezoan[which(piezoan$sample_name == "BDP"),]

#Pulls out day of year to plot as x variable (makes scatter plot)
library(lubridate)
BDPpiezocat$newdate<-mdy(BDPpiezocat$date)
BDPpiezocat$doy<-yday(BDPpiezocat$newdate)
BDPcat$newdate<-mdy(BDPcat$date)
BDPcat$doy<-yday(BDPcat$newdate)
BDPpiezoan$newdate<-mdy(BDPpiezoan$date)
BDPpiezoan$doy<-yday(BDPpiezoan$newdate)
BDPan$newdate<-mdy(BDPan$date)
BDPan$doy<-yday(BDPan$newdate)

#Plots doy as x and Ca_mmol/L as y
plot(BDPpiezocat$doy,BDPpiezocat$Ca_mmolL)

#Plots BDP Ca data by date and has piezometer data
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20")
shape1<-c(21, 22, 23, 24)
BDPpiezoCa<-ggplot(BDPpiezocat, aes(newdate,Ca_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Ca (mmol L-1)")
#Call the graph
BDPpiezoCa

#Plots surface BDP Ca data by date
library(ggplot2)
pal<-c("#bd0026")
shape1<-c(21)
BDPCa<-ggplot(BDPcat, aes(newdate,Ca_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Ca (mmol L-1)")
#how do I get rid of the legend for this graph?
#Call the graph
BDPCa

#save to pdf
pdf("BDPCa.pdf",height=6,width=8)
BDPCa
dev.off()

#Plots BDP Fe data by date and has piezometer depths
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20")
shape1<-c(21, 22, 23, 24)
BDPpiezoFe<-ggplot(BDPpiezocat, aes(newdate,Fe_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Fe (mmol L-1)")
#Call the graph
BDPpiezoFe

#Plots BDP surface Fe data by date
library(ggplot2)
pal<-c("#bd0026")
shape1<-c(21)
BDPFe<-ggplot(BDPcat, aes(newdate,Fe_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Fe (mmol L-1)")
#Call the graph
BDPFe

#Plots BDP K data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20")
shape1<-c(21, 22, 23, 24)
BDPpiezoK<-ggplot(BDPpiezocat, aes(newdate,K_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("K (mmol L-1)")
#Call the graph
BDPpiezoK

#Plots BDP surface K data by date 
library(ggplot2)
pal<-c("#bd0026")
shape1<-c(21)
BDPK<-ggplot(BDPcat, aes(newdate,K_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("K (mmol L-1)")
#Call the graph
BDPK

#Plots BDP Mg data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20")
shape1<-c(21, 22, 23, 24)
BDPpiezoMg<-ggplot(BDPpiezocat, aes(newdate,Mg_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Mg (mmol L-1)")
#Call the graph
BDPpiezoMg

#Plots BDP surface Mg data by date 
library(ggplot2)
pal<-c("#bd0026")
shape1<-c(21)
BDPMg<-ggplot(BDPcat, aes(newdate,Mg_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Mg (mmol L-1)")
#Call the graph
BDPMg

#Plots BDP Mn data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20")
shape1<-c(21, 22, 23, 24)
BDPpiezoMn<-ggplot(BDPpiezocat, aes(newdate,Mn_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Mn (mmol L-1)")
#Call the graph
BDPpiezoMn

#Plots BDP surface Mn data by date
library(ggplot2)
pal<-c("#bd0026")
shape1<-c(21)
BDPMn<-ggplot(BDPcat, aes(newdate,Mn_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Mn (mmol L-1)")
#Call the graph
BDPMn

#Plots BDP Na data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20")
shape1<-c(21, 22, 23, 24)
BDPpiezoNa<-ggplot(BDPpiezocat, aes(newdate,Na_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Na (mmol L-1)")
#Call the graph
BDPpiezoNa

#Plots BDP surface Na data by date
library(ggplot2)
pal<-c("#bd0026")
shape1<-c(21)
BDPNa<-ggplot(BDPcat, aes(newdate,Na_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Na (mmol L-1)")
#Call the graph
BDPNa

#Plots BDP SO42 data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffb2","#fecc5c","#fd8d3c","#f03b20")
shape1<-c(21, 22, 23, 24)
BDPpiezoSO42<-ggplot(BDPpiezoan, aes(newdate,SO42_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("SO42- (mmol L-1)")
#Call the graph
BDPpiezoSO42

#Plots BDP SO42 data by date and has all piezometer depths
library(ggplot2)
pal<-c("#bd0026")
shape1<-c(21)
BDPSO42<-ggplot(BDPan, aes(newdate,SO42_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("SO42- (mmol L-1)")
#Call the graph
BDPSO42

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

DPpiezocat<-piezocat[which(piezocat$sample_name == "P5"
                            | piezocat$sample_name == "P6" | piezocat$sample_name == "P7"
                            | piezocat$sample_name == "P8"),]

DPcat<-piezocat[which(piezocat$sample_name == "DP"),]

#Access DP subset of piezoan data and make DP file

DPpiezoan<-piezoan[which(piezoan$sample_name == "P5"
                          | piezoan$sample_name == "P6" | piezoan$sample_name == "P7"
                          | piezoan$sample_name == "P8"),]

DPan<-piezoan[which(piezoan$sample_name == "DP"),]

#Pulls out day of year to plot as x variable (makes scatter plot)
library(lubridate)
DPpiezocat$newdate<-mdy(DPpiezocat$date)
DPpiezocat$doy<-yday(DPpiezocat$newdate)
DPcat$newdate<-mdy(DPcat$date)
DPcat$doy<-yday(DPcat$newdate)
DPpiezoan$newdate<-mdy(DPpiezoan$date)
DPpiezoan$doy<-yday(DPpiezoan$newdate)
DPan$newdate<-mdy(DPan$date)
DPan$doy<-yday(DPan$newdate)

#Plots doy as x and Ca_mmol/L as y
plot(DPpiezocat$doy,DPpiezocat$Ca_mmolL)

#Plots DP Ca data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e")
shape1<-c(21, 22, 23, 24)
DPpiezoCa<-ggplot(DPpiezocat, aes(newdate,Ca_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Ca (mmol L-1)")
#Call the graph
DPpiezoCa

#Plots DP surface Ca data by date 
library(ggplot2)
pal<-c("#993404")
shape1<-c(21)
DPCa<-ggplot(DPcat, aes(newdate,Ca_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Ca (mmol L-1)")
#Call the graph
DPCa

#Plots DP Fe data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e")
shape1<-c(21, 22, 23, 24)
DPpiezoFe<-ggplot(DPpiezocat, aes(newdate,Fe_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Fe (mmol L-1)")
#Call the graph
DPpiezoFe

#Plots DP surface Fe data by date 
library(ggplot2)
pal<-c("#993404")
shape1<-c(21)
DPFe<-ggplot(DPcat, aes(newdate,Fe_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Fe (mmol L-1)")
#Call the graph
DPFe

#Plots DP K data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e")
shape1<-c(21, 22, 23, 24)
DPpiezoK<-ggplot(DPpiezocat, aes(newdate,K_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("K (mmol L-1)")
#Call the graph
DPpiezoK

#Plots DP surface K data by date 
library(ggplot2)
pal<-c("#993404")
shape1<-c(21)
DPK<-ggplot(DPcat, aes(newdate,K_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("K (mmol L-1)")
#Call the graph
DPK

#Plots DP Mg data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e")
shape1<-c(21, 22, 23, 24)
DPpiezoMg<-ggplot(DPpiezocat, aes(newdate,Mg_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Mg (mmol L-1)")
#Call the graph
DPpiezoMg

#Plots DP surface Mg data by date 
library(ggplot2)
pal<-c("#993404")
shape1<-c(21)
DPMg<-ggplot(DPcat, aes(newdate,Mg_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Mg (mmol L-1)")
#Call the graph
DPMg

#Plots DP Mn data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e")
shape1<-c(21, 22, 23, 24)
DPpiezoMn<-ggplot(DPpiezocat, aes(newdate,Mn_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Mn (mmol L-1)")
#Call the graph
DPpiezoMn

#Plots DP surface Mn data by date 
library(ggplot2)
pal<-c("#993404")
shape1<-c(21)
DPMn<-ggplot(DPcat, aes(newdate,Mn_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Mn (mmol L-1)")
#Call the graph
DPMn

#Plots DP Na data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e")
shape1<-c(21, 22, 23, 24)
DPpiezoNa<-ggplot(DPpiezocat, aes(newdate,Na_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Na (mmol L-1)")
#Call the graph
DPpiezoNa

#Plots DP surface Na data by date 
library(ggplot2)
pal<-c("#993404")
shape1<-c(21)
DPNa<-ggplot(DPcat, aes(newdate,Na_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Na (mmol L-1)")
#Call the graph
DPNa

#Plots DP SO42 data by date and has all piezometer depths
library(ggplot2)
pal<-c("#ffffd4","#fed98e","#fe9929","#d95f0e")
shape1<-c(21, 22, 23, 24)
DPpiezoSO42<-ggplot(DPpiezoan, aes(newdate,SO42_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("SO42- (mmol L-1)")
#Call the graph
DPpiezoSO42

#Plots DP surface SO42 data by date
library(ggplot2)
pal<-c("#993404")
shape1<-c(21)
DPSO42<-ggplot(DPan, aes(newdate,SO42_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("SO42- (mmol L-1)")
#Call the graph
DPSO42

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
shape1<-c(21, 22, 23, 24)
SBCa<-ggplot(SBpiezocat, aes(newdate,Ca_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Ca (mmol L-1)")
#Call the graph
SBCa


#Plots SB Fe data by date and has all depths
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(21, 22, 23, 24)
SBFe<-ggplot(SBpiezocat, aes(newdate,Fe_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Fe (mmol L-1)")
#Call the graph
SBFe

#Plots SB K data by date and has all depths
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(21, 22, 23, 24)
SBK<-ggplot(SBpiezocat, aes(newdate,K_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("K (mmol L-1)")
#Call the graph
SBK

#Plots SB Mg data by date and has all depths
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(21, 22, 23, 24)
SBMg<-ggplot(SBpiezocat, aes(newdate,Mg_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Mg (mmol L-1)")
#Call the graph
SBMg

#Plots SB Mn data by date and has all depths
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(21, 22, 23, 24)
SBMn<-ggplot(SBpiezocat, aes(newdate,Mn_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Mn (mmol L-1)")
#Call the graph
SBMn

#Plots SB Na data by date and has all depths
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(21, 22, 23, 24)
SBNa<-ggplot(SBpiezocat, aes(newdate,Na_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("Na (mmol L-1)")
#Call the graph
SBNa

#Plots SB SO42 data by date and has all depths
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
shape1<-c(21, 22, 23, 24, 25)
SBSO42<-ggplot(SBpiezoan, aes(newdate,SO42_mmolL,fill=as.factor(Depth_cm),shape=as.factor(Depth_cm)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"),shape=guide_legend(title="Depth (cm)"))+
  xlab("\nDate")+
  ylab("SO42- (mmol L-1)")
#Call the graph
SBSO42

#Plots pH averages for all sites
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679")
shape1<-c(21, 22, 23)
AvgpH<-ggplot(avgchem, aes(avg_pH,depth_cm,fill=as.factor(site),shape=as.factor(site)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\npH")+
  ylab("Depth (cm)\n")
#Call the graph
AvgpH

#Plots EC averages for all sites
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679")
shape1<-c(21, 22, 23)
AvgEC<-ggplot(avgchem, aes(avg_EC,depth_cm,fill=as.factor(site),shape=as.factor(site)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nEC")+
  ylab("Depth (cm)\n")
#Call the graph
AvgEC

#Plots SO42- averages for all sites
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679")
shape1<-c(21, 22, 23)
AvgSO42<-ggplot(avgchem, aes(avg_SO42_mmolL,depth_cm,fill=as.factor(site),shape=as.factor(site)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nSO42- (mmol L-1)")+
  ylab("Depth (cm)\n")
#Call the graph
AvgSO42


#Plots Ca averages for all sites
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679")
shape1<-c(21, 22, 23)
AvgCa<-ggplot(avgchem, aes(avg_Ca_mmolL,depth_cm,fill=as.factor(site),shape=as.factor(site)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nCa (mmol L-1)")+
  ylab("Depth (cm)\n")
#Call the graph
AvgCa

#Plots Fe averages for all sites
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679")
shape1<-c(21, 22, 23)
AvgFe<-ggplot(avgchem, aes(avg_Fe_mmolL,depth_cm,fill=as.factor(site),shape=as.factor(site)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nFe (mmol L-1)")+
  ylab("Depth (cm)\n")
#Call the graph
AvgFe

#Plots K averages for all sites
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679")
shape1<-c(21, 22, 23)
AvgK<-ggplot(avgchem, aes(avg_K_mmolL,depth_cm,fill=as.factor(site),shape=as.factor(site)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nK (mmol L-1)")+
  ylab("Depth (cm)\n")
#Call the graph
AvgK

#Plots Mg averages for all sites
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679")
shape1<-c(21, 22, 23)
AvgMg<-ggplot(avgchem, aes(avg_Mg_mmolL,depth_cm,fill=as.factor(site),shape=as.factor(site)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nMg (mmol L-1)")+
  ylab("Depth (cm)\n")
#Call the graph
AvgMg

#Plots Mn averages for all sites
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679")
shape1<-c(21, 22, 23)
AvgMn<-ggplot(avgchem, aes(avg_Mn_mmolL,depth_cm,fill=as.factor(site),shape=as.factor(site)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nMn (mmol L-1)")+
  ylab("Depth (cm)\n")
#Call the graph
AvgMn

#Plots Na averages for all sites
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679")
shape1<-c(21, 22, 23)
AvgNa<-ggplot(avgchem, aes(avg_Na_mmolL,depth_cm,fill=as.factor(site),shape=as.factor(site)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nNa (mmol L-1)")+
  ylab("Depth (cm)\n")
#Call the graph
AvgNa

#Plots DOC averages for all sites
library(ggplot2)
pal<-c("#ffffcc","#c2e699","#78c679")
shape1<-c(21, 22, 23)
AvgDOC<-ggplot(avgchem, aes(avg_DOC_mmolL,depth_cm,fill=as.factor(site),shape=as.factor(site)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nDOC (mmol L-1)")+
  ylab("Depth (cm)\n")+
  #scale_x_discrete(limits=rev(levels(???)))
#Call the graph
AvgDOC
