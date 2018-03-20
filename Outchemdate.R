#Call outlet data by date
Outchem<-read.csv(file="Outchemdate.csv",header=TRUE)
#Call Q 
Discharge<-read.csv(file="discharge.csv",header=TRUE)

#Transform mg/L to mmol/L

Outchem$Al_mmolL<-(Outchem$Al_mgL/26.982)
Outchem$Ca_mmolL<-(Outchem$Ca_mgL/40.08)
Outchem$Fe_mmolL<-(Outchem$Fe_mgL/55.845)
Outchem$K_mmolL<-(Outchem$K_mgL/39.099)
Outchem$Mg_mmolL<-(Outchem$Mg_mgL/24.305)
Outchem$Mn_mmolL<-(Outchem$Mn_mgL/54.938)
Outchem$Na_mmolL<-(Outchem$Na_mgL/22.99)
Outchem$SO42_mmolL<-(Outchem$SO42_mgL/96.06)
Outchem$Cl_mmolL<-(Outchem$Cl_mgL/35.453)

#Pulls out day of year to plot as x variable (makes scatter plot)
library(lubridate)
Outchem$newdate<-mdy(Outchem$Date)
Outchem$doy<-yday(Outchem$newdate)

#pH and EC std dev and std err
library(plyr)
outlet.summary<-ddply(Outchem, c("Site"), summarise,
                            pHmean = mean(pH, na.rm=TRUE), pHsd = sd(pH, na.rm=TRUE),
                            pHsem = sd(pH, na.rm=TRUE)/sqrt(length(pH)),
                            ECmean = mean(EC,na.rm=TRUE), ECsd = sd(EC,na.rm=TRUE),
                            ECsem = sd(EC,na.rm=TRUE)/sqrt(length(EC)),
                      Almean = mean(Al_mmolL,na.rm=TRUE), Alsd = sd(Al_mmolL,na.rm=TRUE),
                      Alsem = sd(Al_mmolL,na.rm=TRUE)/sqrt(length(Al_mmolL)),
                      Camean = mean(Ca_mmolL,na.rm=TRUE), Casd = sd(Ca_mmolL,na.rm=TRUE),
                      Casem = sd(Ca_mmolL,na.rm=TRUE)/sqrt(length(Ca_mmolL)),
                      Femean = mean(Fe_mmolL,na.rm=TRUE), Fesd = sd(Fe_mmolL,na.rm=TRUE),
                      Fesem = sd(Fe_mmolL,na.rm=TRUE)/sqrt(length(Fe_mmolL)),
                      Kmean = mean(K_mmolL,na.rm=TRUE), Ksd = sd(K_mmolL,na.rm=TRUE),
                      Ksem = sd(K_mmolL,na.rm=TRUE)/sqrt(length(K_mmolL)),
                      Mgmean = mean(Mg_mmolL,na.rm=TRUE), Mgsd = sd(Mg_mmolL,na.rm=TRUE),
                      Mgsem = sd(Mg_mmolL,na.rm=TRUE)/sqrt(length(Mg_mmolL)),
                      Mnmean = mean(Mn_mmolL,na.rm=TRUE), Mnsd = sd(Mn_mmolL,na.rm=TRUE),
                      Mnsem = sd(Mn_mmolL,na.rm=TRUE)/sqrt(length(Mn_mmolL)),
                      Namean = mean(Na_mmolL,na.rm=TRUE), Nasd = sd(Na_mmolL,na.rm=TRUE),
                      Nasem = sd(Na_mmolL,na.rm=TRUE)/sqrt(length(Na_mmolL)),
                      SO42mean = mean(SO42_mmolL,na.rm=TRUE), SO42sd = sd(SO42_mmolL,na.rm=TRUE),
                      SO42sem = sd(SO42_mmolL,na.rm=TRUE)/sqrt(length(SO42_mmolL)),
                      Clmean = mean(Cl_mmolL,na.rm=TRUE), Clsd = sd(Cl_mmolL,na.rm=TRUE),
                      Clsem = sd(Cl_mmolL,na.rm=TRUE)/sqrt(length(Cl_mmolL)))

#Plots outlet pH by date
library(ggplot2)
pal="#c2a5cf"
shape1=21
OutpH<-ggplot(Outchem, aes(newdate,pH))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL))+
  xlab("\nDate")+
  ylab("pH\n")
#Call the graph
OutpH

#Plots outlet EC by date
library(ggplot2)
pal="#c2a5cf"
shape1=21
OutEC<-ggplot(Outchem, aes(newdate,EC))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL))+
  xlab("\nDate")+
  ylab("Specific Conductance\n")
#Call the graph
OutEC

#Put outlet pH and EC plots in one figure
library(gridExtra)
library(lubridate)
startdate<-"2017-04-10"
xposition<-date(startdate)
OutpH.1<-OutpH+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=7,label="A",size=14)
OutEC.1<-OutEC+annotate("text",x=xposition,y=2000,label="B",size=14)
grid.arrange(arrangeGrob(OutpH.1,OutEC.1, ncol=2, widths=c(1,1)))

#Save to PDF
pdf("OutECpH.pdf",height=6,width=8)
grid.arrange(arrangeGrob(OutpH.1,OutEC.1, ncol=2, widths=c(1,1)))
dev.off()

#Plots outlet Al by date
library(ggplot2)
pal="#c2a5cf"
shape1=21
OutAl<-ggplot(Outchem, aes(newdate,Al_mmolL))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL))+
  xlab("\nDate")+
  ylab("Al (mmol L-1)\n")
#Call the graph
OutAl

#Plots outlet Ca by date
library(ggplot2)
pal="#c2a5cf"
shape1=21
OutCa<-ggplot(Outchem, aes(newdate,Ca_mmolL))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL))+
  xlab("\nDate")+
  ylab("Ca (mmol L-1)\n")
#Call the graph
OutCa

#Plots outlet Fe by date
library(ggplot2)
pal="#c2a5cf"
shape1=21
OutFe<-ggplot(Outchem, aes(newdate,Fe_mmolL))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL))+
  xlab("\nDate")+
  ylab("Fe (mmol L-1)\n")
#Call the graph
OutFe

#Plots outlet K by date
library(ggplot2)
pal="#c2a5cf"
shape1=21
OutK<-ggplot(Outchem, aes(newdate,K_mmolL))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL))+
  xlab("\nDate")+
  ylab("K (mmol L-1)\n")
#Call the graph
OutK

#Plots outlet Mg by date
library(ggplot2)
pal="#c2a5cf"
shape1=21
OutMg<-ggplot(Outchem, aes(newdate,Mg_mmolL))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL))+
  xlab("\nDate")+
  ylab("Mg (mmol L-1)\n")
#Call the graph
OutMg

#Plots outlet Mn by date
library(ggplot2)
pal="#c2a5cf"
shape1=21
OutMn<-ggplot(Outchem, aes(newdate,Mn_mmolL))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL))+
  xlab("\nDate")+
  ylab("Mn (mmol L-1)\n")
#Call the graph
OutMn

#Plots outlet Na by date
library(ggplot2)
pal="#c2a5cf"
shape1=21
OutNa<-ggplot(Outchem, aes(newdate,Na_mmolL))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL))+
  xlab("\nDate")+
  ylab("Na (mmol L-1)\n")
#Call the graph
OutNa

#Plots outlet SO42- by date
library(ggplot2)
pal="#c2a5cf"
shape1=21
OutSO42<-ggplot(Outchem, aes(newdate,SO42_mmolL))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL))+
  xlab("\nDate")+
  ylab("SO42- (mmol L-1)\n")
#Call the graph
OutSO42

#Plots outlet Cl by date
library(ggplot2)
pal="#c2a5cf"
shape1=21
OutCl<-ggplot(Outchem, aes(newdate,Cl_mmolL))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL))+
  xlab("\nDate")+
  ylab("Cl (mmol L-1)\n")
#Call the graph
OutCl

#Put all outlet plots in one figure
library(gridExtra)
library(lubridate)
startdate<-"2017-04-10"
xposition<-date(startdate)
OutCa.1<-OutCa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=6,label="C",size=14)
OutFe.1<-OutFe+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=6.8,label="A",size=14)
OutK.1<-OutK+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.6,label="E",size=14)
OutMg.1<-OutMg+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=4,label="D",size=14)
OutMn.1<-OutMn+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition.1,y=0.8,label="B",size=14)
OutNa.1<-OutNa+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition.1,y=0.8,label="F",size=14)
OutAl.1<-OutAl+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition.1,y=0.8,label="F",size=14)
OutSO42.1<-OutSO42+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=14,label="G",size=12.5)
grid.arrange(arrangeGrob(OutFe.1,OutMn.1,OutCa.1,OutMg.1,OutK.1,OutNa.1,OutAl.1,OutSO42.1, ncol=2, widths=c(1,1)))

#Save to PDF
pdf("Outionbydate.pdf",height=20,width=20)
grid.arrange(arrangeGrob(OutFe.1,OutMn.1,OutCa.1,OutMg.1,OutK.1,OutNa.1,OutAl.1,OutSO42.1, ncol=2, widths=c(1,1)))
dev.off()

# Plot outlet pH and Fe by date
library(ggplot2)
yscale<-0.05

Out_pHFe_date<-ggplot(Outchem,aes(x=newdate))+
  geom_point(aes(y=pH),fill="purple",colour="black",pch=21,size=4)+
  geom_point(aes(y=Fe_mmolL/yscale,fill="orange"),colour="black",pch=21,size=4)+
  scale_y_continuous(sec.axis=sec_axis(~.*yscale,name="Concentration"))+
  scale_fill_manual(guide="legend",name="ions",
                    values=c("orange"),labels=c("Fe"))+
  labs(y="pH",
       x="Date")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

Out_pHFe_date
#Save to PDF
pdf("Out_pHFe_date.pdf",height=6,width=8)
Out_pHFe_date
dev.off()

#plot Q by Date
library(lubridate)
Discharge$newdate<-mdy_hm(Discharge$date_and_time)
Discharge$doy<-yday(Discharge$newdate)
library(ggplot2)
OutQ<-ggplot(Discharge,aes(Q_m3s,newdate))+
  geom_line(colour="blue")+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title=NULL),shape=guide_legend(title=NULL))+
  xlab("\nDate")+
  ylab("Q ("~m^3~" "~s^-1~")")
#Call the graph
OutQ

#Save to PDF
pdf("OutQ.pdf",height=20,width=20)
OutQ
dev.off()
