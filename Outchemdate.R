#Call outlet data by date
Outchem<-read.csv(file="Outchemdate.csv",header=TRUE)

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
                            pHsem = sd(pH, na.rm=TRUE)/sqrt(length(pH, na.rm=TRUE)),
                            ECmean = mean(EC), ECsd = sd(EC),
                            ECsem = sd(EC)/sqrt(length(EC)),
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
                      Nasem = sd(Na_mmolL)/sqrt(length(Na_mmolL)),
                      SO42mean = mean(SO42_mmolL), SO42sd = sd(SO42_mmolL),
                      SO42sem = sd(SO42_mmolL)/sqrt(length(SO42_mmolL)),
                      Clmean = mean(Cl_mmolL), Clsd = sd(Cl_mmolL),
                      Clsem = sd(Cl_mmolL)/sqrt(length(Cl_mmolL)))

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

#save to pdf
pdf("OutpH.pdf",height=6,width=8)
OutpH
dev.off()

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
  ylab("EC\n")
#Call the graph
OutEC

#save to pdf
pdf("OutEC.pdf",height=6,width=8)
OutEC
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

#save to pdf
pdf("OutAl.pdf",height=6,width=8)
OutAl
dev.off()


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

#save to pdf
pdf("OutCa.pdf",height=6,width=8)
OutCa
dev.off()


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

#save to pdf
pdf("OutFe.pdf",height=6,width=8)
OutFe
dev.off()


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

#save to pdf
pdf("OutK.pdf",height=6,width=8)
OutK
dev.off()

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

#save to pdf
pdf("OutMg.pdf",height=6,width=8)
OutMg
dev.off()


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

#save to pdf
pdf("OutMn.pdf",height=6,width=8)
OutMn
dev.off()


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

#save to pdf
pdf("OutNa.pdf",height=6,width=8)
OutNa
dev.off()


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

#save to pdf
pdf("OutSO42.pdf",height=6,width=8)
OutSO42
dev.off()


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

#save to pdf
pdf("OutCl.pdf",height=6,width=8)
OutCl
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
