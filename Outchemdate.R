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
