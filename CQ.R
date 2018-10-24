# Rename columns to be R friendly

newname<-c("sample_name","date_and_time","Q_m3s","Al_mgL","Ca_mgL","Fe_mgL","K_mgL","Mg_mgL","Mn_mgL","Na_mgL","SO42_mgL","pH","EC")
CQ<-read.csv(file="CQ.csv",header=TRUE)
colnames(CQ)<-newname

#Transform mg/L to mmol/L

CQ$Al_mmolL<-(CQ$Al_mgL/26.982)
CQ$Ca_mmolL<-(CQ$Ca_mgL/40.08)
CQ$Fe_mmolL<-(CQ$Fe_mgL/55.845)
CQ$K_mmolL<-(CQ$K_mgL/39.099)
CQ$Mg_mmolL<-(CQ$Mg_mgL/24.305)
CQ$Mn_mmolL<-(CQ$Mn_mgL/54.938)
CQ$Na_mmolL<-(CQ$Na_mgL/22.99)
CQ$SO42_mmolL<-(CQ$SO42_mgL/96.06)

#Transform pH to H+ mg/L

CQ$H_molL<-(10^(-1*CQ$pH))

#H mg/L to mu/L
CQ$H_umolL<-(1000*1000*CQ$H_molL)


#Take the log of everything

CQ$logQ<-(log10(CQ$Q_m3s))
CQ$logAl<-(log10(CQ$Al_mmolL))
CQ$logCa<-(log10(CQ$Ca_mmolL))
CQ$logFe<-(log10(CQ$Fe_mmolL))
CQ$logK<-(log10(CQ$K_mmolL))
CQ$logMg<-(log10(CQ$Mg_mmolL))
CQ$logMn<-(log10(CQ$Mn_mmolL))
CQ$logNa<-(log10(CQ$Na_mmolL))
CQ$logSO42<-(log10(CQ$SO42_mmolL))
CQ$logH<-(log10(CQ$H_umolL))
CQ$logEC<-(log10(CQ$EC))

#write to csv
write.csv(CQ,"CQdata.csv")

#H+ plot
library(ggplot2)
pal="#c2a5cf"
shape1=21
CQH<-ggplot(CQ, aes(logQ,logH))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"))+
  xlab("\nLog(Q,"~m^3~" "~s^-1~")")+
  ylab("Log("~H^+~", "~mu~"mol"~L^-1~")")+
  geom_smooth(method=lm,colour="black",se=FALSE)
#Call the graph
CQH
pHregression<-lm(logH~logQ,data=CQ)
pHregression
summary(pHregression)

#save to pdf
pdf("CQH.pdf",height=6,width=8)
CQH
dev.off()

#EC plot
library(ggplot2)
pal="#c2a5cf"
shape1=21
CQEC<-ggplot(CQ, aes(logQ,logEC))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"))+
  xlab("\nLog(Q,"~m^3~" "~s^-1~")")+
  ylab("Log(Specific conductance, "~mu~"S "~cm^-1~")\n")+
  geom_smooth(method=lm,colour="black",se=FALSE)
#Call the graph
CQEC

ECregression<-lm(logEC~logQ,data=CQ)
ECregression
summary(ECregression)

#save to pdf
pdf("CQEC.pdf",height=6,width=8)
CQEC
dev.off()

#Reshape the data for basecation CQ plot
CQvarsforbasemelt<-c("logQ","logCa","logK","logMg","logNa")
CQforbasemelt<-CQ[CQvarsforbasemelt]
library(reshape2)
longbaseCQ<-melt(CQforbasemelt,id.vars=c("logQ"),variable.name="ion",value.name="concentration")

#All base cations plot
library(ggplot2)
pal<-c("#eff3ff","#bdd7e7","#6baed6","#2171b5")
shape1<-c(21,22,23,24)
CQbasecat<-ggplot(longbaseCQ, aes(x=logQ,y=concentration,fill=as.factor(ion),shape=as.factor(ion)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  geom_smooth(method=lm,colour="black",se=FALSE)+
  theme_bw()+
  theme(text = element_text(size=20))+
  theme(axis.text=element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())+
  guides(fill=guide_legend(title="ion"),shape=guide_legend(title="ion"))+
  xlab("\nLog(Q, "~m^3~" "~s^-1~")")+
  ylab("Log(Concentration, mmol "~L^-1~")")

#Call the graph
CQbasecat

Caregression<-lm(logCa~logQ,data=CQ)
Caregression
summary(Caregression)

Mgregression<-lm(logMg~logQ,data=CQ)
Mgregression
summary(Mgregression)

Naregression<-lm(logNa~logQ,data=CQ)
Naregression
summary(Naregression)

Kregression<-lm(logK~logQ,data=CQ)
Kregression
summary(Kregression)

#Reshape the data for AMD CQ plot
CQvarsforamdmelt<-c("logQ","logFe","logAl","logMn","logSO42")
CQforamdmelt<-CQ[CQvarsforamdmelt]
library(reshape2)
longamdCQ<-melt(CQforamdmelt,id.vars=c("logQ"),variable.name="ion",value.name="concentration")

#Plot amd derived CQ (Fe, Mn, Al, SO42-)
#All CQ ions plot
#Add in lines and why is the legend not working?
library(ggplot2)
pal<-c("#feedde","#fdbe85","#fd8d3c","#d94701")
shape1<-c(21,22,23,24)
CQAMD<-ggplot(longamdCQ, aes(x=logQ,y=concentration,fill=as.factor(ion), shape=as.factor(ion)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  geom_smooth(method=lm,colour="black",se=FALSE)+
  theme_bw()+
  theme(text = element_text(size=20))+
  theme(axis.text=element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())+
  guides(fill=guide_legend(title="ion"),shape=guide_legend(title="ion"))+
  xlab("\nLog(Q, "~m^3~" "~s^-1~")")+
  ylab("Log(Concentration, mmol "~L^-1~")")
  

#Call the graph
CQAMD

Feregression<-lm(logFe~logQ,data=CQ)
Feregression
summary(Feregression)

Mnregression<-lm(logMn~logQ,data=CQ)
Mnregression
summary(Mnregression)

Alregression<-lm(logAl~logQ,data=CQ)
Alregression
summary(Alregression)

SO42regression<-lm(logSO42~logQ,data=CQ)
SO42regression
summary(SO42regression)

#Put CQ plots in one figure
library(gridExtra)
CQbasecat.1<-CQbasecat+annotate("text",x=-3.8,y=1,label="A",size=14)
CQAMD.1<-CQAMD+annotate("text",x=-3.8,y=1.5,label="B",size=14)

grid.arrange(arrangeGrob(CQbasecat.1,CQAMD.1, ncol=2, widths=c(1,1)))

#Save to PDF
pdf("CQgraphs.pdf",height=8,width=10)
grid.arrange(arrangeGrob(CQbasecat.1,CQAMD.1, ncol=2, widths=c(1,1)))
dev.off()

#Convert CQ to LQ

CQ$Ca_load <- CQ$Ca_mmolL*CQ$Q_m3s*1000
CQ$Fe_load <- CQ$Fe_mmolL*CQ$Q_m3s*1000
CQ$K_load <- CQ$K_mmolL*CQ$Q_m3s*1000
CQ$Mg_load <- CQ$Mg_mmolL*CQ$Q_m3s*1000
CQ$Mn_load <- CQ$Mn_mmolL*CQ$Q_m3s*1000
CQ$Na_load <- CQ$Na_mmolL*CQ$Q_m3s*1000
CQ$Al_load <- CQ$Al_mmolL*CQ$Q_m3s*1000
CQ$SO42_load <- CQ$SO42_mmolL*CQ$Q_m3s*1000
CQ$H_load <- CQ$H_umolL*CQ$Q_m3s*1000

#Log of loads
CQ$logCa_load <- log10(CQ$Ca_load)
CQ$logFe_load <- log10(CQ$Fe_load)
CQ$logK_load <- log10(CQ$K_load)
CQ$logMg_load <- log10(CQ$Mg_load)
CQ$logMn_load <- log10(CQ$Mn_load)
CQ$logNa_load <- log10(CQ$Na_load)
CQ$logAl_load <- log10(CQ$Al_load)
CQ$logSO42_load <- log10(CQ$SO42_load)
CQ$logH_load <- log10(CQ$H_load)

#newdate
library(lubridate)
CQ$newdate<-mdy_hm(CQ$date_and_time)

#Plot H load per date
library(ggplot2)
pal="#c2a5cf"
shape1=21
HLoad<-ggplot(CQ, aes(newdate,H_load))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=30))+
  theme(axis.text=element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("\nDate")+
  ylab("H ("~mu~"mol "~s^-1~")")
#Call the graph
HLoad

#Save to PDF
pdf("HLoadbydate.pdf",height=8,width=8)
HLoad
dev.off()

#Plot Ca load per date
library(ggplot2)
pal="#c2a5cf"
shape1=21
CaLoad<-ggplot(CQ, aes(newdate,Ca_load))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=30))+
  theme(axis.text=element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Date")+
  ylab("Ca (mmol "~s^-1~")")
#Call the graph
CaLoad

#Plot Fe load per date
library(ggplot2)
pal="#c2a5cf"
shape1=21
FeLoad<-ggplot(CQ, aes(newdate,Fe_load))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=30))+
  theme(axis.text=element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Date")+
  ylim(0,1.5)+
  ylab("Fe (mmol "~s^-1~")")
#Call the graph
FeLoad

#Plot K load per date
library(ggplot2)
pal="#c2a5cf"
shape1=21
KLoad<-ggplot(CQ, aes(newdate,K_load))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=30))+
  theme(axis.text=element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Date")+
  ylab("K (mmol "~s^-1~")")
#Call the graph
KLoad

#Plot Mg load per date
library(ggplot2)
pal="#c2a5cf"
shape1=21
MgLoad<-ggplot(CQ, aes(newdate,Mg_load))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=30))+
  theme(axis.text=element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Date")+
  ylab("Mg (mmol "~s^-1~")")
#Call the graph
MgLoad

#Plot Mn load per date
library(ggplot2)
pal="#c2a5cf"
shape1=21
MnLoad<-ggplot(CQ, aes(newdate,Mn_load))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=30))+
  theme(axis.text=element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Date")+
  ylab("Mn (mmol "~s^-1~")")
#Call the graph
MnLoad

#Plot Na load per date
library(ggplot2)
pal="#c2a5cf"
shape1=21
NaLoad<-ggplot(CQ, aes(newdate,Na_load))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=30))+
  theme(axis.text=element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Date")+
  ylab("Na (mmol "~s^-1~")")
#Call the graph
NaLoad

#Plot Al load per date
library(ggplot2)
pal="#c2a5cf"
shape1=21
AlLoad<-ggplot(CQ, aes(newdate,Al_load))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=30))+
  theme(axis.text=element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Date")+
  ylim(0,1.5)+
  ylab("Al (mmol "~s^-1~")")
#Call the graph
AlLoad

#Plot SO42 load per date
library(ggplot2)
pal="#c2a5cf"
shape1=21
SO42Load<-ggplot(CQ, aes(newdate,SO42_load))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=30))+
  theme(axis.text=element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Date")+
  ylab("S"~O[4]^-2~" (mmol "~s^-1~")")
#Call the graph
SO42Load


#Put all load by date plots in one figure
library(gridExtra)
library(lubridate)
startdate<-"2017-06-12"
xposition<-date(startdate)
CaLoad.1<-CaLoad+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=8,label="E",size=14)
FeLoad.1<-FeLoad+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.28,label="A",size=14)
KLoad.1<-KLoad+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.35,label="G",size=14)
MgLoad.1<-MgLoad+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=5.5,label="F",size=14)
MnLoad.1<-MnLoad+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.6,label="B",size=14)
NaLoad.1<-NaLoad+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.7,label="H",size=14)
AlLoad.1<-AlLoad+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=0.19,label="C",size=14)
SO42Load.1<-SO42Load+guides(fill=FALSE,shape=FALSE)+annotate("text",x=xposition,y=15,label="D",size=12.5)
grid.arrange(arrangeGrob(FeLoad,MnLoad,AlLoad,SO42Load, CaLoad, MgLoad, KLoad, NaLoad, ncol=2, widths=c(1,1)))

#Save to PDF
pdf("Outloadbydate.pdf",height=20,width=25)
grid.arrange(arrangeGrob(FeLoad,MnLoad,AlLoad,SO42Load, CaLoad, MgLoad, KLoad, NaLoad, ncol=2, widths=c(1,1)))
dev.off()

#Reshape the data for LQ plot
LQvarsforamdmelt<-c("Q_m3s","H_load","Ca_load","Fe_load","Al_load","K_load","Mg_load","Mn_load","Na_load","SO42_load")
LQforamdmelt<-CQ[LQvarsforamdmelt]
library(reshape2)
longLQ<-melt(LQforamdmelt,id.vars=c("Q_m3s"),variable.name="ion",value.name="load")

#LQ for H
library(ggplot2)
pal="#c2a5cf"
shape1=21
HLQ<-ggplot(CQ, aes(logQ,logH_load))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=30))+
  theme(axis.text=element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("log(Q, "~m^3~s^-1~")")+
  ylab("log(H, "~mu~"mol "~s^-1~")")
#Call the graph
HLQ

#Save to PDF
pdf("HLQ.pdf",height=10,width=10)
HLQ
dev.off()

#LQ for Ca
library(ggplot2)
pal="#c2a5cf"
shape1=21
CaLQ<-ggplot(CQ, aes(logQ,logCa_load))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=30))+
  theme(axis.text=element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("log(Q, "~m^3~s^-1~")")+
  ylab("log(Ca, mmol "~s^-1~")")
#Call the graph
CaLQ

#LQ for Fe
library(ggplot2)
pal="#c2a5cf"
shape1=21
FeLQ<-ggplot(CQ, aes(logQ,logFe_load))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=30))+
  theme(axis.text=element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("log(Q, "~m^3~s^-1~")")+
  ylab("log(Fe, mmol "~s^-1~")")
#Call the graph
FeLQ

#LQ for K
library(ggplot2)
pal="#c2a5cf"
shape1=21
KLQ<-ggplot(CQ, aes(logQ,logK_load))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=30))+
  theme(axis.text=element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("log(Q, "~m^3~s^-1~")")+
  ylim(-1.5,1)+
  ylab("log(K, mmol "~s^-1~")")
#Call the graph
KLQ

#LQ for Mg
library(ggplot2)
pal="#c2a5cf"
shape1=21
MgLQ<-ggplot(CQ, aes(logQ,logMg_load))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=30))+
  theme(axis.text=element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("log(Q, "~m^3~s^-1~")")+
  ylab("log(Mg, mmol "~s^-1~")")
#Call the graph
MgLQ

#LQ for Mn
library(ggplot2)
pal="#c2a5cf"
shape1=21
MnLQ<-ggplot(CQ, aes(logQ,logMn_load))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=30))+
  theme(axis.text=element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("log(Q, "~m^3~s^-1~")")+
  ylim(-1.5,1)+
  ylab("log(Mn, mmol "~s^-1~")")
#Call the graph
MnLQ

#LQ for Na
library(ggplot2)
pal="#c2a5cf"
shape1=21
NaLQ<-ggplot(CQ, aes(logQ,logNa_load))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=30))+
  theme(axis.text=element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("log(Q, "~m^3~s^-1~")")+
  ylab("log(Na, mmol "~s^-1~")")
#Call the graph
NaLQ

#LQ for Al
library(ggplot2)
pal="#c2a5cf"
shape1=21
AlLQ<-ggplot(CQ, aes(logQ,logAl_load))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=30))+
  theme(axis.text=element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("log(Q, "~m^3~s^-1~")")+
  ylab("log(Al, mmol "~s^-1~")")
#Call the graph
AlLQ

#LQ for SO42
library(ggplot2)
pal="#c2a5cf"
shape1=21
SO42LQ<-ggplot(CQ, aes(logQ,logSO42_load))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(text = element_text(size=30))+
  theme(axis.text=element_text(size=30))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("log(Q, "~m^3~s^-1~")")+
  ylab("log(S"~O[4]^-2~", mmol "~s^-1~")")
#Call the graph
SO42LQ

#Put all load by date plots in one figure
library(gridExtra)
grid.arrange(arrangeGrob(FeLQ,MnLQ,AlLQ,SO42LQ, CaLQ, MgLQ, KLQ, NaLQ, ncol=2, widths=c(1,1)))

#Save to PDF
pdf("OutLQ.pdf",height=20,width=25)
grid.arrange(arrangeGrob(FeLQ,MnLQ,AlLQ,SO42LQ, CaLQ, MgLQ, KLQ, NaLQ, ncol=2, widths=c(1,1)))
dev.off()

#Reshape the data for basecation LQ plot
LQvarsforbasemelt<-c("logQ","logCa_load","logK_load","logMg_load","logNa_load")
LQforbasemelt<-CQ[LQvarsforbasemelt]
library(reshape2)
longbaseLQ<-melt(LQforbasemelt,id.vars=c("logQ"),variable.name="ion",value.name="load")

#All base cations plot
library(ggplot2)
pal<-c("#eff3ff","#bdd7e7","#6baed6","#2171b5")
shape1<-c(21,22,23,24)
LQbasecat<-ggplot(longbaseLQ, aes(x=logQ,y=load,fill=as.factor(ion),shape=as.factor(ion)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  geom_smooth(method=lm,colour="black",se=FALSE)+
  theme_bw()+
  theme(text = element_text(size=20))+
  theme(axis.text=element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())+
  guides(fill=guide_legend(title="ion"),shape=guide_legend(title="ion"))+
  xlab("\nLog(Q, "~m^3~" "~s^-1~")")+
  ylab("Log(Load, mmol "~s^-1~")")

#Call the graph
LQbasecat

#Reshape the data for AMD CQ plot
LQvarsforamdmelt<-c("logQ","logFe_load","logAl_load","logMn_load","logSO42_load")
LQforamdmelt<-CQ[LQvarsforamdmelt]
library(reshape2)
longamdLQ<-melt(LQforamdmelt,id.vars=c("logQ"),variable.name="ion",value.name="load")

#Plot amd derived CQ (Fe, Mn, Al, SO42-)
#All CQ ions plot
#Add in lines and why is the legend not working?
library(ggplot2)
pal<-c("#feedde","#fdbe85","#fd8d3c","#d94701")
shape1<-c(21,22,23,24)
LQAMD<-ggplot(longamdLQ, aes(x=logQ,y=load,fill=as.factor(ion), shape=as.factor(ion)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  geom_smooth(method=lm,colour="black",se=FALSE)+
  theme_bw()+
  theme(text = element_text(size=20))+
  theme(axis.text=element_text(size=20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())+
  guides(fill=guide_legend(title="ion"),shape=guide_legend(title="ion"))+
  xlab("\nLog(Q, "~m^3~" "~s^-1~")")+
  ylab("Log(Load, mmol "~s^-1~")")


#Call the graph
LQAMD

#Put CQ plots in one figure
library(gridExtra)
LQbasecat.1<-LQbasecat+annotate("text",x=-3.8,y=2,label="A",size=14)
LQAMD.1<-LQAMD+annotate("text",x=-3.8,y=2.1,label="B",size=14)

grid.arrange(arrangeGrob(LQbasecat.1,LQAMD.1, ncol=2, widths=c(1,1)))

#Save to PDF
pdf("LQgraphs.pdf",height=8,width=12)
grid.arrange(arrangeGrob(LQbasecat.1,LQAMD.1, ncol=2, widths=c(1,1)))
dev.off()

