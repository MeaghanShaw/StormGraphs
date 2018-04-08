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

CQ$H_mgL<-(10^(-1*CQ$pH))

#H mg/L to mu/L
CQ$H_umolL<-((1000/1.008)*CQ$H_mgL)

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
  ylab("Log("~H^+~", "~mu~"mmol"~L^-1~")\n")+
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
#Add in lines and why is the legend not working?
library(ggplot2)
pal<-c("#eff3ff","#bdd7e7","#6baed6","#2171b5")
shape1<-c(21,22,23,24)
CQbasecat<-ggplot(longbaseCQ, aes(x=logQ,y=concentration,fill=as.factor(ion),shape=as.factor(ion)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  geom_smooth(method=lm,colour="black",se=FALSE)+
  theme_bw()+
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
pdf("CQgraphs.pdf",height=10,width=10)
grid.arrange(arrangeGrob(CQbasecat.1,CQAMD.1, ncol=2, widths=c(1,1)))
dev.off()