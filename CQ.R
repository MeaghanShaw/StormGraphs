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
CQ$logH<-(log10(CQ$H_mgL))
CQ$logEC<-(log10(CQ$EC))

#Function to pull regression text out
#lm_eqn<-function(df){
 # m<-lm(y~x,df)
  #eq<-substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
           #      list(a = format(coef(m)[1], digits = 2),
            #          b = format(coef(m)[2], digits = 2),
            #          r2 = format(summary(m)$r.squared,digits=3)))
  #as.character(as.expression(eq))
#}
#making plots

#Ca plot
library(ggplot2)
pal="#c2a5cf"
shape1=21
#df<-CQ[,c("logQ","logCa")]
#names(df)<-c("x","y")
CQCa<-ggplot(CQ, aes(logQ,logCa))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"))+
  xlab("\nLog(Q,"~m^3~" "~s^-1~")")+
  ylab("Log(Ca)\n")+
  ylim(-2,1.5)+
#  geom_text(x=-3.5, y=0.5, label = lm_eqn(df),parse=TRUE)+
  geom_smooth(method=lm,colour="black",se=FALSE)
#Call the graph
CQCa

#save to pdf
pdf("CQCa.pdf",height=6,width=8)
CQCa
dev.off()

#Al plot
library(ggplot2)
pal="#c2a5cf"
shape1=21
#df<-CQ[,c("logQ","logCa")]
#names(df)<-c("x","y")
CQAl<-ggplot(CQ, aes(logQ,logAl))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"))+
  xlab("\nLog(Q,"~m^3~" "~s^-1~")")+
  ylab("Log(Al)\n")+
  ylim(-2,1.5)+
  #  geom_text(x=-3.5, y=0.5, label = lm_eqn(df),parse=TRUE)+
  geom_smooth(method=lm,colour="black",se=FALSE)
#Call the graph
CQAl

#save to pdf
pdf("CQAl.pdf",height=6,width=8)
CQAl
dev.off()

#Fe plot
library(ggplot2)
pal="#c2a5cf"
shape1=21
#df<-CQ[,c("logQ","logCa")]
#names(df)<-c("x","y")
CQFe<-ggplot(CQ, aes(logQ,logFe))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"))+
  xlab("\nLog(Q,"~m^3~" "~s^-1~")")+
  ylab("Log(Fe)\n")+
  ylim(-2,1.5)+
  #  geom_text(x=-3.5, y=0.5, label = lm_eqn(df),parse=TRUE)+
  geom_smooth(method=lm,colour="black",se=FALSE)
#Call the graph
CQFe

#save to pdf
pdf("CQFe.pdf",height=6,width=8)
CQFe
dev.off()

#K plot
library(ggplot2)
pal="#c2a5cf"
shape1=21
#df<-CQ[,c("logQ","logCa")]
#names(df)<-c("x","y")
CQK<-ggplot(CQ, aes(logQ,logK))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"))+
  xlab("\nLog(Q,"~m^3~" "~s^-1~")")+
  ylab("Log(K)\n")+
  ylim(-2,1.5)+
  #  geom_text(x=-3.5, y=0.5, label = lm_eqn(df),parse=TRUE)+
  geom_smooth(method=lm,colour="black",se=FALSE)
#Call the graph
CQK

#save to pdf
pdf("CQK.pdf",height=6,width=8)
CQK
dev.off()

#Mg plot
library(ggplot2)
pal="#c2a5cf"
shape1=21
#df<-CQ[,c("logQ","logCa")]
#names(df)<-c("x","y")
CQMg<-ggplot(CQ, aes(logQ,logMg))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"))+
  xlab("\nLog(Q,"~m^3~" "~s^-1~")")+
  ylab("Log(Mg)\n")+
  ylim(-2,1.5)+
  #  geom_text(x=-3.5, y=0.5, label = lm_eqn(df),parse=TRUE)+
  geom_smooth(method=lm,colour="black",se=FALSE)
#Call the graph
CQMg

#save to pdf
pdf("CQMg.pdf",height=6,width=8)
CQMg
dev.off()

#Mn plot
library(ggplot2)
pal="#c2a5cf"
shape1=21
#df<-CQ[,c("logQ","logCa")]
#names(df)<-c("x","y")
CQMn<-ggplot(CQ, aes(logQ,logMn))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"))+
  xlab("\nLog(Q,"~m^3~" "~s^-1~")")+
  ylab("Log(Mn)\n")+
  ylim(-2,1.5)+
  #  geom_text(x=-3.5, y=0.5, label = lm_eqn(df),parse=TRUE)+
  geom_smooth(method=lm,colour="black",se=FALSE)
#Call the graph
CQMn

#save to pdf
pdf("CQMn.pdf",height=6,width=8)
CQMn
dev.off()

#Na plot
library(ggplot2)
pal="#c2a5cf"
shape1=21
#df<-CQ[,c("logQ","logCa")]
#names(df)<-c("x","y")
CQNa<-ggplot(CQ, aes(logQ,logNa))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"))+
  xlab("\nLog(Q,"~m^3~" "~s^-1~")")+
  ylab("Log(Na)\n")+
  ylim(-2,1.5)+
  #  geom_text(x=-3.5, y=0.5, label = lm_eqn(df),parse=TRUE)+
  geom_smooth(method=lm,colour="black",se=FALSE)
#Call the graph
CQNa

#save to pdf
pdf("CQNa.pdf",height=6,width=8)
CQNa
dev.off()

#SO42- plot
library(ggplot2)
pal="#c2a5cf"
shape1=21
#df<-CQ[,c("logQ","logCa")]
#names(df)<-c("x","y")
CQSO42<-ggplot(CQ, aes(logQ,logSO42))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"))+
  xlab("\nLog(Q,"~m^3~" "~s^-1~")")+
  ylab("Log("~SO[4]^2~"-, mmol"~L^-1~")")+
  #  geom_text(x=-3.5, y=0.5, label = lm_eqn(df),parse=TRUE)+
  geom_smooth(method=lm,colour="black",se=FALSE)+
  ylim(-1.5,2)
#Call the graph
CQSO42

#save to pdf
pdf("CQSO42.pdf",height=6,width=8)
CQSO42
dev.off()

#H+ plot
library(ggplot2)
pal="#c2a5cf"
shape1=21
#df<-CQ[,c("logQ","logCa")]
#names(df)<-c("x","y")
CQH<-ggplot(CQ, aes(logQ,logH))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"))+
  xlab("\nLog(Q,"~m^3~" "~s^-1~")")+
  ylab("Log("~H^+~", mmol"~L^-1~")\n")+
  #  geom_text(x=-3.5, y=0.5, label = lm_eqn(df),parse=TRUE)+
  geom_smooth(method=lm,colour="black",se=FALSE)
#Call the graph
CQH

#save to pdf
pdf("CQH.pdf",height=6,width=8)
CQH
dev.off()

#EC plot
library(ggplot2)
pal="#c2a5cf"
shape1=21
#df<-CQ[,c("logQ","logCa")]
#names(df)<-c("x","y")
CQEC<-ggplot(CQ, aes(logQ,logEC))+
  geom_point(colour="black",size=4, fill=pal, pch=shape1)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Depth (cm)"))+
  xlab("\nLog(Q,"~m^3~" "~s^-1~")")+
  ylab("Log(Specific conductance, "~mu~"S "~cm^-1~")\n")+
  #  geom_text(x=-3.5, y=0.5, label = lm_eqn(df),parse=TRUE)+
  geom_smooth(method=lm,colour="black",se=FALSE)
#Call the graph
CQEC

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
#pal="#c2a5cf"
#shape1=21
#df<-CQ[,c("logQ","logNa")]
#names(df)<-c("x","y")
pal<-c("#ffffcc","#c2e699","#78c679","#238443")
CQbasecat<-ggplot(longbaseCQ, aes(x=logQ,y=concentration,fill=ion))+
  geom_point(colour="black",size=4,pch=21)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(name="Ion",breaks=c("logCa","logK","logMg","logNa"),labels=c("Ca","K","Mg","Na"),values=pal)+
  xlab("\nLog(Q, "~m^3~" "~s^-1~")")
 # ylab("Log(Concentration, mmol "~L^-1~")")
#  geom_text(x=-3.5, y=-0.8, label = lm_eqn(df),parse=TRUE)+
 # geom_smooth(method=lm,se=FALSE)
#Call the graph
CQbasecat

#Plot amd derived CQ (Fe, Mn, Al, SO42-)
#All CQ ions plot
#Add in lines and why is the legend not working?
library(ggplot2)
#pal="#c2a5cf"
#shape1=21
#df<-CQ[,c("logQ","logNa")]
#names(df)<-c("x","y")
CQAMD<-ggplot(CQ, aes(x=logQ))+
  geom_point(aes(y=logFe,fill="blue"),pch=21,colour="black",size=2)+
  geom_point(aes(y=logMn,fill="purple"),pch=21,colour="black",size=2)+
  geom_point(aes(y=logAl,fill="red"),pch=21,colour="black",size=2)+
  geom_point(aes(y=logSO42,fill="orange"),pch=21,colour="black",size=2)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Ion"),values=c("blue","purple","red","orange"),
         labels=c("Fe","Mn","Al","SO42-"))+
  xlab("\nLog(Q, "~m^3~" "~s^-1~")")+
  ylab("Log(Concentration, mmol "~L^-1~")\n")
#  geom_text(x=-3.5, y=-0.8, label = lm_eqn(df),parse=TRUE)+
# geom_smooth(method=lm,se=FALSE)
#Call the graph
CQAMD