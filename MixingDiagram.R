#Bring in mixing data
basecatmix<-read.csv("Mixing.csv",header=TRUE)

#Rename headings to be R friendly
newname<-c("sample_name","Site","date","Depth_cm","Al_mgL","Ca_mgL","Fe_mgL","K_mgL","Mg_mgL","Mn_mgL","Na_mgL")
colnames(basecatmix)<-newname

#convert date
library(lubridate)
basecatmix$newdate<-mdy(basecatmix$date)
basecatmix$doy<-yday(basecatmix$newdate)

#Transform mg/L to mmol/L

basecatmix$Al_mmolL<-(basecatmix$Al_mgL/26.982)
basecatmix$Ca_mmolL<-(basecatmix$Ca_mgL/40.08)
basecatmix$Fe_mmolL<-(basecatmix$Fe_mgL/55.845)
basecatmix$K_mmolL<-(basecatmix$K_mgL/39.099)
basecatmix$Mg_mmolL<-(basecatmix$Mg_mgL/24.305)
basecatmix$Mn_mmolL<-(basecatmix$Mn_mgL/54.938)
basecatmix$Na_mmolL<-(basecatmix$Na_mgL/22.99)

#Divide Ca by Mg and Na by K
basecatmix$Ca_Mg<-(basecatmix$Ca_mmolL/basecatmix$Mg_mmolL)
basecatmix$Na_K<-(basecatmix$Na_mmolL/basecatmix$K_mmolL)

#Ca/Mg std dev and std err
library(plyr)
CaMgstderr.site.summary<-ddply(basecatmix, c("sample_name","Depth_cm","Site"), summarise,
                            CaMgmean = mean(Ca_Mg,na.rm=TRUE), CaMgsd = sd(Ca_Mg,na.rm=TRUE),
                            CaMgsem = sd(Ca_Mg,na.rm=TRUE)/sqrt(length(Ca_Mg)))

#Fe/Na std dev and std err
library(plyr)
FeNastderr.site.summary<-ddply(basecatmix, c("sample_name","Depth_cm","Site"), summarise,
                               FeNamean = mean(Fe_Na,na.rm=TRUE), FeNasd = sd(Fe_Na,na.rm=TRUE),
                               FeNasem = sd(Fe_Na,na.rm=TRUE)/sqrt(length(Fe_Na)))

#Mn/Na std dev and std err
library(plyr)
MnNastderr.site.summary<-ddply(basecatmix, c("sample_name","Depth_cm","Site"), summarise,
                               MnNamean = mean(Mn_Na,na.rm=TRUE), MnNasd = sd(Mn_Na,na.rm=TRUE),
                               MnNasem = sd(Mn_Na,na.rm=TRUE)/sqrt(length(Mn_Na)))
                    

#Plots base cation mixing diagram
library(ggplot2)
pal<-c("#ca0020","#fe9929","#969696","#225ea8")
shape1<-c(21, 22, 23, 24)
Basecatmixing<-ggplot(basecatmix, aes(basecatmix$Na_K,basecatmix$Ca_Mg,fill=as.factor(Site),shape=as.factor(Site)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nNa/K")+
  ylab("Ca/Mg")+
  theme(legend.position="bottom")
#Call the graph
Basecatmixing

#save to pdf
pdf("Basecatmixing.pdf",height=6,width=8)
Basecatmixing
dev.off()

#Divide Fe and Mn by Na
basecatmix$Fe_Na<-(basecatmix$Fe_mmolL/basecatmix$Na_mmolL)
basecatmix$Mn_Na<-(basecatmix$Mn_mmolL/basecatmix$Na_mmolL)
#take log of that
basecatmix$logFeNa<-(log10(basecatmix$Fe_Na))
basecatmix$logMnNa<-(log10(basecatmix$Mn_Na))

#Plots Fe and Mn mixing diagram
library(ggplot2)
pal<-c("#ca0020","#fe9929","#969696","#225ea8")
shape1<-c(21, 22, 23, 24)
Metalsmixing<-ggplot(basecatmix, aes(basecatmix$logMnNa,basecatmix$logFeNa,fill=as.factor(Site),shape=as.factor(Site)))+
  geom_point(colour="black",size=4)+
  scale_shape_manual(values=shape1)+    
  scale_fill_manual(values=pal)+
  theme_bw(base_size=20)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("Log(Mn/Na)")+
  ylab("Log(Fe/Na)")+
  theme(legend.position="bottom")
#Call the graph
Metalsmixing

#save to pdf
pdf("Metalsmixing.pdf",height=6,width=8)
Metalsmixing
dev.off()

#Access LC, BDP, and outlet subset of cation data and make mix by date file
Mixbydate<-basecatmix[which(basecatmix$sample_name == "OUT"),]

#convert date
library(lubridate)
Mixbydate$newdate<-mdy(Mixbydate$date)
Mixbydate$doy<-yday(Mixbydate$newdate)

#Plots Ca/Mg by date
library(ggplot2)
CaMgbydate<-ggplot(Mixbydate, aes(newdate,Mixbydate$Ca_Mg))+
   annotate("rect",fill="blue",alpha=0.5,xmin=as.Date("2017-03-20"),xmax=as.Date("2017-12-01"),ymin=1.62685682,ymax=1.72463918)+
  annotate("rect",fill="red",alpha=1,xmin=as.Date("2017-03-20"),xmax=as.Date("2017-12-01"),ymin=1.20536467,ymax=1.23625933)+
  theme_bw(base_size=20)+
  geom_point(colour="black",size=4,pch=21,fill="purple")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nDate")+
  ylab("Ca/Mg")+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  ylim(1,2)+
  theme(legend.position="bottom")
#Call the graph
CaMgbydate

#save to pdf
pdf("CaMgbydate.pdf",height=8,width=8)
CaMgbydate
dev.off()

#Plots Fe/Na by date
library(ggplot2)
FeNabydate<-ggplot(Mixbydate, aes(newdate,Mixbydate$Fe_Na))+
  annotate("rect",fill="blue",alpha=1,xmin=as.Date("2017-03-20"),xmax=as.Date("2017-12-01"),ymin=0.01638548,ymax=0.02895888)+
  annotate("rect",fill="red",alpha=1,xmin=as.Date("2017-03-20"),xmax=as.Date("2017-12-01"),ymin=0.92076095,ymax=3.16853973)+
  theme_bw(base_size=20)+
  geom_point(colour="black",size=4,pch=21,fill="purple")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nDate")+
  ylab("Fe/Na")+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  ylim(0,4)+
  theme(legend.position="bottom")
#Call the graph
FeNabydate

#save to pdf
pdf("FeNabydate.pdf",height=6,width=8)
FeNabydate
dev.off()

#Plots Mn/Na by date
#Need to update ymin/ymax for LC and BDP
library(ggplot2)
MnNabydate<-ggplot(Mixbydate, aes(newdate,Mixbydate$Mn_Na))+
  annotate("rect",fill="blue",alpha=1,xmin=as.Date("2017-03-20"),xmax=as.Date("2017-12-01"),ymin=0.01638548,ymax=0.02895888)+
  annotate("rect",fill="red",alpha=1,xmin=as.Date("2017-03-20"),xmax=as.Date("2017-12-01"),ymin=0.92076095,ymax=3.16853973)+
  theme_bw(base_size=20)+
  geom_point(colour="black",size=4,pch=21,fill="purple")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill=guide_legend(title="Site"),shape=guide_legend(title="Site"))+
  xlab("\nDate")+
  ylab("Fe/Na")+
  scale_x_date(breaks=c(as.Date("2017-04-01"),as.Date("2017-06-01"),as.Date("2017-08-01"),as.Date("2017-10-01"),as.Date("2017-12-01")))+
  ylim(0,4)+
  theme(legend.position="bottom")
#Call the graph
MnNabydate

#save to pdf
pdf("MnNabydate.pdf",height=6,width=8)
MnNabydate
dev.off()
